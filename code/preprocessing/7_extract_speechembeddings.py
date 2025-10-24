#!/usr/bin/env python3
# extract_speechembeddings.py
# Python-only pipeline using rpy2 to read/write RDS, trims to speech-only,
# no batching (one forward per clip), writes per-user RDS + final combined RDS.

import os
import sys
import glob
import argparse
import numpy as np
import pandas as pd

from scipy.signal import resample_poly
import torch
from transformers import Wav2Vec2FeatureExtractor, Wav2Vec2Model

# --- rpy2 (R interop) ---------------------------------------------------
import rpy2.robjects as ro
from rpy2.robjects.packages import importr
from rpy2.robjects.conversion import localconverter
from rpy2.robjects import default_converter
from rpy2.robjects import pandas2ri, numpy2ri

base = importr('base')

# convenience
def read_rds(path): 
    return base.readRDS(path)

def save_rds(obj, path): 
    base.saveRDS(obj, file=path)

# pandas <-> R helpers
def r_to_pandas(r_df):
    with localconverter(pandas2ri.converter):
        return ro.conversion.rpy2py(r_df)

def pandas_to_r(df):
    with localconverter(pandas2ri.converter):
        return ro.conversion.py2rpy(df)

# -----------------------------------------------------------------------

def ensure_dir(d):
    os.makedirs(d, exist_ok=True)

def resolve_model_dir(root):
    root = os.path.abspath(root)
    if os.path.isfile(os.path.join(root, "config.json")):
        return root
    snaps = sorted(glob.glob(os.path.join(root, "snapshots", "*")))
    for d in snaps:
        if d.endswith("6eba34a2485ea31cb03600241787c3a5edab8626"):
            return d
    raise RuntimeError("Target snapshot not found under: " + root)


def to_mono(sig_np):
    if sig_np.ndim == 2:
        sig_np = sig_np[:, 0]  # first channel to match old code
    sig_np = sig_np.astype(np.float32, copy=False)
    sig_np[~np.isfinite(sig_np)] = 0.0
    return sig_np

def ensure_16k(sig, fs):
    if fs == 16000:
        return sig.astype(np.float32, copy=False)
    return resample_poly(sig, 16000, fs).astype(np.float32, copy=False)

# --- robust bundle loader ------------------------------------------------

def load_user_records(rds_path):
    """
    Return a Python list of R clip-records.
    The bundle is a named R list; each child is one clip (has $audio_wave, ...).
    """
    obj = read_rds(rds_path)

    try:
        n_top = int(base.length(obj)[0])
    except Exception:
        n_top = 0

    recs = []
    for i in range(n_top):
        try:
            child = obj.rx2(i + 1)              # 1-based
            _ = child.rx2('audio_wave')         # must have audio_wave to be a clip
            recs.append(child)
        except Exception:
            continue

    if not recs:
        # Fallback: try top-level as single record
        try:
            _ = obj.rx2('audio_wave')
            recs = [obj]
        except Exception:
            recs = []

    return recs

# --- robust speech-only trimming for hetero speech.to.text ---------------

def trim_to_speech(rec, pad_samples=0):
    """
    Match old R behavior:
      - Use speech.to.text if it contains segments with start_time/end_time.
      - Require trimmed span ≥ 1.0 s at native fs, else keep FULL audio.
      - No padding by default (pad_samples=0).
    """
    aw = rec.rx2('audio_wave')
    sig = aw.rx2('sig')
    fs  = int(aw.rx2('fs')[0])

    with localconverter(default_converter + numpy2ri.converter):
        sig_np = np.array(sig)  # (n,) or (n, c)
    sig_np = to_mono(sig_np)
    n = sig_np.shape[0]

    # Try to read segments
    try:
        parts = rec.rx2('speech.to.text')
        is_list = bool(base.is_list(parts)[0])
    except Exception:
        parts, is_list = None, False

    starts, ends = [], []
    if is_list:
        m = int(base.length(parts)[0])
        r_names   = ro.r('function(x) names(x)')
        r_is_list = ro.r('function(x) is.list(x)')
        for i in range(m):
            try:
                seg = parts.rx2(i + 1)
                if not bool(r_is_list(seg)[0]): 
                    continue
                nms = list(r_names(seg))
                if ('start_time' not in nms) or ('end_time' not in nms):
                    continue
                st = float(seg.rx2('start_time')[0])
                et = float(seg.rx2('end_time')[0])
                if np.isfinite(st) and np.isfinite(et) and et > st:
                    starts.append(st); ends.append(et)
            except Exception:
                continue

    if starts and ends:
        st = min(starts); et = max(ends)
        i1 = max(0, int(np.floor(st * fs)) - pad_samples)
        i2 = min(n, int(np.ceil(et * fs))  + pad_samples)
        # Require ≥ 1.0 s. If not, fall back to full audio.
        if (i2 > i1) and ((i2 - i1) >= fs):
            return sig_np[i1:i2], fs

    # Fallback: full audio (exactly like old R when span < 1s or no timings)
    return sig_np, fs

# --- embedding -----------------------------------------------------------

def extract_one_embedding(sig16, fe, model, device):
    """One forward per clip; guard against empty signals to avoid NaNs."""
    if sig16 is None or len(sig16) == 0:
        raise ValueError("empty signal after trim/resample")
    with torch.no_grad():
        inputs = fe([sig16], sampling_rate=16000, return_tensors="pt")
        inputs = {k: v.to(device) for k, v in inputs.items()}
        out = model(input_values=inputs["input_values"]).last_hidden_state  # [1, T, 1024]
        vec = out.mean(dim=1).squeeze(0).cpu().numpy().astype(np.float32, copy=False)
    return vec

# --- build the mapping table exactly like your R ------------------------

def build_audio_map(map_rds_path, raw_dir):
    """
    Replicates the R pipeline:
      readRDS(...) %>%
        separate(wav_path, into=c("file","clip_idx"), sep=" :: index ", convert=TRUE) %>%
        mutate(file = file.path(raw_dir, file), clip_idx = as.integer(clip_idx)) %>%
        filter(file.exists(file))
    Returns a pandas.DataFrame with columns including: audio_id, file, clip_idx
    """
    ro.r('library(dplyr); library(tidyr)')
    ro.globalenv['map_path'] = map_rds_path
    ro.globalenv['raw_dir']  = raw_dir
    ro.r('''
audio_map <- readRDS(map_path) %>%
  tidyr::separate(wav_path, into=c("file","clip_idx"),
                  sep=" :: index ", remove=FALSE, convert=TRUE) %>%
  dplyr::mutate(file = file.path(raw_dir, file),
                clip_idx = as.integer(clip_idx)) %>%
  dplyr::filter(file.exists(file))
''')
    return r_to_pandas(ro.r('audio_map'))

# --- per-user processing ------------------------------------------------

def process_user_file(rds_path, audio_map_df, out_dir, fe, model, device):
    """
    Process one per-user RDS bundle, trimming to speech, no batching.
    Writes <out_dir>/<user>.rds (one row per audio).
    """
    user_key = os.path.splitext(os.path.basename(rds_path))[0]
    out_path = os.path.join(out_dir, f"{user_key}.rds")
    if os.path.exists(out_path):
        return out_path

    recs = load_user_records(rds_path)

    # Normalize path match to avoid relative-vs-absolute issues
    rds_path_norm = os.path.abspath(rds_path)
    files_norm = audio_map_df["file"].apply(os.path.abspath)
    sub = audio_map_df.loc[files_norm == rds_path_norm].copy()

    # DEBUG: how many clips we think we have
    print(f"[{os.path.basename(rds_path)}] mapped={len(sub)} bundle={len(recs)}", flush=True)

    if sub.empty or not recs:
        save_rds(pandas_to_r(pd.DataFrame()), out_path)
        return out_path

    rows = []
    for _, row in sub.iterrows():
        idx = int(row["clip_idx"])
        if idx < 1 or idx > len(recs):
            continue
        rec = recs[idx - 1]  # 0-based

        try:
            sig_trim, fs = trim_to_speech(rec, pad_samples=0)
            sig16 = ensure_16k(sig_trim, fs)
            emb   = extract_one_embedding(sig16, fe, model, device)
        except Exception as e:
            # Keep alignment; NaNs indicate failure for this clip
            emb = np.full(1024, np.nan, dtype=np.float32)

        row_dict = {
            "audio_id": row["audio_id"],
            "src_file": os.path.basename(rds_path),
            "rec_index": idx
        }
        row_dict.update({f"wav2vec2_{i+1}": float(v) for i, v in enumerate(emb)})
        rows.append(row_dict)

    df_user = pd.DataFrame(rows)
    save_rds(pandas_to_r(df_user), out_path)
    return out_path

# --- combine all per-user RDS ------------------------------------------

def combine_user_rds(out_dir, final_output):
    """
    Read all per-user .rds, row-bind, and write a single combined .rds
    """
    rbind_list = []
    for p in sorted(glob.glob(os.path.join(out_dir, "*.rds"))):
        try:
            r_df = read_rds(p)
            rbind_list.append(r_df)
        except Exception:
            pass
    if not rbind_list:
        # write empty
        save_rds(ro.r('tibble::tibble()'), final_output)
        return

    ro.r('library(dplyr)')
    r_big = ro.r['bind_rows'](*rbind_list)
    save_rds(r_big, final_output)

# --- main ----------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--model-root", required=True,
                    help="HF local snapshot dir (contains config.json or snapshots/...)")
    ap.add_argument("--audio-map-rds", required=True,
                    help="Path to audio_ema_matched_cleaned.rds")
    ap.add_argument("--raw-dir", required=True,
                    help="Prefix to prepend to relative file paths in wav_path")
    ap.add_argument("--out-dir", default="data/speech_embeddings",
                    help="Per-user RDS output folder")
    ap.add_argument("--final-output", default="data/speech_embeddings_all.rds",
                    help="Combined RDS output path")
    ap.add_argument("--device", default="cpu", choices=["cpu","cuda"],
                    help="Use cuda if available")
    args = ap.parse_args()

    ensure_dir(args.out_dir)

    model_dir = resolve_model_dir(args.model_root)
    device = torch.device(args.device if (args.device == "cuda" and torch.cuda.is_available()) else "cpu")

    # Load model + feature extractor
    fe  = Wav2Vec2FeatureExtractor.from_pretrained(model_dir, local_files_only=True)
    mdl = Wav2Vec2Model.from_pretrained(model_dir, local_files_only=True).to(device)
    mdl.eval()

    # Build mapping (audio_id, file, clip_idx), keep only existing files
    audio_map_df = build_audio_map(args.audio_map_rds, args.raw_dir)

    # Unique user RDS files
    files_unique = sorted(audio_map_df['file'].unique())

    for i, src in enumerate(files_unique, 1):
        process_user_file(src, audio_map_df, args.out_dir, fe, mdl, device)
        if i % 25 == 0:
            print(f"... processed {i}/{len(files_unique)} users", flush=True)

    # Final combine
    combine_user_rds(args.out_dir, args.final_output)
    print("Done.")

if __name__ == "__main__":
    main()
