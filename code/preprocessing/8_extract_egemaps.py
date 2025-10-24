#!/usr/bin/env python3
# extract_egemaps.py
# eGeMAPS (openSMILE) extraction mirroring the working wav2vec2 pipeline:
# - Build audio map in R (split wav_path into file + clip_idx, prepend raw_dir, filter exists)
# - Per-user bundle -> per mapped clip
# - Trim to speech if available, else full audio
# - Resample to 16 kHz mono
# - openSMILE eGeMAPS Functionals via process_signal (no temp files)
# - Per-user RDS outputs + final combined RDS

import os
import sys
import glob
import argparse
import numpy as np
import pandas as pd
from scipy.signal import resample_poly

# openSMILE
from opensmile import Smile, FeatureSet, FeatureLevel

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

def r_to_pandas(r_df):
    with localconverter(pandas2ri.converter):
        return ro.conversion.rpy2py(r_df)

def pandas_to_r(df):
    with localconverter(pandas2ri.converter):
        return ro.conversion.py2rpy(df)

# -----------------------------------------------------------------------

def ensure_dir(d):
    os.makedirs(d, exist_ok=True)

def to_mono(sig_np):
    if sig_np.ndim == 2:
        sig_np = sig_np.mean(axis=1)
    sig_np = sig_np.astype(np.float32, copy=False)
    sig_np[~np.isfinite(sig_np)] = 0.0
    return sig_np

def ensure_16k(sig, fs):
    if fs == 16000:
        return sig.astype(np.float32, copy=False)
    return resample_poly(sig, 16000, fs).astype(np.float32, copy=False)

# --- robust bundle loader (top-level named list of clips) ----------------

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

def trim_to_speech(rec, pad_samples=200):
    """
    Use rec$speech.to.text if it contains child lists with 'start_time' & 'end_time'.
    Ignore character entries (e.g., 'No speech was detected...').
    If no usable timings, return full audio (no trim).
    """
    aw = rec.rx2('audio_wave')
    sig = aw.rx2('sig')
    fs  = int(aw.rx2('fs')[0])

    with localconverter(default_converter + numpy2ri.converter):
        sig_np = np.array(sig)  # (n,) or (n, c)
    sig_np = to_mono(sig_np)
    n = sig_np.shape[0]

    # Get speech.to.text (may be character or list)
    try:
        parts = rec.rx2('speech.to.text')
    except Exception:
        parts = None

    # Is it a list? If not, skip trimming.
    try:
        is_list = bool(base.is_list(parts)[0]) if parts is not None else False
    except Exception:
        is_list = False

    starts, ends = [], []
    if is_list:
        m = int(base.length(parts)[0])
        r_names   = ro.r('function(x) names(x)')
        r_is_list = ro.r('function(x) is.list(x)')

        for i in range(m):
            seg = parts.rx2(i + 1)
            try:
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
        i2 = min(n, int(np.ceil(et * fs)) + pad_samples)
        if i2 > i1 and (i2 - i1) >= max(10, int(0.01 * fs)):
            return sig_np[i1:i2], fs

    # No valid timings -> full audio
    return sig_np, fs

# --- eGeMAPS extraction --------------------------------------------------

def opensmile_process_signal(smile: Smile, sig16: np.ndarray) -> pd.DataFrame:
    """
    Run openSMILE on an in-memory signal at 16k using process_signal (no temp files).
    Returns a pandas DataFrame (one row).
    """
    # openSMILE expects shape (n,) float32
    sig16 = np.asarray(sig16, dtype=np.float32)
    # Returns a DataFrame with a single row and (often MultiIndex) columns
    df = smile.process_signal(sig16, sampling_rate=16000)
    # Ensure columns are flat strings
    if isinstance(df.columns, pd.MultiIndex):
        df.columns = ["__".join([str(x) for x in tup if str(x) != ""]) for tup in df.columns.values]
    else:
        df.columns = [str(c) for c in df.columns]
    return df

# --- build the mapping table exactly like the wav2vec2 script ------------

def build_audio_map(map_rds_path, raw_dir):
    """
    Replicates the R pipeline:
      readRDS(...) %>%
        separate(wav_path, into=c("file","clip_idx"), sep=" :: index ", convert=TRUE) %>%
        mutate(file = file.path(raw_dir, file), clip_idx = as.integer(clip_idx)) %>%
        filter(file.exists(file))
    Returns a pandas.DataFrame with at least: audio_id, file, clip_idx
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

# --- per-user processing -------------------------------------------------

def process_user_file(rds_path, audio_map_df, out_dir, smile: Smile):
    """
    Process one per-user RDS bundle, trimming to speech, one clip at a time.
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

    # DEBUG (optional): how many clips we think we have
    print(f"[{os.path.basename(rds_path)}] mapped={len(sub)} bundle={len(recs)}", flush=True)

    if sub.empty or not recs:
        save_rds(pandas_to_r(pd.DataFrame()), out_path)
        return out_path

    rows = []
    for _, row in sub.iterrows():
        idx = int(row["clip_idx"])
        if idx < 1 or idx > len(recs):
            continue
        rec = recs[idx - 1]

        try:
            sig_trim, fs = trim_to_speech(rec, pad_samples=200)
            sig16 = ensure_16k(sig_trim, fs)
            feats_df = opensmile_process_signal(smile, sig16)  # one-row DataFrame
            feats = feats_df.iloc[0].to_dict()
        except Exception:
            # Keep alignment; NaNs for all features on failure
            feats = {}

        row_dict = {
            "audio_id": row.get("audio_id", None),
            "src_file": os.path.basename(rds_path),
            "rec_index": idx
        }
        # Prefix feature names to avoid collisions; fill NaNs if empty
        if feats:
            for k, v in feats.items():
                row_dict[f"egemaps__{k}"] = float(v) if np.isfinite(v) else np.nan
        else:
            row_dict["egemaps__error"] = np.nan

        rows.append(row_dict)

    df_user = pd.DataFrame(rows)
    save_rds(pandas_to_r(df_user), out_path)
    return out_path

# --- combine all per-user RDS -------------------------------------------

def combine_user_rds(out_dir, final_output):
    rbind_list = []
    for p in sorted(glob.glob(os.path.join(out_dir, "*.rds"))):
        try:
            r_df = read_rds(p)
            rbind_list.append(r_df)
        except Exception:
            pass
    if not rbind_list:
        save_rds(ro.r('tibble::tibble()'), final_output)
        return
    ro.r('library(dplyr)')
    r_big = ro.r['bind_rows'](*rbind_list)
    save_rds(r_big, final_output)

# --- main ----------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--audio-map-rds", required=True,
                    help="Path to audio_ema_matched_cleaned.rds")
    ap.add_argument("--raw-dir", required=True,
                    help="Prefix to prepend to relative file paths inside wav_path")
    ap.add_argument("--out-dir", default="data/egemaps_features",
                    help="Per-user RDS output folder")
    ap.add_argument("--final-output", default="data/egemaps_all.rds",
                    help="Combined RDS output path")
    args = ap.parse_args()

    ensure_dir(args.out_dir)

    # Initialize openSMILE (eGeMAPS v02 Functionals)
    smile = Smile(
        feature_set=FeatureSet.eGeMAPSv02,
        feature_level=FeatureLevel.Functionals
    )

    # Build mapping (audio_id, file (absolute), clip_idx), keep only existing files
    audio_map_df = build_audio_map(args.audio_map_rds, args.raw_dir)

    # Unique per-user RDS files to process
    files_unique = sorted(audio_map_df['file'].unique())

    for i, src in enumerate(files_unique, 1):
        process_user_file(src, audio_map_df, args.out_dir, smile)
        if i % 25 == 0:
            print(f"... processed {i}/{len(files_unique)} users", flush=True)

    # Final combine
    combine_user_rds(args.out_dir, args.final_output)
    print("Done.")

if __name__ == "__main__":
    main()
