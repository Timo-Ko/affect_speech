#!/usr/bin/env python3
# eGeMAPS extraction from MP4 files using ffmpeg + opensmile.
# Incremental writing of per-user and combined CSVs.
# Filename format: YYYY-MM-DD_HH-MM-SS.mp4

import os
import glob
import math
import argparse
import subprocess
import shutil
import warnings
from datetime import datetime, timezone

import numpy as np
import pandas as pd
from opensmile import Smile, FeatureSet, FeatureLevel

warnings.filterwarnings("ignore", category=UserWarning)

SR = 16000  # sample rate


def ts_from_filename(fname: str) -> int:
    """Extract UTC timestamp from filename like 2018-10-30_17-02-12.mp4."""
    stem = os.path.splitext(os.path.basename(fname))[0]
    dt = datetime.strptime(stem, "%Y-%m-%d_%H-%M-%S").replace(tzinfo=timezone.utc)
    return int(dt.timestamp())


def decode_mp4_ffmpeg(path: str, ffmpeg_bin: str, sr: int = SR, start=None, end=None) -> np.ndarray:
    """Decode mp4 -> float32 mono array using ffmpeg."""
    if shutil.which(ffmpeg_bin) is None:
        raise RuntimeError(f"ffmpeg not found: {ffmpeg_bin}")
    cmd = [ffmpeg_bin, "-nostdin", "-hide_banner", "-loglevel", "error"]
    if start is not None and end is not None and float(end) > float(start):
        cmd += ["-ss", f"{float(start):.3f}", "-to", f"{float(end):.3f}"]
    cmd += ["-i", path, "-ac", "1", "-ar", str(sr), "-f", "f32le", "pipe:1"]
    proc = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if proc.returncode != 0:
        raise RuntimeError(f"ffmpeg decode failed for {path}: {proc.stderr.decode(errors='ignore')}")
    return np.frombuffer(proc.stdout, dtype=np.float32)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--raw-dir", required=True)
    ap.add_argument("--transcripts", required=True)
    ap.add_argument("--out-dir", default="data/egemaps_features")
    ap.add_argument("--final-output", default="data/egemaps_all.csv")
    ap.add_argument("--ffmpeg-bin", default="ffmpeg")
    ap.add_argument("--log-every", type=int, default=50)
    ap.add_argument("--max-files", type=int, default=0)
    args = ap.parse_args()

    os.makedirs(args.out_dir, exist_ok=True)

    # --- Load transcripts and aggregate start/end ---
    tr = pd.read_csv(args.transcripts)
    req = {"participant_id", "timestamp", "start_time_s", "end_time_s"}
    if not req.issubset(tr.columns):
        raise SystemExit(f"Missing columns in transcripts: {req - set(tr.columns)}")

    tr["timestamp_s"] = pd.to_numeric(tr["timestamp"], errors="coerce")
    tr["timestamp_s"] = tr["timestamp_s"].where(tr["timestamp_s"] < 1e12, tr["timestamp_s"] / 1000)
    agg = (
        tr.groupby(["participant_id", "timestamp_s"], as_index=False)
          .agg(start=("start_time_s", "min"), end=("end_time_s", "max"))
    )

    # --- Discover .mp4 files ---
    mp4s = sorted(glob.glob(os.path.join(args.raw_dir, "*", "*.mp4")))
    if not mp4s:
        raise SystemExit("No MP4 files found in raw-dir.")

    df_files = pd.DataFrame({
        "file_path": mp4s,
        "participant_id": [os.path.basename(os.path.dirname(p)) for p in mp4s],
        "file": [os.path.basename(p) for p in mp4s],
    })
    df_files["timestamp_s"] = df_files["file"].apply(lambda f: ts_from_filename(f))
    df = df_files.merge(agg, on=["participant_id", "timestamp_s"], how="left")

    # --- Optional limit ---
    if args.max_files > 0:
        df = df.head(args.max_files)

    # --- Init openSMILE ---
    smile = Smile(feature_set=FeatureSet.eGeMAPSv02, feature_level=FeatureLevel.Functionals)

    # --- Setup output ---
    ffmpeg_path = shutil.which(args.ffmpeg_bin)
    print(f"ffmpeg: {ffmpeg_path or 'NOT FOUND'}")
    print(f"Files to process: {len(df)}")
    print(f"Writing per-user to {args.out_dir}")
    print(f"Writing combined to {args.final_output}")

    if os.path.exists(args.final_output):
        os.remove(args.final_output)

    # --- Process incrementally ---
    for i, row in df.iterrows():
        user = row["participant_id"]
        fpath = row["file_path"]
        fname = row["file"]
        ts = int(row["timestamp_s"])
        start = row.get("start", np.nan)
        end = row.get("end", np.nan)

        rec = {
            "participant_id": user,
            "file": fname,
            "file_path": fpath,
            "timestamp_s": ts,
            "trim_start_s": float(start) if pd.notna(start) else np.nan,
            "trim_end_s": float(end) if pd.notna(end) else np.nan,
        }

        try:
            sig = decode_mp4_ffmpeg(fpath, args.ffmpeg_bin, sr=SR, start=start, end=end)
            feats = smile.process_signal(sig, sampling_rate=SR)
            if isinstance(feats.columns, pd.MultiIndex):
                feats.columns = ["__".join([str(x) for x in tup if str(x) != ""]) for tup in feats.columns.values]
            feats = feats.iloc[0].to_dict()
            for k, v in feats.items():
                rec[f"egemaps__{k}"] = v
        except Exception as e:
            rec["egemaps__error"] = str(e)

        # write immediately
        out_row = pd.DataFrame([rec])
        user_csv = os.path.join(args.out_dir, f"{user}.csv")
        out_row.to_csv(user_csv, mode="a", header=not os.path.exists(user_csv), index=False)
        out_row.to_csv(args.final_output, mode="a", header=not os.path.exists(args.final_output), index=False)

        if (i + 1) % args.log_every == 0:
            print(f"... processed {i + 1}/{len(df)}", flush=True)

    print("Done.", flush=True)


if __name__ == "__main__":
    main()
