#!/usr/bin/env python3
# eGeMAPS extraction from pre-trimmed 16 kHz mono WAV files using openSMILE.
# Uses the same trimmed WAV inputs as the wav2vec2 speech-embedding script.
# Filename format: YYYY-MM-DD_HH-MM-SS.wav

import argparse
import glob
import os
import warnings
from datetime import datetime, timezone

import pandas as pd
from opensmile import FeatureLevel, FeatureSet, Smile

warnings.filterwarnings("ignore", category=UserWarning)


def ts_from_filename(fname: str) -> int:
    """Extract UTC timestamp from filename like 2018-10-30_17-02-12.wav."""
    stem = os.path.splitext(os.path.basename(fname))[0]
    dt = datetime.strptime(stem, "%Y-%m-%d_%H-%M-%S").replace(tzinfo=timezone.utc)
    return int(dt.timestamp())


def flatten_columns(feats: pd.DataFrame) -> pd.DataFrame:
    if isinstance(feats.columns, pd.MultiIndex):
        feats = feats.copy()
        feats.columns = [
            "__".join([str(x) for x in tup if str(x) != ""])
            for tup in feats.columns.values
        ]
    return feats


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument(
        "--raw-dir",
        default="data/audio_wav_trimmed",
        help="Directory containing pre-trimmed WAVs, either root/*/*.wav or one participant folder.",
    )
    ap.add_argument("--out-dir", default="data/egemaps_features")
    ap.add_argument("--final-output", default="data/egemaps_all.csv")
    ap.add_argument("--log-every", type=int, default=50)
    ap.add_argument("--max-files", type=int, default=0)
    args = ap.parse_args()

    if not os.path.isdir(args.raw_dir):
        raise SystemExit(f"raw-dir is not a directory: {args.raw_dir}")

    os.makedirs(args.out_dir, exist_ok=True)

    # Discover WAVs. Support root/*/*.wav and a single participant folder/*.wav.
    wavs = sorted(glob.glob(os.path.join(args.raw_dir, "*.wav")))
    if not wavs:
        wavs = sorted(glob.glob(os.path.join(args.raw_dir, "*", "*.wav")))
    if not wavs:
        raise SystemExit("No WAV files found in raw-dir.")

    df = pd.DataFrame(
        {
            "file_path": wavs,
            "participant_id": [os.path.basename(os.path.dirname(p)) for p in wavs],
            "file": [os.path.basename(p) for p in wavs],
        }
    )
    df["timestamp_s"] = df["file"].apply(ts_from_filename)

    if args.max_files > 0:
        df = df.head(args.max_files)

    smile = Smile(feature_set=FeatureSet.eGeMAPSv02, feature_level=FeatureLevel.Functionals)

    print(f"Files to process: {len(df)}")
    print(f"Reading trimmed WAVs from: {args.raw_dir}")
    print(f"Writing per-user to: {args.out_dir}")
    print(f"Writing combined to: {args.final_output}")

    if os.path.exists(args.final_output):
        os.remove(args.final_output)
    for old_csv in glob.glob(os.path.join(args.out_dir, "*.csv")):
        os.remove(old_csv)

    for i, row in df.iterrows():
        user = row["participant_id"]
        fpath = row["file_path"]
        fname = row["file"]
        ts = int(row["timestamp_s"])

        rec = {
            "participant_id": user,
            "file": fname,
            "file_path": fpath,
            "timestamp_s": ts,
        }

        try:
            feats = flatten_columns(smile.process_file(fpath))
            feats = feats.iloc[0].to_dict()
            for k, v in feats.items():
                rec[f"egemaps__{k}"] = v
        except Exception as e:
            rec["egemaps__error"] = str(e)

        out_row = pd.DataFrame([rec])
        user_csv = os.path.join(args.out_dir, f"{user}.csv")
        out_row.to_csv(user_csv, mode="a", header=not os.path.exists(user_csv), index=False)
        out_row.to_csv(args.final_output, mode="a", header=not os.path.exists(args.final_output), index=False)

        if (i + 1) % args.log_every == 0:
            print(f"... processed {i + 1}/{len(df)}", flush=True)

    print("Done.", flush=True)


if __name__ == "__main__":
    main()
