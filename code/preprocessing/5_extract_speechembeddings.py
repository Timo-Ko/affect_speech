#!/usr/bin/env python3
# Extract speech embeddings from pre-trimmed 16kHz mono WAVs using wav2vec2.

import os, glob, argparse, wave
import numpy as np
import pandas as pd
import torch
from transformers import AutoProcessor, AutoModel
from datetime import datetime, timezone

SR = 16000  # Hz

def ts_from_filename(fname: str) -> int:
    stem = os.path.splitext(os.path.basename(fname))[0]
    dt = datetime.strptime(stem, "%Y-%m-%d_%H-%M-%S").replace(tzinfo=timezone.utc)
    return int(dt.timestamp())

def read_wav_mono16k(path: str) -> np.ndarray:
    """Read 16-bit PCM mono WAV at 16 kHz and return float32 in [-1, 1]."""
    with wave.open(path, "rb") as wf:
        n_channels = wf.getnchannels()
        fr = wf.getframerate()
        sw = wf.getsampwidth()
        n_frames = wf.getnframes()
        if n_channels != 1 or fr != SR or sw != 2:
            raise ValueError(
                f"expected 16k mono s16le, got ch={n_channels}, sr={fr}, sw={sw} for {path}"
            )
        buf = wf.readframes(n_frames)
    return np.frombuffer(buf, dtype=np.int16).astype(np.float32) / 32768.0

def mean_embed(sig16: np.ndarray, processor, model, device, max_seconds: int) -> np.ndarray:
    if sig16.size == 0:
        raise ValueError("empty signal")
    max_len = SR * max_seconds if max_seconds and max_seconds > 0 else None
    if max_len and sig16.size > max_len:
        chunks = [sig16[i:i + max_len] for i in range(0, sig16.size, max_len)]
    else:
        chunks = [sig16]

    embs = []
    with torch.no_grad():
        for arr in chunks:
            inputs = processor(arr, sampling_rate=SR, return_tensors="pt")
            inputs = {k: v.to(device) for k, v in inputs.items()}
            out = model(**inputs).last_hidden_state  # [1, T, D]
            embs.append(out.mean(dim=1).squeeze(0).cpu())
    return torch.stack(embs, dim=0).mean(dim=0).numpy().astype(np.float32, copy=False)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--raw-dir", required=True,
                    help="Either data/audio_wav_trimmed (root) or a single user folder like data/audio_wav_trimmed/<participant_id>")
    ap.add_argument("--out-dir", default="data/speech_embeddings", help="per-user CSV dir")
    ap.add_argument("--model", default="audeering/wav2vec2-large-robust-12-ft-emotion-msp-dim")
    ap.add_argument("--device", default="cpu", choices=["cpu", "cuda"])
    ap.add_argument("--max-seconds", type=int, default=60)
    ap.add_argument("--log-every", type=int, default=200)
    ap.add_argument("--max-files", type=int, default=0)
    args = ap.parse_args()

    os.makedirs(args.out_dir, exist_ok=True)

    # Discover WAVs after args exist. Support root/*/*.wav and user/*.wav.
    if not os.path.isdir(args.raw_dir):
        raise SystemExit(f"raw-dir is not a directory: {args.raw_dir}")

    wavs = sorted(glob.glob(os.path.join(args.raw_dir, "*.wav")))
    if not wavs:
        wavs = sorted(glob.glob(os.path.join(args.raw_dir, "*", "*.wav")))
    if not wavs:
        raise SystemExit("No WAV files found.")

    df = pd.DataFrame({
        "file_path": wavs,
        "participant_id": [os.path.basename(os.path.dirname(p)) for p in wavs],
        "file": [os.path.basename(p) for p in wavs],
    })
    df["timestamp_s"] = df["file"].apply(ts_from_filename)
    if args.max_files > 0:
        df = df.head(args.max_files)

    device = torch.device(args.device if (args.device == "cuda" and torch.cuda.is_available()) else "cpu")
    processor = AutoProcessor.from_pretrained(args.model, local_files_only=True)
    model = AutoModel.from_pretrained(args.model, local_files_only=True).to(device)
    model.eval()

    print(f"model: {args.model} on {device}")
    print(f"files: {len(df)}")
    print(f"out dir: {args.out_dir}")

    for i, row in df.iterrows():
        user, fpath, fname = row["participant_id"], row["file_path"], row["file"]
        ts = int(row["timestamp_s"])

        rec = {
            "participant_id": user,
            "file": fname,
            "file_path": fpath,
            "timestamp_s": ts,
        }

        try:
            sig = read_wav_mono16k(fpath)
            emb = mean_embed(sig, processor, model, device, args.max_seconds)
            rec.update({f"wav2vec2_{j}": float(v) for j, v in enumerate(emb, 1)})
        except Exception as e:
            rec["wav2vec2_error"] = str(e)

        user_csv = os.path.join(args.out_dir, f"{user}.csv")
        pd.DataFrame([rec]).to_csv(user_csv, mode="a",
                                   header=not os.path.exists(user_csv), index=False)

        if (i + 1) % args.log_every == 0:
            print(f"... processed {i + 1}/{len(df)}", flush=True)

    print("Done.", flush=True)

if __name__ == "__main__":
    main()
