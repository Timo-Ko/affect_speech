#!/usr/bin/env python3
import os, csv, math, argparse, subprocess
from concurrent.futures import ProcessPoolExecutor, as_completed

def parse_args():
    ap = argparse.ArgumentParser()
    ap.add_argument("--transcripts", default="data/transcripts_combined.csv")
    ap.add_argument("--raw-dir",     default="data/audio_files")
    ap.add_argument("--out-dir",     default="data/audio_wav_trimmed")
    ap.add_argument("--ffmpeg-bin",  default=os.environ.get("FFMPEG_BIN","ffmpeg"))
    ap.add_argument("--workers",     type=int, default=6)
    ap.add_argument("--log-every",   type=int, default=200)
    return ap.parse_args()

def sane_seconds(x):
    try:
        v = float(x)
        if not math.isfinite(v) or v < 0: return None
        return v
    except Exception:
        return None

def row_to_paths(row, raw_dir, out_dir):
    pid = row["participant_id"].strip()
    ts  = row["timestamp"].strip()            # e.g., 2018-10-23_17-59-51
    mp4 = os.path.join(raw_dir, pid, f"{ts}.mp4")
    wav = os.path.join(out_dir, pid, f"{ts}.wav")
    return pid, ts, mp4, wav

def ensure_parent(p):
    d = os.path.dirname(p)
    if d and not os.path.isdir(d):
        os.makedirs(d, exist_ok=True)

def ffmpeg_trim(ffmpeg, src, dst, start_s, end_s):
    # -ss before -i is fastest for mp4; -to is relative to start when used together
    dur = max(0.0, end_s - start_s)
    if dur <= 0: return "skip_bad_span"
    ensure_parent(dst)
    cmd = [
        ffmpeg, "-hide_banner", "-loglevel", "error", "-y",
        "-ss", f"{start_s:.3f}",
        "-i", src,
        "-t", f"{dur:.3f}",
        "-ac", "1", "-ar", "16000",
        dst,
    ]
    try:
        subprocess.run(cmd, check=True)
        return "ok"
    except subprocess.CalledProcessError as e:
        return f"ffmpeg_error:{e.returncode}"
    except FileNotFoundError:
        return "ffmpeg_not_found"

def process_one(row, raw_dir, out_dir, ffmpeg):
    pid, ts, mp4, wav = row_to_paths(row, raw_dir, out_dir)
    if not os.path.isfile(mp4):
        return ("missing_mp4", pid, ts)
    if os.path.isfile(wav):
        return ("exists", pid, ts)

    s = sane_seconds(row.get("start_time_s"))
    e = sane_seconds(row.get("end_time_s"))
    if s is None or e is None:
        return ("bad_times", pid, ts)
    if e < s:
        s, e = e, s  # swap if reversed

    res = ffmpeg_trim(ffmpeg, mp4, wav, s, e)
    return (res, pid, ts)

def main():
    args = parse_args()
    os.makedirs(args.out_dir, exist_ok=True)

    # read CSV once
    with open(args.transcripts, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        rows = [r for r in reader]

    total = len(rows)
    print(f"rows: {total}")
    stats = {"ok":0,"exists":0,"missing_mp4":0,"bad_times":0,"skip_bad_span":0}
    errors = 0

    with ProcessPoolExecutor(max_workers=args.workers) as ex:
        futs = [ex.submit(process_one, r, args.raw_dir, args.out_dir, args.ffmpeg_bin) for r in rows]
        for i, fut in enumerate(as_completed(futs), 1):
            res, pid, ts = fut.result()
            if res in stats: stats[res] += 1
            else: errors += 1
            if i % args.log_every == 0:
                print(f"... {i}/{total} done | ok={stats['ok']} exist={stats['exists']} "
                      f"missing={stats['missing_mp4']} badt={stats['bad_times']} "
                      f"short={stats['skip_bad_span']} err={errors}")

    print("summary:",
          {k:v for k,v in stats.items()},
          f"errors={errors}")

if __name__ == "__main__":
    main()
