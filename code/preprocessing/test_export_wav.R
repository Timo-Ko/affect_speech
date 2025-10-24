suppressPackageStartupMessages({
  library(tuneR)
  library(tools)
})

`%||%` <- function(a,b) if (!is.null(a)) a else b

.save_wave_int16 <- function(sig, fs, out_wav, target_peak = 0.98) {
  sig <- as.numeric(sig); sig[!is.finite(sig)] <- 0
  
  # Heuristic: detect stored scale
  peak <- max(abs(sig), na.rm = TRUE)
  if (is.na(peak)) peak <- 0
  
  if (peak == 0) {
    warning("All-zero signal; writing silence.")
    pcm <- rep.int(0L, length(sig))
  } else if (peak <= 1.5) {
    # float in [-1,1] → normalize then convert to int16
    sig <- sig * (target_peak / peak)
    sig[sig >  1] <-  1; sig[sig < -1] <- -1
    pcm <- as.integer(round(sig * 32767))
  } else if (peak <= 40000) {
    # already int16-ish numeric → normalize to target_peak of 32767
    sig <- sig * (target_peak * 32767 / peak)
    sig[sig >  32767]  <-  32767
    sig[sig < -32768]  <- -32768
    pcm <- as.integer(round(sig))
  } else {
    # unexpected scale → rescale robustly
    sig <- sig / peak
    pcm <- as.integer(round(sig * 32767 * target_peak))
  }
  
  W <- Wave(left = pcm, samp.rate = as.integer(fs), bit = 16)
  writeWave(W, out_wav)
  invisible(normalizePath(out_wav))
}

# Trim helper (optional)
.trim_to_speech <- function(wav, rec, pad = 200L) {
  parts <- tryCatch(rec$speech.to.text, error = function(e) NULL)
  if (is.null(parts)) return(wav)
  getv <- function(x, nm) if (is.list(x)) x[[nm]] else NA_real_
  st <- unlist(lapply(parts, getv, "start_time"))
  et <- unlist(lapply(parts, getv, "end_time"))
  st <- st[is.finite(st) & st >= 0]; et <- et[is.finite(et) & st >= 0]
  if (!length(st) || !length(et)) return(wav)
  s1 <- max(1L, floor(min(st) * wav$fs) - pad)
  s2 <- min(wav$nSamples, ceiling(max(et) * wav$fs) + pad)
  wav$t        <- wav$t[s1:s2]
  wav$sig      <- if (!is.null(dim(wav$sig)) && length(dim(wav$sig)) > 1L) wav$sig[s1:s2, , drop = FALSE] else wav$sig[s1:s2]
  wav$nSamples <- length(wav$t)
  wav$duration <- wav$nSamples / wav$fs
  wav
}

save_clip_from_rds <- function(rds_path,
                               clip_name   = NULL,
                               index       = NULL,
                               out_wav     = NULL,
                               trim_speech = FALSE,
                               pad_ms      = 200L) {
  obj <- readRDS(rds_path)
  rec_names <- names(obj)
  is_single <- is.list(obj) && !is.null(obj$audio_wave)
  
  if (is_single) {
    rec <- obj
    picked_name <- file_path_sans_ext(basename(rds_path))
  } else {
    if (!is.null(clip_name) && clip_name %in% rec_names) {
      rec <- obj[[clip_name]]; picked_name <- clip_name
    } else if (!is.null(index)) {
      stopifnot(index >= 1, index <= length(obj))
      rec <- obj[[index]]; picked_name <- if (length(rec_names)) rec_names[index] else paste0("clip", index)
    } else {
      stop("Provide clip_name or index.")
    }
  }
  
  stopifnot(is.list(rec), !is.null(rec$audio_wave))
  wav <- rec$audio_wave
  if (trim_speech) wav <- .trim_to_speech(wav, rec, pad = as.integer(pad_ms))
  
  sig <- wav$sig
  if (!is.null(dim(sig)) && length(dim(sig)) > 1L) sig <- rowMeans(sig)
  fs  <- as.integer(wav$fs %||% 16000L)
  
  # Diagnostics
  signum <- as.numeric(sig); signum[!is.finite(signum)] <- 0
  cat(sprintf("fs=%d | len=%d | peak=%.6f | mean(abs)=%.6f\n",
              fs, length(signum), max(abs(signum)), mean(abs(signum))))
  
  if (is.null(out_wav)) {
    base_id <- file_path_sans_ext(basename(rds_path))
    clip_id <- file_path_sans_ext(basename(picked_name))
    out_wav <- file.path(getwd(), paste0(base_id, "__", clip_id, ".wav"))
  }
  
  p <- .save_wave_int16(signum, fs, out_wav)
  message("WAV saved: ", p, "\nDownload from the Files pane.")
}

#source("save_clip_from_rds.R")

save_clip_from_rds(
  rds_path   = "data/archive/study2/files_raw/ips3wori.RDS",
  clip_name  = "2018-10-10_20-59-50.mp4")






### check zeros 

suppressPackageStartupMessages({
  library(purrr)
  library(dplyr)
  library(tibble)
})

check_audio_in_rds <- function(path) {
  obj <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(obj)) return(tibble(file = basename(path), has_audio = NA, nonzero_samples = NA))
  
  # Normalize: top-level list of clips or a single clip
  if (is.list(obj) && !is.null(obj$audio_wave)) {
    clips <- list(obj)
  } else if (is.list(obj)) {
    clips <- obj
  } else {
    return(tibble(file = basename(path), has_audio = FALSE, nonzero_samples = 0))
  }
  
  nonzero_counts <- map_dbl(clips, function(rec) {
    sig <- tryCatch(rec$audio_wave$sig, error = function(e) NULL)
    if (is.null(sig)) return(0)
    vals <- as.numeric(sig)
    vals[!is.finite(vals)] <- 0
    sum(abs(vals) > 1e-6)   # count nonzero samples
  })
  
  tibble(
    file = basename(path),
    n_clips = length(clips),
    nonzero_samples = sum(nonzero_counts),
    has_audio = any(nonzero_counts > 0)
  )
}

# --- run on your data directory ---
rds_files <- list.files("data/archive/study2/files_raw", pattern = "\\.RDS$", full.names = TRUE)
results <- map_dfr(rds_files[1:10], check_audio_in_rds)

# quick summary
results %>%
  count(has_audio) %>%
  print()

# see which files still have real sound
results %>% filter(has_audio)



