# load required libraries
library(dplyr)
library(rlang)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)

# helper functions
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Google NLP error stored as a character string beginning with "#error"
has_google_error <- function(rec) {
  s <- tryCatch(rec$google_NLP_annotateText$sentences, error = function(e) NULL)
  is.character(s) && any(grepl("^#error", s))
}

# ----------------------------------------------------------------------

audio_dir   <- "data/archive/study2/files_raw"
audio_files <- list.files(audio_dir, "\\.RDS$", full.names = TRUE)

audio_list <- vector("list", length(audio_files))

for (i in seq_along(audio_files)) {
  path <- audio_files[i]
  pid  <- tools::file_path_sans_ext(basename(path))
  
  recs <- tryCatch(readRDS(path),
                   error = function(e) { warning("Skipping: ", path); NULL })
  if (is.null(recs) || length(recs) == 0) next
  
  ts <- names(recs); n <- length(ts)
  
  overall_dur    <- numeric(n)
  word_count     <- integer(n)
  transcript_vec <- character(n)
  sent_score     <- numeric(n)
  sent_mag       <- numeric(n)
  
  ## ---------- inner clip-loop ----------
  for (j in seq_len(n)) {
    rec <- recs[[j]]
    
    ## 1) overall duration
    overall_dur[j] <- if (!is.null(rec$audio_wave) && !is.null(rec$audio_wave$duration)) {
      rec$audio_wave$duration
    } else {
      NA_real_
    }
    
    ## 2) transcript + word count (robust to Google errors)
    if (!is.null(rec$speech.to.text)) {
      if (has_google_error(rec)) {
        # error present -> invalidate transcript & word count
        transcript_vec[j] <- NA_character_
        word_count[j]     <- 0L
      } else {
        # no error: count words only from proper segment lists
        parts <- Filter(function(x) is.list(x) && !is.null(x$words),
                        rec$speech.to.text)
        if (length(parts)) {
          # Some records store character vectors; coerce safely
          wc <- vapply(parts, function(p) {
            w <- tryCatch(p$words, error = function(e) character())
            length(w)
          }, integer(1))
          word_count[j] <- sum(wc)
        } else {
          word_count[j] <- 0L
        }
        transcript_vec[j] <- rec$speech.to.text$Complete.transcript %||% ""
      }
    } else {
      # no speech.to.text at all
      transcript_vec[j] <- ""
      word_count[j]     <- 0L
    }
    
    ## 3) sentiment
    if (!is.null(rec$google_NLP_annotateText) &&
        !is.null(rec$google_NLP_annotateText$documentSentiment)) {
      sent_tbl      <- rec$google_NLP_annotateText$documentSentiment
      sent_score[j] <- sent_tbl$score[1]
      sent_mag[j]   <- sent_tbl$magnitude[1]
    } else {
      sent_score[j] <- NA_real_
      sent_mag[j]   <- NA_real_
    }
  } # loop j
  
  ## ---------- store participant block ----------
  audio_list[[i]] <- tibble(
    participant_id   = pid,
    audio_time       = as.POSIXct(ts, "%Y-%m-%d_%H-%M-%S", tz = "UTC"),
    audio_id         = paste0(pid, "_", format(audio_time, "%Y%m%d%H%M%S")),
    wav_path         = sprintf("%s :: index %s", basename(path), seq_len(n)),
    overall_duration = overall_dur,
    word_count       = word_count,
    transcript       = transcript_vec,
    sent_score       = sent_score,
    sent_magnitude   = sent_mag
  )
  
  rm(recs); gc()
}

audio_data <- bind_rows(audio_list)

# save data
saveRDS(audio_data, "data/audio_data.rds")
