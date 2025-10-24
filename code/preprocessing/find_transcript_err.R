# find_transcription_errors.R
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

## -------------------- 1) Config --------------------
audio_dir <- "data/archive/study2/files_raw"         # <-- adjust if needed
out_path  <- "data/transcription_errors.csv"

audio_files <- list.files(audio_dir, "\\.RDS$", full.names = TRUE)
if (!length(audio_files)) {
  stop("No .RDS files found in: ", audio_dir)
}

## -------------------- 2) Helpers --------------------
# Extract the first "#error ..." line from google_NLP_annotateText$sentences
# Returns a character string (first error line) or NA_character_ if none.
get_error_line <- function(rec) {
  s <- tryCatch(rec$google_NLP_annotateText$sentences, error = function(e) NULL)
  if (is.null(s)) return(NA_character_)
  # Flatten lists to a character vector but ignore non-character entries
  if (is.list(s)) {
    s <- unlist(lapply(s, function(x) if (is.character(x)) x else NULL), use.names = FALSE)
  }
  if (!length(s)) return(NA_character_)
  hits <- grep("^\\s*#\\s*error", s, ignore.case = TRUE, value = TRUE)
  if (length(hits)) hits[[1]] else NA_character_
}

## -------------------- 3) Scan all bundles --------------------
results <- data.frame(
  file      = character(0),
  clip_idx  = integer(0),
  error_text= character(0),
  stringsAsFactors = FALSE
)

pb <- utils::txtProgressBar(min = 0, max = length(audio_files), style = 3)
for (f_idx in seq_along(audio_files)) {
  f <- audio_files[f_idx]
  recs <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(recs)) { utils::setTxtProgressBar(pb, f_idx); next }
  
  # Normalize: if single record (has $audio_wave), wrap into a list
  if (is.list(recs) && !is.null(recs$audio_wave)) recs <- list(recs)
  
  n <- length(recs)
  if (n == 0) { utils::setTxtProgressBar(pb, f_idx); next }
  
  for (i in seq_len(n)) {
    rec <- recs[[i]]
    err_line <- get_error_line(rec)
    if (!is.na(err_line)) {
      results <- rbind(
        results,
        data.frame(
          file       = basename(f),
          clip_idx   = i,
          error_text = substr(err_line, 1, 200),
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  utils::setTxtProgressBar(pb, f_idx)
}
close(pb)

## -------------------- 4) Save & report --------------------
cat("Found", nrow(results), "clips with transcription errors.\n")

if (nrow(results)) {
  write_csv(results, out_path)
  print(utils::head(results, 10))
  cat("\nSaved details to:", out_path, "\n")
} else {
  cat("No transcription errors detected.\n")
}
