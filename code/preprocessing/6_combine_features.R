# load packages

library(dplyr)
library(purrr)
library(readr)
library(data.table)
library(jsonlite)
library(tidyr)
library(stringr)

# transcripts and general info 

transcripts <- read.csv("data/transcripts_combined.csv")

# diarization results 

# convert to a clean dataframe
diarization_df <- fromJSON("data/diarization_per_file.json") %>%
  as_tibble() %>%
  mutate(
    timestamp = str_remove(file, "\\.mp4\\.json$"),
    participant_id = participant
  ) %>%
  select(participant_id, timestamp, num_speakers)


# word embeddings
wordembeddings <- readRDS("data/textembeddings_robertalarge.rds") # regular

wordembeddings_masked  <- readRDS("data/textembeddings_robertalarge_masked.rds") %>%
  rename_with(~ paste0(.x, "_masked"), -c(participant_id, timestamp))

# speech embeddings

# Directory containing per-user embeddings
in_dir <- "data/speech_embeddings"

# List all CSV files
files <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE)

# read each CSV; skip empty or broken files; keep differing columns
read_one <- function(f) {
  if (file.size(f) == 0) return(NULL)
  tryCatch(
    fread(f, showProgress = FALSE),           # fast and robust
    error = function(e) { message("FAIL: ", f, " -> ", e$message); NULL }
  )
}

lst <- lapply(files, read_one)
speech_embeddings_all <- rbindlist(lst, fill = TRUE, use.names = TRUE)

cat("Files:", length(files),
    " | Loaded:", sum(vapply(lst, Negate(is.null), logical(1))),
    " | Rows:", nrow(speech_embeddings_all), "\n")


speechembeddings <- speech_embeddings_all %>%
  mutate(
    timestamp = format(as.POSIXct(timestamp_s, origin = "1970-01-01", tz = "UTC"),
                       "%Y-%m-%d_%H-%M-%S")
  ) %>%
  select(participant_id, timestamp, starts_with("wav2vec2_"))

# liwc scores from liwc-22 software

# regular
liwc <- read.csv("data/LIWC-22 Results - transcripts_combined - LIWC Analysis.csv") %>%
  select(-text, -Segment, -text, -start_time_s, -end_time_s, -duration_s)

# masked
liwc_masked <- read.csv("data/LIWC-22 Results - transcripts_masked - LIWC Analysis.csv") %>%
  select(-text_masked, -Segment, -text_masked, -start_time_s, -end_time_s, -duration_s) %>%
  rename_with(~ paste0(.x, "_masked"), -c(participant_id, timestamp))

# egemaps
egemaps <- read.csv("data/egemaps_all.csv")

egemaps <- egemaps %>%
  mutate(
    timestamp = format(as.POSIXct(timestamp_s, origin = "1970-01-01", tz = "UTC"),
                       "%Y-%m-%d_%H-%M-%S")
  ) %>%
  select(participant_id, timestamp, everything(), -file_path, -file, -timestamp_s, -trim_start_s, -trim_end_s)

# combine all
audio_features <- list(
  transcripts,
  diarization_df,
  liwc,
  liwc_masked,
  wordembeddings,
  wordembeddings_masked,
  speechembeddings,
  egemaps
) %>%
  purrr::reduce(~ dplyr::left_join(.x, .y, by = c("participant_id", "timestamp")))


# save data
saveRDS(audio_features, "data/audio_features.rds")

# finish