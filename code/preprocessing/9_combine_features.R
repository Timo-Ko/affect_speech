library(dplyr)
library(purrr)

## combine all features 

# load audio ema matches
audio_ema_matched <- readRDS("data/audio_ema_matched_cleaned.rds")

# read in features

# word embeddings
wordembeddings <- readRDS("data/textembeddings_robertalarge.rds") # regular
wordembeddings_masked <- readRDS("data/textembeddings_robertalarge_masked.rds") # masked

# add suffix "_masked" to all remaining column names
colnames(wordembeddings_masked)[colnames(wordembeddings_masked) != "audio_id"] <-
  paste0(colnames(wordembeddings_masked)[colnames(wordembeddings_masked) != "audio_id"], "_masked")

# speech embeddings
speechembeddings <- readRDS("data/speech_embeddings_all.rds")

speechembeddings$src_file <- NULL
speechembeddings$rec_index  <- NULL

# liwc scores from liwc-22 software

# regular
liwc <- read.csv("data/LIWC-22 Results - audio_transcripts - LIWC Analysis.csv")
liwc$transcript <- NULL
liwc$Segment <- NULL

# masked
liwc_masked <- read.csv("data/LIWC-22 Results - audio_transcripts_masked - LIWC Analysis.csv")
liwc_masked$transcript_masked <- NULL
liwc_masked$Segment <- NULL

# add suffix "_masked" to all columns except "audio_id"
colnames(liwc_masked)[colnames(liwc_masked) != "audio_id"] <-
  paste0(colnames(liwc_masked)[colnames(liwc_masked) != "audio_id"], "_masked")

# egemaps
egemaps <- readRDS("data/egemaps_all.rds")

# combine all
audio_ema_features <- list(
  audio_ema_matched,
  wordembeddings,
  wordembeddings_masked,
  speechembeddings,
  liwc,
  liwc_masked,
  egemaps
) %>% purrr::reduce(~ left_join(.x, .y, by = "audio_id"))

# investigate edge cases where speech features could not be extracted

# save data
saveRDS(audio_ema_features, "data/audio_ema_features.rds")

# finish