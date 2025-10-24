## 0)  libraries  --------------------------------------------------------
library(dplyr)

## 1)  load the combined data  -------------------------------------------
matched_df <- readRDS("data/audio_ema_matched.rds")

## 2)  FILTER A  ─ time diff between ema and speech entry <= 10 mins ------------------------------
filtered_time_df <- matched_df %>% 
  filter(abs(time_diff)      <= 600)          # 10 mins

## 3)  FILTER B  ─ clip-level rules (min 10 words spoken and 5 secs of voiced speech) ------------------------------
filtered_length_df <- filtered_time_df %>% 
  filter(word_count      >= 10 & voice_duration      >= 5)          # ≥ 10 words and 5 secs of voiced audio

## 5)  save and quick report ---------------------------------------------
saveRDS(filtered_length_df, "data/audio_ema_matched_cleaned.rds") # rds

cat("\nFinal data: ",
    n_distinct(filtered_df$participant_id), " participants, ",
    nrow(filtered_df), " matched voice-EMA instances kept.\n")


# finish