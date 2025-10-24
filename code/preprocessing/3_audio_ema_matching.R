library(dplyr)
library(tidyr)

## --------------------------------------------------------------------------
## 1) read the prepared data frames
audio_df <- readRDS("data/audio_data.rds")   # contains audio_id, audio_time, …
ema_df   <- readRDS("data/ema_data.rds")     # contains ema_id,   survey_time, …

## --------------------------------------------------------------------------
## 2) match every audio clip to the temporally nearest EMA from the same
##    participant.  All original columns are preserved.
audio_ema_matched <- audio_df %>% 
  left_join(
    ema_df,
    by           = "participant_id",
    multiple     = "all",
    relationship = "many-to-many"
  ) %>% 
  mutate(
    dt  = as.numeric(difftime(audio_time, survey_time, units = "secs")),
    adt = abs(dt)
  ) %>% 
  group_by(participant_id, audio_id, audio_time) %>% 
  slice_min(adt, n = 1, with_ties = TRUE) %>%       # step 1
  slice_max(dt,  n = 1, with_ties = FALSE) %>%      # step 2
  ungroup() %>% 
  
  ## --- drop duplicate-EMA matches (keep audio with smaller |Δt|) ---
  group_by(participant_id, ema_id, survey_time) %>% 
  slice_min(abs(dt), n = 1, with_ties = FALSE) %>%  # step 4
  ungroup() %>% 
  
  mutate(time_diff = dt) %>% 
  select(-adt, -dt) %>% 
  arrange(participant_id, audio_time)

## --------------------------------------------------------------------------

## 3) save 

saveRDS(audio_ema_matched, "data/audio_ema_matched.rds")

## finish