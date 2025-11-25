## libraries  --------------------------------------------------------
library(dplyr)
library(psych)


##  load the combined data  -------------------------------------------
matched_df <- readRDS("data/audio_ema_matched.rds")


# library(jsonlite)
# library(dplyr)
# library(stringr)
# 
# 
# diarization <- fromJSON("data/diarization_per_file.json") %>%
#   as_tibble() %>%
#   mutate(
#     timestamp = str_remove(file, "\\.mp4\\.json$"),
#     participant_id = participant
#   ) %>%
#   select(participant_id, timestamp, num_speakers)
# 
# diarization <- diarization %>%
#   mutate(
#     timestamp = as.POSIXct(
#       timestamp,
#       format = "%Y-%m-%d_%H-%M-%S",
#       tz = "UTC"
#     )
#   )
# 
# matched_df <- matched_df %>% left_join(., diarization, by = c("participant_id", "timestamp"))

## FILTER A  ─ ema within 10 mins available  ------------------------------
filtered_time_df <- matched_df %>% 
  filter(abs(time_diff)      <= 600)          # 10 mins

# get some statistics for initial sample
counts <- filtered_time_df %>%
  count(participant_id)

mean_obs <- mean(counts$n)
sd_obs   <- sd(counts$n)

mean_obs
sd_obs

## FILTER B  ─ clip-level rules (min 10 words spoken and 5 secs of voiced speech) ------------------------------
filtered_length_df <- filtered_time_df %>% 
  filter(n_words.x      >= 10 & duration_s      >= 5)          # ≥ 10 words and 5 secs of voiced audio

## FILTER C  ─ only one speaker detected ------------------------------
filtered_diarization_df <- filtered_length_df %>% 
  filter(num_speakers      == 1)          

# append demographics to final data
demographics <- read.csv("data/Age_Gender_Fa18.csv")

audio_ema_matched_cleaned <- filtered_diarization_df  %>%
  left_join(
    demographics,
    by = c("participant_id" = "beiwe_id")
  ) %>%
  relocate(participant_id, Age, Gender, .before = everything())


## save and quick report ---------------------------------------------
saveRDS(audio_ema_matched_cleaned, "data/audio_ema_matched_cleaned.rds") # rds

cat("\nFinal data: ",
    n_distinct(audio_ema_matched_cleaned$participant_id), " participants, ",
    nrow(audio_ema_matched_cleaned), " matched voice-EMA instances kept.\n")

# get some statistics for final sample
counts <- audio_ema_matched_cleaned %>%
  count(participant_id)

mean_obs <- mean(counts$n)
sd_obs   <- sd(counts$n)

mean_obs
sd_obs

describe(audio_ema_matched_cleaned$n_words.x)
describe(audio_ema_matched_cleaned$duration_s)

# participant demographics
share_female <- audio_ema_matched_cleaned %>%
  distinct(participant_id, Gender) %>%      # one row per participant
  summarise(
    n_total   = n(),
    n_female  = sum(Gender == "Female", na.rm = TRUE),
    share_female = n_female / n_total
  )

share_female

mean_age <- audio_ema_matched_cleaned %>%
  distinct(participant_id, Age) %>%
  summarise(mean_age = mean(Age, na.rm = TRUE))

mean_age



# emotion self-reports
describe(audio_ema_matched_cleaned$ema_content)
describe(audio_ema_matched_cleaned$ema_sad)
describe(audio_ema_matched_cleaned$ema_energy)

# finish