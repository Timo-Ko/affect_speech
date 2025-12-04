## ============================================================
## 0) Setup
## ============================================================
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)
`%||%` <- rlang::`%||%`

## helper: recode EMA answers (safe for NA question_id)
recode_ema_answer <- function(question_id, answer_chr) {
  if (is.na(question_id)) return(NA_real_)
  
  a    <- stringr::str_trim(answer_chr %||% "")
  a_lc <- stringr::str_to_lower(a)
  
  if (a == "" || a_lc %in% c("not_presented","not presented","no_answer_selected",
                             "skipped","skip question","na","n/a","null")) {
    return(NA_real_)
  }
  
  num <- suppressWarnings(readr::parse_number(a))
  if (!is.na(num)) return(as.numeric(num))
  
  if (question_id %in% c("content","sad")) {
    map4 <- c(
      "not at all"   = 0,
      "a little bit" = 1,
      "quite a bit"  = 2,
      "very much"    = 3
    )
    return(unname(map4[a_lc] %||% NA_real_))
  }
  
  if (question_id == "energy") {
    map5_0based <- c(
      "low energy"           = 0, "low"            = 0,
      "somewhat low energy"  = 1, "somewhat low"   = 1,
      "neutral"              = 2,
      "somewhat high energy" = 3, "somewhat high"  = 3,
      "high energy"          = 4, "high"           = 4
    )
    return(unname(map5_0based[a_lc] %||% NA_real_))
  }
  
  NA_real_
}

## ============================================================
## 1) Read paired EMA–audio CSV
## ============================================================
ema_audio_long <- read.csv("data/EMAS_paired_with_audio_long_form_dataset.csv")

ema_audio_long <- ema_audio_long %>%
  select(-X, -Unnamed..0)   # drop index cols

## ============================================================
## 2) Propagate matched audio and its EMA time within each survey file
##    Group key = participant_id + filename (beep file)
## ============================================================

ema_audio_long <- ema_audio_long %>%
  mutate(
    participant_id = tools::file_path_sans_ext(names),
    beep_id        = filename                         # one file = one EMA survey
  ) %>%
  group_by(participant_id, beep_id) %>%
  mutate(
    # choose one audio file per survey 
    .audio_file = {
      v <- closest_audio_filename[!is.na(closest_audio_filename) &
                                    closest_audio_filename != ""]
      if (length(v) == 0) NA_character_ else v[1]
    },
    
    # EMA time used for matching: from the row where that audio_file was chosen
    .ema_match_time_chr = {
      if (is.na(.audio_file[1])) {
        NA_character_
      } else {
        idx <- which(closest_audio_filename == .audio_file[1] & !is.na(UTC.time))
        if (length(idx) == 0) NA_character_ else as.character(UTC.time[idx[1]])
      }
    },
    
    # time difference 
    .ema_match_dt = {
      if (is.na(.audio_file[1])) {
        NA_real_
      } else {
        idx <- which(closest_audio_filename == .audio_file[1] & !is.na(difftime))
        if (length(idx) == 0) NA_real_ else difftime[idx[1]]
      }
    }
  ) %>%
  ungroup() %>%
  mutate(
    matched_audio_filename = .audio_file,
    nearest_survey_time    = ymd_hms(.ema_match_time_chr, tz = "UTC"),
    time_diff              = as.numeric(.ema_match_dt),
    adt                    = abs(time_diff)
  ) %>%
  select(-.audio_file, -.ema_match_time_chr, -.ema_match_dt)

## ============================================================
## 3) Build per-audio wide table with EMA scores
## ============================================================

py_matched_wide <- ema_audio_long %>%
  # keep only surveys that have a matched audio
  filter(!is.na(matched_audio_filename)) %>%
  
  mutate(
    audio_key = basename(matched_audio_filename) %>%
      str_replace("\\.wav$", ""),
    
    audio_timestamp = as.POSIXct(
      audio_key,
      format = "%Y-%m-%d_%H-%M-%S",
      tz = "UTC"
    ),
    
    q_clean = question.text %>%
      str_squish() %>%
      str_replace(":$", ""),
    
    question_id = case_when(
      str_detect(q_clean, regex("\\bcontent\\b", ignore_case = TRUE)) ~ "content",
      str_detect(q_clean, regex("\\bsad\\b",     ignore_case = TRUE)) ~ "sad",
      str_detect(q_clean, regex("\\benergy\\b",  ignore_case = TRUE)) ~ "energy",
      TRUE ~ NA_character_
    ),
    
    answer = as.character(answer)
  ) %>%
  
  filter(!is.na(question_id)) %>%
  
  mutate(
    answer_num = mapply(recode_ema_answer, question_id, answer) %>% as.numeric()
  ) %>%
  
  group_by(participant_id, audio_key, audio_timestamp,
           nearest_survey_time, time_diff, adt, question_id) %>%
  summarise(
    answer_num = dplyr::first(answer_num),
    .groups = "drop"
  ) %>%
  
  pivot_wider(
    id_cols     = c(participant_id, audio_key, audio_timestamp,
                    nearest_survey_time, time_diff, adt),
    names_from  = question_id,
    values_from = answer_num,
    names_prefix = "ema_"
  ) %>%
  
  rename(
    timestamp = audio_timestamp
  ) %>%
  arrange(participant_id, timestamp)

## Inspect to sanity-check
dim(py_matched_wide)
summary(py_matched_wide$time_diff)
head(py_matched_wide)

## ============================================================
## 4) Join audio features
## ============================================================

audio_df <- readRDS("data/audio_features.rds") %>%
  mutate(
    timestamp = as.POSIXct(as.character(timestamp),
                           format = "%Y-%m-%d_%H-%M-%S",
                           tz = "UTC")
  )

audio_ema_matched <- audio_df %>%
  left_join(py_matched_wide,
            by = c("participant_id", "timestamp"))

saveRDS(audio_ema_matched, "data/audio_ema_matched.rds")

# finish 
