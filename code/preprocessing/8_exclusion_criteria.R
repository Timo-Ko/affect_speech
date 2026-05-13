## libraries  --------------------------------------------------------
library(dplyr)
library(psych)

##  load the combined data  -------------------------------------------
matched_df <- readRDS("data/audio_ema_matched.rds")

## keep audios with ema within 10 mins available  ------------------------------
filtered_time_df <- matched_df %>% 
  filter(abs(time_diff)      <= 600)          # 10 mins

# get some statistics for initial sample
counts <- filtered_time_df %>%
  count(participant_id)

mean_obs <- mean(counts$n)
sd_obs   <- sd(counts$n)

mean_obs
sd_obs

### apply filters

## FILTER A  ─ clip-level rules (min 10 words spoken and 5 secs of voiced speech) ------------------------------
filtered_length_df <- filtered_time_df %>% 
  filter(n_words.x      >= 10 & duration_s      >= 5)          # ≥ 10 words and 5 secs of voiced audio

## FILTER B  ─ only one speaker detected ------------------------------
filtered_diarization_df <- filtered_length_df %>% 
  filter(num_speakers      == 1)          


# append demographics to final data

first_nonmissing <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) NA_character_ else x[1]
}

demographics <- read.csv("data/subset_beiwe_2018UTdemog.csv", stringsAsFactors = FALSE) %>%
  transmute(
    beiwe_id = beiwe_id,
    recorded = as.POSIXct(intro_demographics_RecordedDate_string, format = "%m/%d/%y %H:%M"),
    Gender = trimws(intro_demographics_Q1_string),
    Age = suppressWarnings(as.numeric(intro_demographics_Q24_string)),
    Ethnicity_raw = trimws(intro_demographics_Q7_string)
  ) %>%
  mutate(
    Ethnicity_raw = na_if(Ethnicity_raw, "")
  ) %>%
  arrange(beiwe_id, recorded) %>%
  group_by(beiwe_id) %>%
  summarise(
    Gender = first_nonmissing(Gender),
    Age = suppressWarnings(as.numeric(first_nonmissing(as.character(Age)))),
    Ethnicity_raw = first_nonmissing(Ethnicity_raw),
    .groups = "drop"
  ) %>%
  mutate(
    Ethnicity = case_when(
      is.na(Ethnicity_raw) ~ NA_character_,
      grepl(",", Ethnicity_raw, fixed = TRUE) ~ "Multiple",
      grepl("Hispanic/Latino", Ethnicity_raw, fixed = TRUE) ~ "Hispanic/Latino",
      grepl("Asian/ Asian American", Ethnicity_raw, fixed = TRUE) ~ "Asian/Asian American",
      grepl("Anglo/White", Ethnicity_raw, fixed = TRUE) ~ "White",
      TRUE ~ "Other"
    )
  ) %>%
  select(beiwe_id, Age, Gender, Ethnicity)

audio_ema_matched_cleaned <- filtered_diarization_df %>%
  left_join(
    demographics,
    by = c("participant_id" = "beiwe_id")
  ) %>%
  relocate(participant_id, Age, Gender, Ethnicity, .before = everything())


## save 
saveRDS(audio_ema_features_cleaned, "data/audio_ema_matched_cleaned.rds") # rds

## get some descriptive statistics for final sample
counts <- audio_ema_matched_cleaned %>%
  count(participant_id)

mean_obs <- mean(counts$n)
sd_obs   <- sd(counts$n)

mean_obs
sd_obs

describe(audio_ema_matched_cleaned$n_words.x)
describe(audio_ema_matched_cleaned$duration_s)

# participant demographics
participant_demo <- audio_ema_matched_cleaned %>%
  distinct(participant_id, Age, Gender, Ethnicity)

# age
age_summary <- participant_demo %>%
  summarise(
    n_age = sum(!is.na(Age)),
    mean_age = mean(Age, na.rm = TRUE),
    sd_age = sd(Age, na.rm = TRUE)
  )

age_summary

# gender distribution
gender_summary <- participant_demo %>%
  count(Gender, name = "n") %>%
  mutate(
    share_all = n / sum(n),
    share_nonmissing = ifelse(
      is.na(Gender),
      NA_real_,
      n / sum(n[!is.na(Gender)])
    )
  )

gender_summary

# ethnicity distribution
ethnicity_summary <- participant_demo %>%
  count(Ethnicity, name = "n") %>%
  mutate(
    share_all = n / sum(n),
    share_nonmissing = ifelse(
      is.na(Ethnicity),
      NA_real_,
      n / sum(n[!is.na(Ethnicity)])
    )
  )

ethnicity_summary

# cors between emotion self-reports

cor_mat <- cor(audio_ema_matched_cleaned %>% select(ema_content, ema_sad, ema_energy), use = "pairwise.complete.obs", method = "spearman")

cor_mat

# distribution of emotion self-reports
describe(audio_ema_matched_cleaned$ema_content)
describe(audio_ema_matched_cleaned$ema_sad)
describe(audio_ema_matched_cleaned$ema_energy)

# create supplementary figure

library(tidyverse)

ema_long <- audio_ema_matched_cleaned %>%
  transmute(
    Contentment = ema_content,
    Sadness = ema_sad,
    Arousal = ema_energy
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "State",
    values_to = "Rating"
  ) %>%
  filter(!is.na(Rating)) %>%
  mutate(
    State = factor(State, levels = c("Contentment", "Sadness", "Arousal")),
    Rating = as.integer(Rating)
  )

valid_ratings <- tibble(
  State = c(rep("Contentment", 4), rep("Sadness", 4), rep("Arousal", 5)),
  Rating = c(0:3, 0:3, 0:4)
) %>%
  mutate(
    State = factor(State, levels = c("Contentment", "Sadness", "Arousal"))
  )

ema_counts <- ema_long %>%
  count(State, Rating) %>%
  right_join(valid_ratings, by = c("State", "Rating")) %>%
  mutate(n = replace_na(n, 0))

y_max <- max(ema_counts$n)

state_dist <- ggplot(ema_counts, aes(x = factor(Rating), y = n)) +
  geom_col(color = "white", fill = "grey70", width = 0.9) +
  facet_wrap(~ State, nrow = 1, scales = "free_x") +
  scale_y_continuous(
    limits = c(0, y_max * 1.05),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = "Self-reported rating",
    y = "Count"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(
  filename = "figures/fig_s2_state_distribution.png",
  plot = state_dist,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)

# finish