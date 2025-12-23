## libraries  --------------------------------------------------------
library(dplyr)
library(psych)


##  load the combined data  -------------------------------------------
matched_df <- readRDS("data/audio_ema_matched.rds")

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
saveRDS(audio_ema_features, "data/audio_ema_matched_cleaned.rds") # rds

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

# cors between emotion self-reports

cor_mat <- cor(audio_ema_matched_cleaned %>% select(ema_content, ema_sad, ema_energy), use = "pairwise.complete.obs", method = "spearman")

cor_mat

# distribution of emotion self-reports
describe(audio_ema_matched_cleaned$ema_content)
describe(audio_ema_matched_cleaned$ema_sad)
describe(audio_ema_matched_cleaned$ema_energy)

# create figure

library(tidyverse)

ema_long <- audio_ema_matched_cleaned %>%
  transmute(
    Contentment = ema_content,
    Sadness     = ema_sad,
    Arousal     = ema_energy
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "State",
    values_to = "Rating"
  ) %>%
  filter(!is.na(Rating)) %>%              # ← remove NAs explicitly
  mutate(
    Rating = factor(as.integer(Rating), levels = 0:4),
    State  = factor(State, levels = c("Contentment", "Sadness", "Arousal"))
  )

ema_counts <- ema_long %>%
  count(State, Rating) %>%
  complete(State, Rating, fill = list(n = 0))

y_max <- max(ema_counts$n)

state_dist <- ggplot(ema_counts, aes(x = Rating, y = n)) +
  geom_col(color = "white", fill = "grey70", width = 0.9) +
  facet_wrap(~ State, nrow = 1, scales = "fixed") +
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
  filename = "figures/state_distribution.png",
  plot     = state_dist,
  width    = 12,
  height   = 6,
  units    = "in",
  dpi      = 300
)



# finish