### SAMPLE DESCRIPTIVES ###

# read in data frames
affect_egemaps  <- readRDS("data/affect_egemaps.RData")

# total number of audio samples with corresponding 
nrow(affect_egemaps)

# number of participants in the final sample
length(unique(affect_egemaps$user_id))

length(unique(affect_egemaps$e_s_questionnaire_id))


# show distribution of valence and arousal values in final sample

hist(affect_egemaps$valence)

hist(affect_egemaps$arousal)

# show distribution of sentence conditions

table(affect_egemaps$condition)

# show valence and arousal statistics per condition - no differences!

affect_egemaps %>%
  group_by(condition) %>%
  dplyr::summarize(mean_valence = mean(valence, na.rm=TRUE), mean_arousal = mean(arousal, na.rm=TRUE))

