### PREPARATION ####

# load data 

egemaps_features <- readRDS("data/study1/egemaps_features.RData")
compare_features <- readRDS("data/study1/compare_features.RData")

affect_df <- readRDS("data/study1/affect_df.RData")

affect_df$user_id <- as.integer(affect_df$user_id)

# load demographic data 
demographics_df <- readRDS("data/study1/demographics_df.RData")

### MERGE AFFECT WITH AUDIO FEATURES ####

# remove unnedded columns
egemaps_features$name <- NULL
compare_features$name <- NULL

egemaps_features$frameTime <- NULL
compare_features$frameTime <- NULL

affect_egemaps <- merge(egemaps_features, affect_df, by="e_s_questionnaire_id")

affect_compare <- merge(compare_features, affect_df, by="e_s_questionnaire_id")

# merge data
affect_egemaps <- egemaps_features %>%
  dplyr::inner_join(affect_df, by=c("e_s_questionnaire_id" = "e_s_questionnaire_id", "user_id" = "user_id", "questionnaireStartedTimestamp"= "questionnaireStartedTimestamp")) %>%  # join w voice features
  dplyr::left_join(demographics_df, by=c("user_id" = "p_0001")) # join w demographics

affect_compare <- compare_features %>%
  dplyr::inner_join(affect_df, by=c("e_s_questionnaire_id" = "e_s_questionnaire_id", "user_id" = "user_id", "questionnaireStartedTimestamp"= "questionnaireStartedTimestamp")) %>%  # join w voice features
  dplyr::left_join(demographics_df, by=c("user_id" = "p_0001")) # join w demographics

# reorder columns
affect_egemaps <- affect_egemaps  %>% 
  dplyr::select(c("e_s_questionnaire_id", "questionnaireStartedTimestamp", "id", "user_id" , "Demo_A1", "Demo_GE1", "condition", "valence", "md_valence", "diff_valence", "arousal", "md_arousal", "diff_arousal"),everything())

affect_compare <- affect_compare  %>%
  dplyr::select(c("e_s_questionnaire_id", "questionnaireStartedTimestamp", "id", "user_id" , "Demo_A1", "Demo_GE1", "condition", "valence", "md_valence", "diff_valence", "arousal", "md_arousal", "diff_arousal"),everything())

# save dfs 
saveRDS(affect_egemaps, "data/study1/affect_egemaps_study1.RData")
saveRDS(affect_compare, "data/study1/affect_compare_study1.RData")

### DESCRIPTIVES OF MERGED DATA ####

# number of participants
length(unique(affect_egemaps$user_id))

# number of es
length(unique(affect_egemaps$e_s_questionnaire_id))

# number of audio logs
dim(affect_egemaps)

## FINISH