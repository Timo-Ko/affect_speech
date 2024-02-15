### PREPARATION ####

# load data 

egemaps_features <- readRDS("study1_ger/data/egemaps_features.RData")
compare_features <- readRDS("study1_ger/data/compare_features.RData")

affect_df <- readRDS("study1_ger/data/affect_df.RData")

affect_df$user_id <- as.integer(affect_df$user_id)

# load demographic data 
demographics_df <- readRDS("../questionnaire_data/data/demographics_df.RData")

### MERGE AFFECT WITH AUDIO FEATURES ####

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
saveRDS(affect_egemaps, "study1_ger/data/affect_egemaps.RData")
saveRDS(affect_compare, "study1_ger/data/affect_compare.RData")

### DESCRIPTIVES OF MERGED DATA ####

# number of participants
length(unique(affect_egemaps$user_id))

# number of es
length(unique(affect_egemaps$e_s_questionnaire_id))

## FINISH