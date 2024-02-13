### PREPARATION ####

# load data 

egemaps_features <- readRDS("data/egemaps_features.RData")
compare_features <- readRDS("data/compare_features.RData")

affect_df <- readRDS("data/affect_df.RData")

### MERGE AFFECT WITH AUDIO FEATURES ####

affect_egemaps <- merge(egemaps_features, affect_df, by="e_s_questionnaire_id")

affect_compare <- merge(compare_features, affect_df, by="e_s_questionnaire_id")

# reorder columns
affect_egemaps <- affect_egemaps  %>% 
  dplyr::select(c("e_s_questionnaire_id", "questionnaireStartedTimestamp", "id", "user_id" , "condition", "valence", "md_valence", "diff_valence", "arousal", "md_arousal", "diff_arousal"),everything())

affect_compare <- affect_compare  %>% 
  dplyr::select(c("e_s_questionnaire_id", "questionnaireStartedTimestamp", "id", "user_id" , "condition", "valence", "md_valence", "diff_valence", "arousal", "md_arousal", "diff_arousal"),everything())

# save dfs 
saveRDS(affect_egemaps, "data/affect_egemaps.RData")
saveRDS(affect_compare, "data/affect_compare.RData")

### DESCRIPTIVES OF MERGED DATA ####

# number of participants
length(unique(affect_egemaps$user_id))

# number of es
length(unique(affect_egemaps$e_s_questionnaire_id))

## FINISH