### PREPARATION ####

packages <- c("dplyr", "caret")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# load function for target independet preprocessing
source("code/functions/target_independent_preproc.R")

# load data

# study 1
affect_voice_study1 <- readRDS(file="data/study1/affect_voice_study1.rds") 

# study 2
affect_egemaps_study2 <- readRDS(file="data/study2/affect_acoustics.RData") 
affect_wordembeddings_study2 <- readRDS(file="data/study2/affect_wordembeddings.RData") 

#### TARGET INDEPENDENT PREPROCESSING ####

# define the columns that are not features (here preprocessing is not applied)
no_feature_columns_study1 = c("e_s_questionnaire_id", "questionnaireStartedTimestamp", "id","user_id", "Demo_A1", "Demo_GE1", 
                              "condition", "valence", "md_valence", "diff_valence", "arousal", "md_arousal", "diff_arousal")

no_feature_columns_study2 = c("user_id", "timestamp", "Age", "Gender", "content", "sad", "arousal",
                              "Total.words" , "File.duration.in.seconds", "Voice.only.duration.in.seconds", "Text", "Sentiment.magnitude", "Sentiment.score")

# apply functions for target-independent preprocessing

# study1
affect_voice_study1_ml <- target_independent_preproc(affect_voice_study1, no_feature_columns_study1)

# study2
affect_egemaps_study2_ml <- target_independent_preproc(affect_speech_study1, no_feature_columns_study2)
affect_compare_study2_ml <- target_independent_preproc(affect_speech_study1, no_feature_columns_study2)
affect_wordembeddings_study2_ml <- target_independent_preproc(affect_speech_study1, no_feature_columns_study2)
affect_egemaps_wordembeddings_study2_ml <- target_independent_preproc(affect_speech_study1, no_feature_columns_study2)

# study2: create combined egemaps + wordembeddings feature set
#affect_egemaps_wordembeddings_study2_ml <- test

# save data

# study1
saveRDS(affect_voice_study1_ml, "data/study1/affect_voice_study1_ml.rds")

# study2
saveRDS(affect_egemaps_study2_ml, "data/study2/affect_speech_study2_ml.rds")
saveRDS(affect_compare_study2_ml, "data/study2/affect_speech_study2_ml.rds")
saveRDS(affect_wordembeddings_study2_ml, "data/study2/affect_speech_study2_ml.rds")
saveRDS(affect_egemaps_wordembeddings_study2_ml, "data/study2/affect_speech_study2_ml.rds")

# FINISH