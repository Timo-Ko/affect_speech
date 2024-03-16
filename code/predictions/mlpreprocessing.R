### PREPARATION ####

packages <- c("dplyr", "caret")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# load function for target independet preprocessing
source("code/functions/target_independent_preproc.R")

# load data

# study 1
affect_voice_study1 <- readRDS(file="data/study1/affect_voice_study1_cleaned.rds") 

# study 2
affect_voice_wordembeddings_study2 <- readRDS(file="data/study2/affect_voice_wordembeddings.rds") 

#### TARGET INDEPENDENT PREPROCESSING ####

# define the columns that are not features (here preprocessing is not applied)
no_feature_columns_study1 = c("e_s_questionnaire_id", "questionnaireStartedTimestamp", "id","user_id", "condition",
                              "Demo_A1", "Demo_GE1", 
                              "valence", "md_valence", "diff_valence", 
                              "arousal", "md_arousal", "diff_arousal")

no_feature_columns_study2 = c("user_id", "timestamp", 
                              "Age", "Gender", 
                              "sad", "md_sad", "diff_sad",
                              "content", "md_content", "diff_content",
                              "arousal", "md_arousal", "diff_arousal",
                              "Total.words" , "File.duration.in.seconds", "Voice.only.duration.in.seconds", "Text", "Sentiment.magnitude", "Sentiment.score")

# apply functions for target-independent preprocessing

# study1
affect_voice_study1_ml <- target_independent_preproc(affect_voice_study1, no_feature_columns_study1)

# study2
affect_voice_wordembeddings_study2_ml <- target_independent_preproc(affect_voice_wordembeddings_study2, no_feature_columns_study2)

# save data

# study1
saveRDS(affect_voice_study1_ml, "data/study1/affect_voice_study1_ml.rds")

# study2
saveRDS(affect_voice_wordembeddings_study2_ml, "data/study2/affect_voice_wordembeddings_study2_ml.rds")

# FINISH