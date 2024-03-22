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
                              "Total.words" , "File.duration.in.seconds", "Voice.only.duration.in.seconds", "Sentiment.magnitude", "Sentiment.score")

# apply functions for target-independent preprocessing (separately to each feature subset since they are used separately in prediction models)

# study1

affect_voice_study1_ml <- affect_voice_study1

affect_voice_study1_ml[, which(colnames(affect_voice_study1_ml) == "F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_study1_ml) == "equivalentSoundLevel_dBp")] <-
target_independent_preproc(affect_voice_study1_ml[, which(colnames(affect_voice_study1_ml) == "F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_study1_ml) == "equivalentSoundLevel_dBp")], no_feature_columns_study1) # egemaps features

affect_voice_study1_ml[, which(colnames(affect_voice_study1_ml) == "audspec_lengthL1norm_sma_range"):which(colnames(affect_voice_study1_ml) == "mfcc_sma_de[14]_stddevFallingSlope")] <-
  target_independent_preproc(affect_voice_study1_ml[, which(colnames(affect_voice_study1_ml) == "audspec_lengthL1norm_sma_range"):which(colnames(affect_voice_study1_ml) == "mfcc_sma_de[14]_stddevFallingSlope")], no_feature_columns_study1) # compare features

# study2

affect_voice_wordembeddings_study2_ml <- affect_voice_wordembeddings_study2

affect_voice_wordembeddings_study2_ml [, which(colnames(affect_voice_wordembeddings_study2_ml ) == "F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_wordembeddings_study2_ml ) == "equivalentSoundLevel_dBp")] <-
  target_independent_preproc(affect_voice_wordembeddings_study2_ml [, which(colnames(affect_voice_wordembeddings_study2_ml ) == "F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_wordembeddings_study2_ml ) == "equivalentSoundLevel_dBp")], no_feature_columns_study2) # egemaps features

affect_voice_wordembeddings_study2_ml [, which(colnames(affect_voice_wordembeddings_study2_ml ) == "audspec_lengthL1norm_sma_range"):which(colnames(affect_voice_wordembeddings_study2_ml ) == "mfcc_sma_de.14._stddevFallingSlope")] <-
  target_independent_preproc(affect_voice_wordembeddings_study2_ml [, which(colnames(affect_voice_wordembeddings_study2_ml ) == "audspec_lengthL1norm_sma_range"):which(colnames(affect_voice_wordembeddings_study2_ml ) == "mfcc_sma_de.14._stddevFallingSlope")], no_feature_columns_study2) # compare features

affect_voice_wordembeddings_study2_ml [, which(colnames(affect_voice_wordembeddings_study2_ml ) == "Dim1_texts"):which(colnames(affect_voice_wordembeddings_study2_ml ) == "Dim1024_texts")] <-
  target_independent_preproc(affect_voice_wordembeddings_study2_ml [, which(colnames(affect_voice_wordembeddings_study2_ml ) == "Dim1_texts"):which(colnames(affect_voice_wordembeddings_study2_ml ) == "Dim1024_texts")], no_feature_columns_study2) # wordembedding features

# save data

# study1
saveRDS(affect_voice_study1_ml, "data/study1/affect_voice_study1_ml.rds")

# study2
saveRDS(affect_voice_wordembeddings_study2_ml, "data/study2/affect_voice_wordembeddings_study2_ml.rds")

# FINISH