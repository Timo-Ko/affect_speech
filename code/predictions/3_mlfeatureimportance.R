### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "data.table", "ranger", "ggplot2", "mlr3verse", "mlr3learners", "parallel", "mlr3tuning", "stringr", "DALEX", "DALEXtra", "iml", "patchwork", "forcats")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

set.seed(123, kind = "L'Ecuyer") # set seed to make sure all results are reproducible

source("code/functions/extract_betas.R")

# study 1
affect_voice_study1  <- readRDS("data/study1/affect_voice_study1_ml.rds")

# remove illegal characters from colnames 
colnames(affect_voice_study1) <- make.names(colnames(affect_voice_study1), unique = TRUE)

# study 2
affect_voice_wordembeddings_study2 <- readRDS("data/study2/affect_voice_wordembeddings_study2_ml.rds")

# remove illegal characters from colnames 
colnames(affect_voice_wordembeddings_study2) <- make.names(colnames(affect_voice_wordembeddings_study2), unique = TRUE)

#### MISSING DATA IMPUTATION ####

## create tasks for missing value imputation

# study 1
egemaps_arousal_study1 = TaskRegr$new(id = "egemaps_arousal", 
                                      backend = affect_voice_study1[,c(which(colnames(affect_voice_study1)=="arousal"),  
                                                                       which(colnames(affect_voice_study1)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_study1)=="equivalentSoundLevel_dBp"))], 
                                      target = "arousal")

# study 2
egemaps_content_study2 = TaskRegr$new(id = "egemaps_content", 
                                      backend = affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)== "content"), 
                                                                                      which(colnames(affect_voice_wordembeddings_study2)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_wordembeddings_study2)=="equivalentSoundLevel_dBp"))], 
                                      target = "content")

egemaps_arousal_study2 = TaskRegr$new(id = "egemaps_arousal", 
                                      backend = affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)== "arousal"), 
                                                                                      which(colnames(affect_voice_wordembeddings_study2)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_wordembeddings_study2)=="equivalentSoundLevel_dBp"))], 
                                      target = "arousal")

# create pipeline to impute missings
po_impute = po("imputehist") 

# Apply the pipeline to the tasks
egemaps_arousal_study1_imp <- po_impute$train(list(egemaps_arousal_study1))[[1]]
egemaps_content_study2_imp <- po_impute$train(list(egemaps_content_study2))[[1]]
egemaps_arousal_study2_imp <- po_impute$train(list(egemaps_arousal_study2))[[1]]

# get imputed data
affect_voice_study1_arousal_imputed = as.data.frame(egemaps_arousal_study1_imp$data())

affect_voice_study2_content_imputed = as.data.frame(egemaps_content_study2_imp$data())
affect_voice_study2_arousal_imputed = as.data.frame(egemaps_arousal_study2_imp$data())

#### TRAIN MODELS ####

# standardize outcome variables bc they are scaled differently!
summary(affect_voice_study1_arousal_imputed$arousal)
summary(affect_voice_study2_arousal_imputed$arousal)
summary(affect_voice_study2_content_imputed$content)

affect_voice_study1_arousal_imputed$arousal_stand <- scale(affect_voice_study1_arousal_imputed$arousal, center = TRUE, scale = TRUE)
affect_voice_study2_arousal_imputed$arousal_stand <- scale(affect_voice_study2_arousal_imputed$arousal, center = TRUE, scale = TRUE)
affect_voice_study2_content_imputed$content_stand <- scale(affect_voice_study2_content_imputed$content, center = TRUE, scale = TRUE)

## create new prediction tasks w imputed data

# study 1
egemaps_arousal_study1_imp = TaskRegr$new(id = "egemaps_arousal_study1_imp", 
                                      backend = subset(affect_voice_study1_arousal_imputed, select = -arousal),
                                      target = "arousal_stand")

# study 2
egemaps_content_study2_imp = TaskRegr$new(id = "egemaps_content_study2_imp", 
                                      backend = subset(affect_voice_study2_content_imputed, select = -content),
                                      target = "content_stand")

egemaps_arousal_study2_imp = TaskRegr$new(id = "egemaps_arousal_study2_imp", 
                                      backend = subset(affect_voice_study2_arousal_imputed, select = -arousal),
                                      target = "arousal_stand")

# create en learners
lrn_rr_arousal_study1 = lrn("regr.cv_glmnet", alpha = 0.5)
lrn_rr_content_study2 = lrn("regr.cv_glmnet", alpha = 0.5)
lrn_rr_arousal_study2 = lrn("regr.cv_glmnet", alpha = 0.5)

# train models

model_rr_egemaps_arousal_study1 <- lrn_rr_arousal_study1$train(egemaps_arousal_study1_imp) # train model
model_rr_egemaps_content_study2 <- lrn_rr_content_study2$train(egemaps_content_study2_imp) # train model
model_rr_egemaps_arousal_study2 <- lrn_rr_arousal_study2$train(egemaps_arousal_study2_imp) # train model

# save trained models
saveRDS(model_rr_egemaps_arousal_study1, "results/study1/model_rr_egemaps_arousal_study1.rds") 
saveRDS(model_rr_egemaps_content_study2, "results/study2/model_rr_egemaps_content_study2.rds") 
saveRDS(model_rr_egemaps_arousal_study2, "results/study2/model_rr_egemaps_arousal_study2.rds") 

#### SINGLE FEATURE IMPORTANCE: PROSODIC FEATURES ####

# load models 

model_rr_egemaps_arousal_study1 <- readRDS( "results/study1/model_rr_egemaps_arousal_study1.rds") 
model_rr_egemaps_content_study2 <- readRDS("results/study2/model_rr_egemaps_content_study2.rds") 
model_rr_egemaps_arousal_study2 <- readRDS( "results/study2/model_rr_egemaps_arousal_study2.rds") 

## get selected features from en model 

model_rr_egemaps_arousal_study1$selected_features()
model_rr_egemaps_content_study2$selected_features()
model_rr_egemaps_arousal_study2$selected_features()

## get beta weights from en model 
coef_arousal_study1 = coef(model_rr_egemaps_arousal_study1$model, s = "lambda.1se") #s because this was the value used for training/evaluation
coef_content_study2 = coef(model_rr_egemaps_content_study2$model, s = "lambda.1se") #s because this was the value used for training/evaluation
coef_arousal_study2 = coef(model_rr_egemaps_arousal_study2$model, s = "lambda.1se") #s because this was the value used for training/evaluation

coefficients_arousal_study1 = extract_beta_coefficients(coef_arousal_study1)[[1]]
coefficients_content_study2 = extract_beta_coefficients(coef_content_study2)[[1]]
coefficients_arousal_study2 = extract_beta_coefficients(coef_arousal_study2)[[1]]

rownames(coefficients_arousal_study1) <- NULL
rownames(coefficients_content_study2) <- NULL
rownames(coefficients_arousal_study2) <- NULL

# save betas
write.csv2(coefficients_arousal_study1, "results/study1/coefficients_arousal_study1.csv")
write.csv2(coefficients_content_study2, "results/study2/coefficients_content_study2.csv")
write.csv2(coefficients_arousal_study2, "results/study2/coefficients_arousal_study2.csv")

# finish
