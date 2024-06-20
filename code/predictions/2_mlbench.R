### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "parallel", "data.table", "ggplot2", "ggtext", "mlr3", "mlr3verse", "mlr3learners", "mlr3tuning","ranger", "glmnet", "future", "remotes", "bbotk", "patchwork")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

set.seed(123, kind = "L'Ecuyer") # set seed to make sure all results are reproducible

# load required functions
source("code/functions/bmr_results.R")
source("code/functions/sign_test_folds.R")

### READ IN DATA ####

# study 1
affect_voice_study1  <- readRDS("data/study1/affect_voice_study1_ml.rds")

# remove illegal characters from colnames 
colnames(affect_voice_study1) <- make.names(colnames(affect_voice_study1), unique = TRUE)

# study 2
affect_voice_wordembeddings_study2 <- readRDS("data/study2/affect_voice_wordembeddings_study2_ml.rds")

# remove illegal characters from colnames 
colnames(affect_voice_wordembeddings_study2) <- make.names(colnames(affect_voice_wordembeddings_study2), unique = TRUE)

#### CREATE TASKS: STUDY 1 ####

## state affect experience

# raw valence score 
egemaps_valence_study1 = TaskRegr$new(id = "egemaps_valence", 
                               backend = affect_voice_study1[,c(which(colnames(affect_voice_study1)=="user_id"), 
                                                           which(colnames(affect_voice_study1)=="valence"),  
                                                           which(colnames(affect_voice_study1)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_study1)=="equivalentSoundLevel_dBp"))], 
                               target = "valence")

# raw arousal score
egemaps_arousal_study1 = TaskRegr$new(id = "egemaps_arousal", 
                               backend = affect_voice_study1[,c(which(colnames(affect_voice_study1)=="user_id"), 
                                                           which(colnames(affect_voice_study1)=="arousal"),  
                                                           which(colnames(affect_voice_study1)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_study1)=="equivalentSoundLevel_dBp"))], 
                               target = "arousal")

## supplementary predictions: fluctuation from baseline

# valence fluctuation from baseline
egemaps_valence_diff_study1 = TaskRegr$new(id = "egemaps_valence_diff", 
                                    backend = affect_voice_study1[,c(which(colnames(affect_voice_study1)=="user_id"), 
                                                                which(colnames(affect_voice_study1)=="diff_valence"),  
                                                                which(colnames(affect_voice_study1)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_study1)=="equivalentSoundLevel_dBp"))], 
                                    target = "diff_valence")

# arousal fluctuation from baseline
egemaps_arousal_diff_study1 = TaskRegr$new(id = "egemaps_arousal_diff", 
                                    backend = affect_voice_study1[,c(which(colnames(affect_voice_study1)=="user_id"), 
                                                                which(colnames(affect_voice_study1)=="diff_arousal"),  
                                                                which(colnames(affect_voice_study1)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_study1)=="equivalentSoundLevel_dBp"))], 
                                    target = "diff_arousal")

## supplementary predictions: compare feature set

# valence
compare_valence_study1 = TaskRegr$new(id = "compare_valence", 
                               backend = affect_voice_study1[,c(which(colnames(affect_voice_study1)=="user_id"), 
                                                           which(colnames(affect_voice_study1)=="valence"),  
                                                           which(colnames(affect_voice_study1)=="audspec_lengthL1norm_sma_range"):which(colnames(affect_voice_study1)=="mfcc_sma_de.14._stddevFallingSlope"))], 
                               target = "valence")

# arousal
compare_arousal_study1 = TaskRegr$new(id = "compare_arousal", 
                               backend = affect_voice_study1[,c(which(colnames(affect_voice_study1)=="user_id"), 
                                                           which(colnames(affect_voice_study1)=="arousal"),  
                                                           which(colnames(affect_voice_study1)=="audspec_lengthL1norm_sma_range"):which(colnames(affect_voice_study1)=="mfcc_sma_de.14._stddevFallingSlope"))], 
                               target = "arousal")


# supplementary prediction: gender

affect_voice_gender_study1 <- affect_voice_study1[!is.na(affect_voice_study1$Demo_GE1),] # create new df with no missing data for gender
affect_voice_gender_study1$Demo_GE1 <- as.factor(affect_voice_gender_study1$Demo_GE1) # convert gender to factor

egemaps_gender_study1 = TaskClassif$new(id = "egemaps_gender_study1", 
                                        backend = affect_voice_gender_study1[,c(which(colnames(affect_voice_gender_study1)=="user_id"),
                                                                           which(colnames(affect_voice_gender_study1)=="Demo_GE1"),  
                                                                           which(colnames(affect_voice_gender_study1)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_gender_study1)=="equivalentSoundLevel_dBp"))], 
                                        target = "Demo_GE1")

## add blocking

# gender
egemaps_gender_study1$col_roles$group = "user_id"
egemaps_gender_study1$col_roles$feature = setdiff(egemaps_gender_study1$col_roles$feature, "user_id")

# raw valence score
egemaps_valence_study1$col_roles$group = "user_id"
egemaps_valence_study1$col_roles$feature = setdiff(egemaps_valence_study1$col_roles$feature, "user_id")

# raw arousal score
egemaps_arousal_study1$col_roles$group = "user_id"
egemaps_arousal_study1$col_roles$feature = setdiff(egemaps_arousal_study1$col_roles$feature, "user_id")

# valence difference from baseline
egemaps_valence_diff_study1$col_roles$group = "user_id"
egemaps_valence_diff_study1$col_roles$feature = setdiff(egemaps_valence_diff_study1$col_roles$feature, "user_id")

# arousal difference from baseline
egemaps_arousal_diff_study1$col_roles$group = "user_id"
egemaps_arousal_diff_study1$col_roles$feature = setdiff(egemaps_arousal_diff_study1$col_roles$feature, "user_id")

# valence, compare feature set
compare_valence_study1$col_roles$group = "user_id"
compare_valence_study1$col_roles$feature = setdiff(compare_valence_study1$col_roles$feature, "user_id")

# arousal, compare feature set
compare_arousal_study1$col_roles$group = "user_id"
compare_arousal_study1$col_roles$feature = setdiff(compare_arousal_study1$col_roles$feature, "user_id")

#### CREATE TASKS: STUDY 2 ####

## benchmark predictions

# supplementary analysis: gender
affect_voice_wordembeddings_gender_study2 <- affect_voice_wordembeddings_study2[!is.na(affect_voice_wordembeddings_study2$Gender),] # create new df with no missing data for gender
affect_voice_wordembeddings_gender_study2$Gender <- as.factor(affect_voice_wordembeddings_gender_study2$Gender) # convert gender to factor

egemaps_gender_study2 = TaskClassif$new(id = "egemaps_gender", 
                           backend = affect_voice_wordembeddings_gender_study2[,c(which(colnames(affect_voice_wordembeddings_gender_study2)=="user_id"), 
                                                         which(colnames(affect_voice_wordembeddings_gender_study2)== "Gender"), 
                                                         which(colnames(affect_voice_wordembeddings_gender_study2)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_wordembeddings_gender_study2)=="equivalentSoundLevel_dBp"))], 
                           target = "Gender")

## egemaps feature set

# raw contentedness score
egemaps_content_study2 = TaskRegr$new(id = "egemaps_content", 
                               backend = affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)=="user_id"), 
                                                             which(colnames(affect_voice_wordembeddings_study2)== "content"), 
                                                             which(colnames(affect_voice_wordembeddings_study2)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_wordembeddings_study2)=="equivalentSoundLevel_dBp"))], 
                               target = "content")

# raw sadness score
egemaps_sad_study2 = TaskRegr$new(id = "egemaps_sad", 
                           backend = affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)=="user_id"), 
                                                         which(colnames(affect_voice_wordembeddings_study2)== "sad"), 
                                                         which(colnames(affect_voice_wordembeddings_study2)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_wordembeddings_study2)=="equivalentSoundLevel_dBp"))], 
                           target = "sad")

# raw arousal score
egemaps_arousal_study2 = TaskRegr$new(id = "egemaps_arousal", 
                           backend = affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)=="user_id"), 
                                                         which(colnames(affect_voice_wordembeddings_study2)== "arousal"), 
                                                         which(colnames(affect_voice_wordembeddings_study2)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_wordembeddings_study2)=="equivalentSoundLevel_dBp"))], 
                           target = "arousal")

## supplementary analyses: fluctuations

# content fluctuation from baseline
egemaps_content_diff_study2 = TaskRegr$new(id = "egemaps_content_diff", 
                                    backend = affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)=="user_id"), 
                                                                  which(colnames(affect_voice_wordembeddings_study2)== "diff_content"), 
                                                                  which(colnames(affect_voice_wordembeddings_study2)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_wordembeddings_study2)=="equivalentSoundLevel_dBp"))], 
                                    target = "diff_content")

# sad fluctuation from baseline
egemaps_sad_diff_study2 = TaskRegr$new(id = "egemaps_sad_diff", 
                                backend =  affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)=="user_id"), 
                                                               which(colnames(affect_voice_wordembeddings_study2)== "diff_sad"), 
                                                               which(colnames(affect_voice_wordembeddings_study2)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_wordembeddings_study2)=="equivalentSoundLevel_dBp"))], 
                                target = "diff_sad")

# arousal fluctuation from baseline
egemaps_arousal_diff_study2 = TaskRegr$new(id = "egemaps_arousal_diff", 
                                backend =  affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)=="user_id"), 
                                                               which(colnames(affect_voice_wordembeddings_study2)== "diff_arousal"), 
                                                               which(colnames(affect_voice_wordembeddings_study2)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_wordembeddings_study2)=="equivalentSoundLevel_dBp"))], 
                                target = "diff_arousal")

## supplementary analyses: compare feature set

# raw contentedness score
compare_content_study2 = TaskRegr$new(id = "compare_content", 
                               backend = affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)=="user_id"), 
                                                                  which(colnames(affect_voice_wordembeddings_study2)== "content"), 
                                                                  which(colnames(affect_voice_wordembeddings_study2)=="audspec_lengthL1norm_sma_range"):which(colnames(affect_voice_wordembeddings_study2)=="mfcc_sma_de.14._stddevFallingSlope"))], 
                               target = "content")

# raw sadness score
compare_sad_study2 = TaskRegr$new(id = "compare_sad", 
                           backend = affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)=="user_id"), 
                                                              which(colnames(affect_voice_wordembeddings_study2)== "sad"), 
                                                              which(colnames(affect_voice_wordembeddings_study2)== "audspec_lengthL1norm_sma_range"), which(colnames(affect_voice_wordembeddings_study2)=="audspec_lengthL1norm_sma_range"):which(colnames(affect_voice_wordembeddings_study2)=="mfcc_sma_de.14._stddevFallingSlope"))], 
                           target = "sad")

# raw arousal score
compare_arousal_study2 = TaskRegr$new(id = "compare_arousal", 
                               backend = affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)=="user_id"), 
                                                                               which(colnames(affect_voice_wordembeddings_study2)== "arousal"), 
                                                                               which(colnames(affect_voice_wordembeddings_study2)=="audspec_lengthL1norm_sma_range"):which(colnames(affect_voice_wordembeddings_study2)=="mfcc_sma_de.14._stddevFallingSlope"))], 
                               target = "arousal")

## word embeddings 

# raw contentedness score
wordembeddings_content = TaskRegr$new(id = "wordembeddings_content", 
                                      backend = affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)=="user_id"), 
                                                                                      which(colnames(affect_voice_wordembeddings_study2)=="content"), 
                                                                                      which(colnames(affect_voice_wordembeddings_study2)=="Dim1_texts"):which(colnames(affect_voice_wordembeddings_study2)=="Dim1024_texts"))], 
                                      target = "content")

# raw sadness score
wordembeddings_sad = TaskRegr$new(id = "wordembeddings_sad", 
                                  backend = affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)=="user_id"), 
                                                                                  which(colnames(affect_voice_wordembeddings_study2)=="sad"), 
                                                                                  which(colnames(affect_voice_wordembeddings_study2)=="Dim1_texts"):which(colnames(affect_voice_wordembeddings_study2)=="Dim1024_texts"))], 
                                  target = "sad")

# raw arousal score
wordembeddings_arousal = TaskRegr$new(id = "wordembeddings_arousal", 
                                      backend = affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)=="user_id"), 
                                                                                      which(colnames(affect_voice_wordembeddings_study2)=="arousal"), 
                                                                                      which(colnames(affect_voice_wordembeddings_study2)=="Dim1_texts"):which(colnames(affect_voice_wordembeddings_study2)=="Dim1024_texts"))], 
                                      target = "arousal")


## egemaps + word embeddings 

# raw contentedness score
egemaps_wordembeddings_content = TaskRegr$new(id = "egemaps_wordembeddings_content", 
                                      backend = affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)=="user_id"), 
                                                                                      which(colnames(affect_voice_wordembeddings_study2)=="content"), 
                                                                                      which(colnames(affect_voice_wordembeddings_study2)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_wordembeddings_study2)=="equivalentSoundLevel_dBp"), 
                                                                                      which(colnames(affect_voice_wordembeddings_study2)=="Dim1_texts"):which(colnames(affect_voice_wordembeddings_study2)=="Dim1024_texts"))], 
                                      target = "content")

# raw sadness score
egemaps_wordembeddings_sad = TaskRegr$new(id = "egemaps_wordembeddings_sad", 
                                  backend = affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)=="user_id"), 
                                                                                  which(colnames(affect_voice_wordembeddings_study2)=="sad"), 
                                                                                  which(colnames(affect_voice_wordembeddings_study2)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_wordembeddings_study2)=="equivalentSoundLevel_dBp"), 
                                                                                  which(colnames(affect_voice_wordembeddings_study2)=="Dim1_texts"):which(colnames(affect_voice_wordembeddings_study2)=="Dim1024_texts"))], 
                                  target = "sad")

# raw arousal score
egemaps_wordembeddings_arousal = TaskRegr$new(id = "egemaps_wordembeddings_arousal", 
                                      backend = affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)=="user_id"), 
                                                                                      which(colnames(affect_voice_wordembeddings_study2)=="arousal"),
                                                                                      which(colnames(affect_voice_wordembeddings_study2)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_wordembeddings_study2)=="equivalentSoundLevel_dBp"), 
                                                                                      which(colnames(affect_voice_wordembeddings_study2)=="Dim1_texts"):which(colnames(affect_voice_wordembeddings_study2)=="Dim1024_texts"))], 
                                      target = "arousal")

## add blocking

# gender
egemaps_gender_study2$col_roles$group = "user_id"
egemaps_gender_study2$col_roles$feature = setdiff(egemaps_gender_study2$col_roles$feature, "user_id")

# raw content score
egemaps_content_study2$col_roles$group = "user_id"
egemaps_content_study2$col_roles$feature = setdiff(egemaps_content_study2$col_roles$feature, "user_id")

# raw sad score
egemaps_sad_study2$col_roles$group = "user_id"
egemaps_sad_study2$col_roles$feature = setdiff(egemaps_sad_study2$col_roles$feature, "user_id")

# raw arousal score
egemaps_arousal_study2$col_roles$group = "user_id"
egemaps_arousal_study2$col_roles$feature = setdiff(egemaps_arousal_study2$col_roles$feature, "user_id")

# content difference from baseline
egemaps_content_diff_study2$col_roles$group = "user_id"
egemaps_content_diff_study2$col_roles$feature = setdiff(egemaps_content_diff_study2$col_roles$feature, "user_id")

# sad difference from baseline
egemaps_sad_diff_study2$col_roles$group = "user_id"
egemaps_sad_diff_study2$col_roles$feature = setdiff(egemaps_sad_diff_study2$col_roles$feature, "user_id")

# arousal difference from baseline
egemaps_arousal_diff_study2$col_roles$group = "user_id"
egemaps_arousal_diff_study2$col_roles$feature = setdiff(egemaps_arousal_diff_study2$col_roles$feature, "user_id")

# raw content score, compare feature set
compare_content_study2$col_roles$group = "user_id"
compare_content_study2$col_roles$feature = setdiff(compare_content_study2$col_roles$feature, "user_id")

# raw sad score, compare feature set
compare_sad_study2$col_roles$group = "user_id"
compare_sad_study2$col_roles$feature = setdiff(compare_sad_study2$col_roles$feature, "user_id")

# raw arousal score, compare feature set
compare_arousal_study2$col_roles$group = "user_id"
compare_arousal_study2$col_roles$feature = setdiff(compare_arousal_study2$col_roles$feature, "user_id")

# word embeddings 

# raw content 
wordembeddings_content$col_roles$group = "user_id"
wordembeddings_content$col_roles$feature = setdiff(wordembeddings_content$col_roles$feature, "user_id")

# raw sad score
wordembeddings_sad$col_roles$group = "user_id"
wordembeddings_sad$col_roles$feature = setdiff(wordembeddings_sad$col_roles$feature, "user_id")

# raw arousal score
wordembeddings_arousal$col_roles$group = "user_id"
wordembeddings_arousal$col_roles$feature = setdiff(wordembeddings_arousal$col_roles$feature, "user_id")

# egemaps + word embeddings 

# raw content 
egemaps_wordembeddings_content$col_roles$group = "user_id"
egemaps_wordembeddings_content$col_roles$feature = setdiff(egemaps_wordembeddings_content$col_roles$feature, "user_id")

# raw sad score
egemaps_wordembeddings_sad$col_roles$group = "user_id"
egemaps_wordembeddings_sad$col_roles$feature = setdiff(egemaps_wordembeddings_sad$col_roles$feature, "user_id")

# raw arousal score
egemaps_wordembeddings_arousal$col_roles$group = "user_id"
egemaps_wordembeddings_arousal$col_roles$feature = setdiff(egemaps_wordembeddings_arousal$col_roles$feature, "user_id")

#### CREATE LEARNERS ####

lrn_fl = lrn("regr.featureless")
lrn_rf = lrn("regr.ranger", 
             num.trees =1000) # random forest
lrn_rr = lrn("regr.cv_glmnet"#,
             ) # lasso

### PREPROCESSING IN CV ####

po_impute_hist = po("imputehist") # hist imputation
po_impute_oor = po("imputeoor") # out of range imputation

# combine training with pre-processing
lrn_rf_po = po_impute_oor  %>>% lrn_rf
lrn_rr_po =  po_impute_hist  %>>% lrn_rr

#### RESAMPLING ####

# repeated cv
repeated_cv = rsmp("repeated_cv", repeats = 5, folds = 10)

# # custom cv causes issues w the blocking!!
# 
# # create function for cv folds
# create_repeated_cv_resampling = function(n_rows, n_folds, n_repeats) {
#   results = list(train_sets = list(), test_sets = list())
#   
#   for (repeat_index in 1:n_repeats) {
#     # Shuffle indices
#     shuffled_indices = sample(n_rows)
#     
#     # Determine fold sizes accounting for remainder
#     fold_sizes = rep(n_rows %/% n_folds, n_folds)
#     fold_sizes[1:(n_rows %% n_folds)] = fold_sizes[1:(n_rows %% n_folds)] + 1
#     
#     # Create fold assignments
#     folds = split(shuffled_indices, rep(1:n_folds, fold_sizes))
#     
#     # Create training and testing sets for each fold
#     for (fold_index in seq_along(folds)) {
#       test_indices = folds[[fold_index]]
#       train_indices = unlist(folds[-fold_index], use.names = FALSE)
#       
#       results$train_sets[[length(results$train_sets) + 1]] = train_indices
#       results$test_sets[[length(results$test_sets) + 1]] = test_indices
#     }
#   }
#   
#   return(results)
# }
# 
# # study 1
# 
# cv_sets_study1 = create_repeated_cv_resampling(nrow(affect_voice_study1), 10, 5)
# train_sets_study1 = cv_sets_study1$train_sets
# test_sets_study1 = cv_sets_study1$test_sets
# 
# # Create resampling objects
# custom_cv = rsmp("custom")
# 
# res_study1 = custom_cv$clone()
# res_study1$instantiate(egemaps_valence_study1, train_sets_study1, test_sets_study1)
# # 
# # res_egemaps_arousal_study1 = custom_cv$clone()
# # res_egemaps_arousal_study1$instantiate(egemaps_arousal_study1, train_sets_study1, test_sets_study1)
# # 
# # res_egemaps_valence_diff_study1 = custom_cv$clone()
# # res_egemaps_valence_diff_study1$instantiate(egemaps_valence_diff_study1, train_sets_study1, test_sets_study1)
# # 
# # res_egemaps_arousal_diff_study1 = custom_cv$clone()
# # res_egemaps_arousal_diff_study1$instantiate(egemaps_arousal_diff_study1, train_sets_study1, test_sets_study1)
# # 
# # res_compare_valence_study1 = custom_cv$clone()
# # res_compare_valence_study1$instantiate(compare_valence_study1, train_sets_study1, test_sets_study1)
# # 
# # res_compare_arousal_study1 = custom_cv$clone()
# # res_compare_arousal_study1$instantiate(compare_arousal_study1, train_sets_study1, test_sets_study1)
# # 
# # res_compare_arousal_study1 = custom_cv$clone()
# # res_compare_arousal_study1$instantiate(compare_arousal_study1, train_sets_study1, test_sets_study1)
# 
# cv_sets_gender_study1 = create_repeated_cv_resampling(nrow(affect_voice_gender_study1), 10, 5)
# train_sets_gender_study1 = cv_sets_gender_study1$train_sets
# test_sets_gender_study1 = cv_sets_gender_study1$test_sets
# 
# res_egemaps_gender_study1 = custom_cv$clone()
# res_egemaps_gender_study1$instantiate(egemaps_gender_study1, train_sets_gender_study1, test_sets_gender_study1)
# 
# # study 2
# 
# cv_sets_study2 = create_repeated_cv_resampling(nrow(affect_voice_wordembeddings_study2), 10, 5)
# train_sets_study2 = cv_sets_study2$train_sets
# test_sets_study2 = cv_sets_study2$test_sets
# 
# # Create resampling objects
# custom_cv = rsmp("custom")
# 
# res_egemaps_arousal_study2 = custom_cv$clone()
# res_egemaps_arousal_study2$instantiate(egemaps_arousal_study2, train_sets_study2, test_sets_study2)
# 
# res_egemaps_content_study2 = custom_cv$clone()
# res_egemaps_content_study2$instantiate(egemaps_content_study2, train_sets_study2, test_sets_study2)
# 
# res_egemaps_sad_study2 = custom_cv$clone()
# res_egemaps_sad_study2$instantiate(egemaps_sad_study2, train_sets_study2, test_sets_study2)
# 
# res_egemaps_arousal_diff_study2 = custom_cv$clone()
# res_egemaps_arousal_diff_study2$instantiate(egemaps_arousal_diff_study2, train_sets_study2, test_sets_study2)
# 
# res_egemaps_content_diff_study2 = custom_cv$clone()
# res_egemaps_content_diff_study2$instantiate(egemaps_content_diff_study2, train_sets_study2, test_sets_study2)
# 
# res_egemaps_sad_diff_study2 = custom_cv$clone()
# res_egemaps_sad_diff_study2$instantiate(egemaps_sad_diff_study2, train_sets_study2, test_sets_study2)
# 
# res_compare_arousal_study2 = custom_cv$clone()
# res_compare_arousal_study2$instantiate(compare_arousal_study2, train_sets_study2, test_sets_study2)
# 
# res_compare_content_study2 = custom_cv$clone()
# res_compare_content_study2$instantiate(compare_content_study2, train_sets_study2, test_sets_study2)
# 
# res_compare_sad_study2 = custom_cv$clone()
# res_compare_sad_study2$instantiate(compare_sad_study2, train_sets_study2, test_sets_study2)
# 
# res_compare_arousal_study2 = custom_cv$clone()
# res_compare_arousal_study2$instantiate(compare_arousal_study2, train_sets_study2, test_sets_study2)
# 
# res_wordembeddings_content = custom_cv$clone()
# res_wordembeddings_content$instantiate(wordembeddings_content, train_sets_study2, test_sets_study2)
# 
# res_wordembeddings_sad = custom_cv$clone()
# res_wordembeddings_sad$instantiate(wordembeddings_sad, train_sets_study2, test_sets_study2)
# 
# res_wordembeddings_arousal = custom_cv$clone()
# res_wordembeddings_arousal$instantiate(wordembeddings_arousal, train_sets_study2, test_sets_study2)
# 
# res_egemaps_wordembeddings_content = custom_cv$clone()
# res_egemaps_wordembeddings_content$instantiate(egemaps_wordembeddings_content, train_sets_study2, test_sets_study2)
# 
# res_egemaps_wordembeddings_sad = custom_cv$clone()
# res_egemaps_wordembeddings_sad$instantiate(egemaps_wordembeddings_sad, train_sets_study2, test_sets_study2)
# 
# res_egemaps_wordembeddings_arousal = custom_cv$clone()
# res_egemaps_wordembeddings_arousal$instantiate(egemaps_wordembeddings_arousal, train_sets_study2, test_sets_study2)
# 
# cv_sets_gender_study2 = create_repeated_cv_resampling(nrow(affect_voice_wordembeddings_gender_study2), 10, 5)
# train_sets_gender_study2 = cv_sets_gender_study2$train_sets
# test_sets_gender_study2 = cv_sets_gender_study2$test_sets
# 
# res_egemaps_gender_study2 = custom_cv$clone()
# res_egemaps_gender_study2$instantiate(egemaps_gender_study2, train_sets_gender_study2, test_sets_gender_study2)

#### BENCHMARK: STUDY 1 ####

# avoid console output from mlr3tuning
logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")

# show progress
progressr::handlers(global = TRUE)
progressr::handlers("progress")

## momentary affect experience

bmgrid_study1 = benchmark_grid(
  task = list(egemaps_valence_study1,
           egemaps_arousal_study1,
           egemaps_valence_diff_study1, # supplementary analyses: affect fluctuation
           egemaps_arousal_diff_study1,
           compare_valence_study1, # supplementary analyses: compare2016 feature set
           compare_arousal_study1
  ),
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = repeated_cv
)

future::plan("multisession", workers = 5) # enable parallelization

bmr_study1 = benchmark(bmgrid_study1, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_study1, "results/study1/bmr_study1.rds") # save results

# supplementary prediction: gender

# create classification learner 
lrn_classif_rf_po = po_impute_oor  %>>% lrn("classif.ranger", num.trees =1000, predict_type = "prob")
lrn_classif_rr_po = po_impute_hist  %>>% lrn("classif.cv_glmnet", predict_type = "prob")

bmgrid_egemaps_gender_study1 = benchmark_grid(
  task = c(egemaps_gender_study1),
  learner = list(lrn("classif.featureless", predict_type = "prob"), lrn_classif_rf_po, lrn_classif_rr_po ),
  resampling = repeated_cv
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_egemaps_gender_study1 = benchmark(bmgrid_egemaps_gender_study1, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_egemaps_gender_study1, "results/study1/bmr_egemaps_gender_study1.rds") # save results

#### BENCHMARK: STUDY 2 ####

# avoid console output from mlr3tuning
logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")

# show progress
progressr::handlers(global = TRUE)
progressr::handlers("progress")

## momentary affect experience

bmgrid_study2 = benchmark_grid(
  task = c(egemaps_arousal_study2,
           egemaps_content_study2,
           egemaps_sad_study2, 
           egemaps_arousal_diff_study2,  # supplementary analyses: affect fluctuation
           egemaps_content_diff_study2,
           egemaps_sad_diff_study2,
           compare_arousal_study2, # supplementary analyses: compare2016 feature set
           compare_content_study2,
           compare_sad_study2,
           wordembeddings_arousal, # wordembeddings only
           wordembeddings_content,
           wordembeddings_sad, 
           egemaps_wordembeddings_arousal, # wordembeddings and voice combined
           egemaps_wordembeddings_content,
           egemaps_wordembeddings_sad
           ),
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = repeated_cv
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_study2 = benchmark(bmgrid_study2, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_study2, "results/study2/bmr_study2.rds") # save results


## supplementary prediction: gender
bmgrid_egemaps_gender_study2 = benchmark_grid(
  task = egemaps_gender_study2,
  learner = list(lrn("classif.featureless", predict_type = "prob"), lrn_classif_rf_po,lrn_classif_rr_po ),
  resampling = repeated_cv
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_egemaps_gender_study2 = benchmark(bmgrid_egemaps_gender_study2, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_egemaps_gender_study2, "results/study2/bmr_egemaps_gender_study2.rds") # save results

#### DEEP DIVE BENCHMARK RESULTS AND SIGNIFICANCE TESTS ####

## read in benchmark results

# study 1
bmr_study1 <- readRDS("results/study1/bmr_study1.rds")

bmr_egemaps_gender_study1 <- readRDS("results/study1/bmr_egemaps_gender_study1.rds")

# study 2
bmr_study2 <- readRDS("results/study2/bmr_study2.rds")

bmr_egemaps_gender_study2 <- readRDS("results/study2/bmr_egemaps_gender_study2.rds")

## view aggregated performance 

## modify rho to handle NAs and aggregate to the median 
md_rho = msr("regr.srho")
md_rho$aggregator = function(x) median(x, na.rm = TRUE)

md_rsq = msr("regr.rsq")
md_rsq$aggregator = function(x) median(x, na.rm = TRUE)

md_rmse = msr("regr.rmse")
md_rmse$aggregator = function(x) median(x, na.rm = TRUE)

md_mae = msr("regr.mae")
md_mae$aggregator = function(x) median(x, na.rm = TRUE)

mes = c(md_rho, md_rsq, md_rmse, md_mae) # set performance measures

# study 1

bmr_study1$aggregate(mes)

bmr_egemaps_gender_study1$aggregate(msr("classif.acc"))

# study 2
bmr_study2$aggregate(mes)

bmr_egemaps_gender_study2$aggregate(msr("classif.acc"))

## deep dive: retrieve benchmark results across tasks and learners for single cv folds (this is needed for barplots)

# study 1
bmr_results_folds_study1 <- extract_bmr_results(bmr_study1, mes)

# study 2
bmr_results_folds_study2 <- extract_bmr_results(bmr_study2, mes)

## create combined overview table of performance incl. significance tests
pred_table_study1 <- results_table(affect_voice_study1, bmr_results_folds_study1)

pred_table_study2 <- results_table(affect_voice_wordembeddings_study2, bmr_results_folds_study2)

# save prediction tables
write.csv2(pred_table_study1, "results/pred_table_study1.csv")
write.csv2(pred_table_study2, "results/pred_table_study2.csv")

# correct p values for multiple comparison for main analyses 
p_adj <- p.adjust(c(
  pred_table_study1[pred_table_study1$task_id == "egemaps_valence" & pred_table_study1$learner_id == "imputeoor.regr.ranger", "p_rsq"],
  pred_table_study1[pred_table_study1$task_id == "egemaps_arousal" & pred_table_study1$learner_id == "imputeoor.regr.ranger", "p_rsq"],
  pred_table_study1[pred_table_study1$task_id == "egemaps_valence" & pred_table_study1$learner_id == "imputehist.regr.cv_glmnet", "p_rsq"],
  pred_table_study1[pred_table_study1$task_id == "egemaps_arousal" & pred_table_study1$learner_id == "imputehist.regr.cv_glmnet", "p_rsq"],
  pred_table_study2[pred_table_study2$task_id == "egemaps_content" & pred_table_study2$learner_id == "imputeoor.regr.ranger", "p_rsq"],
  pred_table_study2[pred_table_study2$task_id == "egemaps_sad" & pred_table_study2$learner_id == "imputeoor.regr.ranger", "p_rsq"],
  pred_table_study2[pred_table_study2$task_id == "egemaps_arousal" & pred_table_study2$learner_id == "imputeoor.regr.ranger", "p_rsq"],
  pred_table_study2[pred_table_study2$task_id == "egemaps_content" & pred_table_study2$learner_id == "imputehist.regr.cv_glmnet", "p_rsq"],
  pred_table_study2[pred_table_study2$task_id == "egemaps_sad" & pred_table_study2$learner_id == "imputehist.regr.cv_glmnet", "p_rsq"],
  pred_table_study2[pred_table_study2$task_id == "egemaps_arousal" & pred_table_study2$learner_id == "imputehist.regr.cv_glmnet", "p_rsq"],
  pred_table_study2[pred_table_study2$task_id == "wordembeddings_content" & pred_table_study2$learner_id == "imputeoor.regr.ranger", "p_rsq"],
  pred_table_study2[pred_table_study2$task_id == "wordembeddings_sad" & pred_table_study2$learner_id == "imputeoor.regr.ranger", "p_rsq"],
  pred_table_study2[pred_table_study2$task_id == "wordembeddings_arousal" & pred_table_study2$learner_id == "imputeoor.regr.ranger", "p_rsq"],
  pred_table_study2[pred_table_study2$task_id == "wordembeddings_content" & pred_table_study2$learner_id == "imputehist.regr.cv_glmnet", "p_rsq"],
  pred_table_study2[pred_table_study2$task_id == "wordembeddings_sad" & pred_table_study2$learner_id == "imputehist.regr.cv_glmnet", "p_rsq"],
  pred_table_study2[pred_table_study2$task_id == "wordembeddings_arousal" & pred_table_study2$learner_id == "imputehist.regr.cv_glmnet", "p_rsq"] 
  ),
  method = "holm")

# subset relevant tasks for plotting 
bmr_results_folds_study1_plot <- bmr_results_folds_study1 %>% filter(task_id %in% c("egemaps_valence", "egemaps_arousal")) # only keep relevant main tasks 

bmr_results_folds_study2_plot <- bmr_results_folds_study2 %>% filter(task_id %in% c("egemaps_content", "egemaps_sad", "egemaps_arousal",
                                                                               "wordembeddings_content", "wordembeddings_sad", "wordembeddings_arousal",
                                                                               "egemaps_wordembeddings_content", "egemaps_wordembeddings_sad", "egemaps_wordembeddings_arousal"))  # only keep relevant main tasks 

# rename

bmr_results_folds_study1_plot <- bmr_results_folds_study1_plot  %>% 
  mutate(study = "study1") %>% 
  mutate(learner_id = case_when(
    learner_id == "regr.featureless" ~    "Baseline",
    learner_id == "imputeoor.regr.ranger" ~ "Random Forest",
    learner_id == "imputehist.regr.cv_glmnet" ~ "LASSO")) %>% 
  mutate(feature_set = case_when(
    task_id == "egemaps_valence" ~    "Voice Acoustics",
    task_id == "egemaps_arousal" ~    "Voice Acoustics")) %>% 
  mutate(task_id = case_when(
    task_id == "egemaps_valence" ~    "Valence",
    task_id == "egemaps_arousal" ~ "Arousal"))

bmr_results_folds_study2_plot <- bmr_results_folds_study2_plot %>% 
  filter(!task_id %in% c("egemaps_arousal_diff", "egemaps_content_diff", "egemaps_sad_diff",
                      "wordembeddings_arousal_diff", "wordembeddings_content_diff", "wordembeddings_sad_diff",
                      "egemaps_wordembeddings_arousal_diff", "egemaps_wordembeddings_content_diff", "egemaps_wordembeddings_sad_diff",
                      "egemaps_wordembeddings_arousal", "egemaps_wordembeddings_content", "egemaps_wordembeddings_sad")) %>% 
  mutate(study = "study2") %>% 
  mutate(learner_id = case_when(
    learner_id == "regr.featureless" ~    "Baseline",
    learner_id == "imputeoor.regr.ranger" ~ "Random Forest",
    learner_id == "imputehist.regr.cv_glmnet" ~ "LASSO")) %>% 
  mutate(feature_set = case_when(
    task_id == "egemaps_content" ~    "Voice Acoustics",
    task_id == "egemaps_sad" ~    "Voice Acoustics",
    task_id == "egemaps_arousal" ~ "Voice Acoustics",
    task_id == "wordembeddings_content" ~    "Word Embeddings",
    task_id == "wordembeddings_sad" ~    "Word Embeddings",
    task_id == "wordembeddings_arousal" ~ "Word Embeddings")) %>% 
  mutate(task_id = case_when(
    task_id == "egemaps_content" ~    "Contentedness",
    task_id == "egemaps_sad" ~    "Sadness",
    task_id == "egemaps_arousal" ~ "Arousal",
    task_id == "wordembeddings_content" ~    "Contentedness",
    task_id == "wordembeddings_sad" ~    "Sadness",
    task_id == "wordembeddings_arousal" ~ "Arousal")) %>% 
  mutate(regr.srho = if_else(learner_id == "LASSO" & is.na(regr.srho), 0, regr.srho)) # replace NA with zero for LASSO 

# rbind both studies
bmr_results_folds <- rbind(bmr_results_folds_study1_plot, bmr_results_folds_study2_plot)

# create four figures with main results - two columns for performance measures and separated by study (study 1 on top then study 2 below), Pearson r on the left and r2 on the right, sign pred in bold

bmr_plot_srho <-
  ggplot(
    bmr_results_folds,
    aes(
      x = factor(
        interaction(task_id, feature_set, study),
        levels = rev(c( # order factor
          "Valence.Voice Acoustics.study1",
          "Arousal.Voice Acoustics.study1",
          "Contentedness.Voice Acoustics.study2",
          "Sadness.Voice Acoustics.study2",
          "Arousal.Voice Acoustics.study2",
          "Contentedness.Word Embeddings.study2",
          "Sadness.Word Embeddings.study2",
          "Arousal.Word Embeddings.study2"
        )
        )) ,
      y = regr.srho,
      color = learner_id,
      shape = learner_id)
  ) +
  geom_boxplot(
    width = 0.3,
    lwd = 1,
    aes(color = learner_id),
    alpha = 0.3,
    position = position_dodge(0.5)
  ) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5),
             size = 3, alpha = 0.5) +
  scale_x_discrete(
    element_blank(),
    labels = rev(c(
      "Valence (Prosody)",
      "**Arousal (Prosody)**",
      "**Contentedness (Prosody)**",
      "Sadness (Prosody)",
      "**Arousal (Prosody)**",
      "**Contentedness (Semantics)**",
      "**Sadness (Semantics)**",
      "**Arousal (Semantics)**"
    )
    )) +  
  scale_color_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a"), name = "Algorithm") +
  scale_shape_manual(values = c(16, 17, 18), name = "Algorithm") +
  scale_y_continuous(name = bquote("Spearman correlation (r)"), limits = c(-0.1, 0.5)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  theme_minimal(base_size = 25) +
  labs(colour = "Algorithm") + # change legend title
  theme(axis.text.x = element_text(angle = -45, hjust = 0)) + # rotate x axis labels
  coord_flip() + # flip coordinates
  guides(color = guide_legend(reverse = TRUE),
         shape = guide_legend(reverse = TRUE)) +
  theme(axis.text.x = element_markdown(), 
        axis.text.y = element_markdown(hjust = 0),
        legend.position = "top", 
        legend.key.size = unit(0.5, "cm"))


# plot rsq
bmr_plot_rsq <-
  ggplot(bmr_results_folds, aes(
    x = factor(
      interaction(task_id, feature_set, study),
      levels = rev(c(
        "Valence.Voice Acoustics.study1",
        "Arousal.Voice Acoustics.study1",
        "Contentedness.Voice Acoustics.study2",
        "Sadness.Voice Acoustics.study2",
        "Arousal.Voice Acoustics.study2",
        "Contentedness.Word Embeddings.study2",
        "Sadness.Word Embeddings.study2",
        "Arousal.Word Embeddings.study2"
      ))
    ),
    y = regr.rsq,
    color = learner_id,
    shape = learner_id # This maps both color and shape to learner_id
  )) +
  geom_boxplot(
    width = 0.3,
    lwd = 1,
    aes(color = learner_id),
    alpha = 0.3,
    position = position_dodge(0.5)
  ) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5), size = 3, alpha = 0.5) +
  scale_color_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a"), name = "Algorithm") +
  scale_shape_manual(values = c(16, 17, 18), name = "Algorithm") +
  scale_y_continuous(name = bquote(paste("R" ^ 2)), limits = c(-0.15, 0.15)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  theme_minimal(base_size = 25) +
  labs(colour = "Algorithm") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0), axis.text.y = element_blank()) +
  coord_flip() +
  guides(color = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE)) +
  theme(legend.position = "top", 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(0.5, "cm"))

# use patchwork to combine plots for spearman r and rsq into one figure

get_legend <- function(myplot) {
  # Extract legends
  tmp <- ggplotGrob(myplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


# Extract the legend
legend <- get_legend(bmr_plot_rsq)

legend2 <- get_legend(bmr_plot_srho)

bmr_plot_srho <- bmr_plot_srho + theme(legend.position = "none") # remove legend 

bmr_plot_rsq <- bmr_plot_rsq + theme(legend.position = "none") # remove legend 

# arrange plots
bmr_plot <- bmr_plot_srho + bmr_plot_rsq + plot_layout(guides = "collect") & theme(legend.position = "top")

# save figure

png(file="figures/bmr_plot.png",width=1500, height=1500)

bmr_plot

dev.off()

### FINISH