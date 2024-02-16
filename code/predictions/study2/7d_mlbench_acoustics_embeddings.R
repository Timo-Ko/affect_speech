### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "parallel", "data.table", "ggplot2", "mlr3", "mlr3learners", "mlr3tuning","ranger", "glmnet", "future", "remotes")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

set.seed(123, kind = "L'Ecuyer") # set seed to make sure all results are reproducible

### READ IN DATA ####

# read in data frame
affect_acoustics_wordembeddings <- readRDS("data/affect_acoustics_wordembeddings.RData")

# load required functions
source("r_code/functions/sign_test_folds.R")
source("r_code/functions/bmr_results.R")

#### CREATE TASKS - VOICE ACOUSTICS ONLY  ####

## benchmark check predictions

affect_acoustics_wordembeddings_age <- affect_acoustics_wordembeddings[!is.na(affect_acoustics_wordembeddings$Age),] # create new df with no missing data for age

# age
egemaps_age = TaskRegr$new(id = "egemaps_age", 
                           backend = affect_acoustics_wordembeddings_age[,c(which(colnames(affect_acoustics_wordembeddings_age)=="user_id"), 
                                                         which(colnames(affect_acoustics_wordembeddings_age)== "Age"), 
                                                         which(colnames(affect_acoustics_wordembeddings_age)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics_wordembeddings_age)=="equivalentSoundLevel_dBp"))], 
                           target = "Age")

# gender
affect_acoustics_wordembeddings_gender <- affect_acoustics_wordembeddings[!is.na(affect_acoustics_wordembeddings$Gender),] # create new df with no missing data for age
affect_acoustics_wordembeddings_gender$Gender <- as.factor(affect_acoustics_wordembeddings_gender$Gender) # convert gender to factor

egemaps_gender = TaskClassif$new(id = "egemaps_gender", 
                                 backend = affect_acoustics_wordembeddings_gender[,c(which(colnames(affect_acoustics_wordembeddings_gender)=="user_id"), 
                                                               which(colnames(affect_acoustics_wordembeddings_gender)== "Gender"), 
                                                               which(colnames(affect_acoustics_wordembeddings_gender)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics_wordembeddings_gender)=="equivalentSoundLevel_dBp"))], 
                                 target = "Gender")

## momentary affect experience

# raw contentedness score
egemaps_content = TaskRegr$new(id = "egemaps_content", 
                               backend = affect_acoustics_wordembeddings[,c(which(colnames(affect_acoustics_wordembeddings)=="user_id"), 
                                                             which(colnames(affect_acoustics_wordembeddings)== "content"), 
                                                             which(colnames(affect_acoustics_wordembeddings)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics_wordembeddings)=="equivalentSoundLevel_dBp"))], 
                               target = "content")

# raw sadness score
egemaps_sad = TaskRegr$new(id = "egemaps_sad", 
                           backend = affect_acoustics_wordembeddings[,c(which(colnames(affect_acoustics_wordembeddings)=="user_id"), 
                                                         which(colnames(affect_acoustics_wordembeddings)== "sad"), 
                                                         which(colnames(affect_acoustics_wordembeddings)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics_wordembeddings)=="equivalentSoundLevel_dBp"))], 
                           target = "sad")

# raw arousal score
egemaps_arousal = TaskRegr$new(id = "egemaps_arousal", 
                               backend = affect_acoustics_wordembeddings[,c(which(colnames(affect_acoustics_wordembeddings)=="user_id"), 
                                                             which(colnames(affect_acoustics_wordembeddings)== "arousal"), 
                                                             which(colnames(affect_acoustics_wordembeddings)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics_wordembeddings)=="equivalentSoundLevel_dBp"))], 
                               target = "arousal")

# content fluctuation from baseline
egemaps_content_diff = TaskRegr$new(id = "egemaps_content_diff", 
                                    backend = affect_acoustics_wordembeddings[,c(which(colnames(affect_acoustics_wordembeddings)=="user_id"), 
                                                                  which(colnames(affect_acoustics_wordembeddings)== "diff_content"), 
                                                                  which(colnames(affect_acoustics_wordembeddings)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics_wordembeddings)=="equivalentSoundLevel_dBp"))], 
                                    target = "diff_content")

# sad fluctuation from baseline
egemaps_sad_diff = TaskRegr$new(id = "egemaps_sad_diff", 
                                backend =  affect_acoustics_wordembeddings[,c(which(colnames(affect_acoustics_wordembeddings)=="user_id"), 
                                                               which(colnames(affect_acoustics_wordembeddings)== "diff_sad"), 
                                                               which(colnames(affect_acoustics_wordembeddings)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics_wordembeddings)=="equivalentSoundLevel_dBp"))], 
                                target = "diff_sad")

# arousal fluctuation from baseline
egemaps_arousal_diff = TaskRegr$new(id = "egemaps_arousal_diff", 
                                    backend =  affect_acoustics_wordembeddings[,c(which(colnames(affect_acoustics_wordembeddings)=="user_id"), 
                                                                   which(colnames(affect_acoustics_wordembeddings)== "diff_arousal"), 
                                                                   which(colnames(affect_acoustics_wordembeddings)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics_wordembeddings)=="equivalentSoundLevel_dBp"))], 
                                    target = "diff_arousal")


## add blocking

# age
# Use participant id column as block factor
egemaps_age$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_age$col_roles$feature = setdiff(egemaps_age$col_roles$feature, "user_id")

# gender
# Use participant id column as block factor
egemaps_gender$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_gender$col_roles$feature = setdiff(egemaps_gender$col_roles$feature, "user_id")

# raw content score
# Use participant id column as block factor
egemaps_content$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_content$col_roles$feature = setdiff(egemaps_content$col_roles$feature, "user_id")

# raw sad score
# Use Id column as block factor
egemaps_sad$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_sad$col_roles$feature = setdiff(egemaps_sad$col_roles$feature, "user_id")

# raw arousal score
# Use Id column as block factor
egemaps_arousal$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_arousal$col_roles$feature = setdiff(egemaps_arousal$col_roles$feature, "user_id")

# content difference from baseline
# Use participant id column as block factor
egemaps_content_diff$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_content_diff$col_roles$feature = setdiff(egemaps_content_diff$col_roles$feature, "user_id")

# sad difference from baseline
# Use Id column as block factor
egemaps_sad_diff$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_sad_diff$col_roles$feature = setdiff(egemaps_sad_diff$col_roles$feature, "user_id")

# arousal difference from baseline
# Use Id column as block factor
egemaps_arousal_diff$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_arousal_diff$col_roles$feature = setdiff(egemaps_arousal_diff$col_roles$feature, "user_id")


#### CREATE TASKS - WORDEMBEDDINGS ONLY  ####

## benchmark check predictions

# age
wordembeddings_age = TaskRegr$new(id = "wordembeddings_age", 
                           backend = affect_acoustics_wordembeddings_age[,c(which(colnames(affect_acoustics_wordembeddings_age)=="user_id"), 
                                                                        which(colnames(affect_acoustics_wordembeddings_age)== "Age"), 
                                                                        which(colnames(affect_acoustics_wordembeddings_age)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics_wordembeddings_age)=="equivalentSoundLevel_dBp"))], 
                           target = "Age")

# gender

wordembeddings_gender = TaskClassif$new(id = "wordembeddings_gender", 
                                 backend = affect_acoustics_wordembeddings_gender[,c(which(colnames(affect_acoustics_wordembeddings_gender)=="user_id"), 
                                                                              which(colnames(affect_acoustics_wordembeddings_gender)== "Gender"), 
                                                                              which(colnames(affect_acoustics_wordembeddings_gender)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics_wordembeddings_gender)=="equivalentSoundLevel_dBp"))], 
                                 target = "Gender")

# momentary affect experience

# raw contentedness score
wordembeddings_content = TaskRegr$new(id = "wordembeddings_content", 
                                      backend = affect_acoustics_wordembeddings[,c(which(colnames(affect_acoustics_wordembeddings)=="user_id"), which(colnames(affect_acoustics_wordembeddings)=="content"), which(colnames(affect_acoustics_wordembeddings)=="Dim1"):which(colnames(affect_acoustics_wordembeddings)=="Dim1024"))], 
                                      target = "content")

# raw sadness score
wordembeddings_sad = TaskRegr$new(id = "wordembeddings_sad", 
                                  backend = affect_acoustics_wordembeddings[,c(which(colnames(affect_acoustics_wordembeddings)=="user_id"), which(colnames(affect_acoustics_wordembeddings)=="sad"), which(colnames(affect_acoustics_wordembeddings)=="Dim1"):which(colnames(affect_acoustics_wordembeddings)=="Dim1024"))], 
                                  target = "sad")

# raw arousal score
wordembeddings_arousal = TaskRegr$new(id = "wordembeddings_arousal", 
                                      backend = affect_acoustics_wordembeddings[,c(which(colnames(affect_acoustics_wordembeddings)=="user_id"), which(colnames(affect_acoustics_wordembeddings)=="arousal"), which(colnames(affect_acoustics_wordembeddings)=="Dim1"):which(colnames(affect_acoustics_wordembeddings)=="Dim1024"))], 
                                      target = "arousal")

# content fluctuation from baseline
wordembeddings_content_diff = TaskRegr$new(id = "wordembeddings_content_diff", 
                                           backend = affect_acoustics_wordembeddings[,c(which(colnames(affect_acoustics_wordembeddings)=="user_id"), which(colnames(affect_acoustics_wordembeddings)=="diff_content"), which(colnames(affect_acoustics_wordembeddings)=="Dim1"):which(colnames(affect_acoustics_wordembeddings)=="Dim1024"))], 
                                           target = "diff_content")

# sad fluctuation from baseline
wordembeddings_sad_diff = TaskRegr$new(id = "wordembeddings_sad_diff", 
                                       backend = affect_acoustics_wordembeddings[,c(which(colnames(affect_acoustics_wordembeddings)=="user_id"), which(colnames(affect_acoustics_wordembeddings)=="diff_sad"), which(colnames(affect_acoustics_wordembeddings)=="Dim1"):which(colnames(affect_acoustics_wordembeddings)=="Dim1024"))], 
                                       target = "diff_sad")

# arousal fluctuation from baseline
wordembeddings_arousal_diff = TaskRegr$new(id = "wordembeddings_arousal_diff", 
                                           backend = affect_acoustics_wordembeddings[,c(which(colnames(affect_acoustics_wordembeddings)=="user_id"), which(colnames(affect_acoustics_wordembeddings)=="diff_arousal"), which(colnames(affect_acoustics_wordembeddings)=="Dim1"):which(colnames(affect_acoustics_wordembeddings)=="Dim1024"))], 
                                           target = "diff_arousal")

## add blocking

# age
# Use participant id column as block factor
wordembeddings_age$col_roles$group = "user_id"

# Remove Id from feature space
wordembeddings_age$col_roles$feature = setdiff(wordembeddings_age$col_roles$feature, "user_id")

# gender
# Use participant id column as block factor
wordembeddings_gender$col_roles$group = "user_id"

# Remove Id from feature space
wordembeddings_gender$col_roles$feature = setdiff(wordembeddings_gender$col_roles$feature, "user_id")


# raw content score
# Use participant id column as block factor
wordembeddings_content$col_roles$group = "user_id"

# Remove Id from feature space
wordembeddings_content$col_roles$feature = setdiff(wordembeddings_content$col_roles$feature, "user_id")

# raw sad score
# Use Id column as block factor
wordembeddings_sad$col_roles$group = "user_id"

# Remove Id from feature space
wordembeddings_sad$col_roles$feature = setdiff(wordembeddings_sad$col_roles$feature, "user_id")

# raw arousal score
# Use Id column as block factor
wordembeddings_arousal$col_roles$group = "user_id"

# Remove Id from feature space
wordembeddings_arousal$col_roles$feature = setdiff(wordembeddings_arousal$col_roles$feature, "user_id")

# content difference from baseline
# Use participant id column as block factor
wordembeddings_content_diff$col_roles$group = "user_id"

# Remove Id from feature space
wordembeddings_content_diff$col_roles$feature = setdiff(wordembeddings_content_diff$col_roles$feature, "user_id")

# sad difference from baseline
# Use Id column as block factor
wordembeddings_sad_diff$col_roles$group = "user_id"

# Remove Id from feature space
wordembeddings_sad_diff$col_roles$feature = setdiff(wordembeddings_sad_diff$col_roles$feature, "user_id")

# arousal difference from baseline
# Use Id column as block factor
wordembeddings_arousal_diff$col_roles$group = "user_id"

# Remove Id from feature space
wordembeddings_arousal_diff$col_roles$feature = setdiff(wordembeddings_arousal_diff$col_roles$feature, "user_id")


#### CREATE TASKS - ALL FEATURES  ####

## benchmark check predictions

# age
egemaps_wordembeddings_age = TaskRegr$new(id = "egemaps_wordembeddings_age", 
                           backend = affect_acoustics_wordembeddings_age[,c(which(colnames(affect_acoustics_wordembeddings_age)=="user_id"), 
                                                         which(colnames(affect_acoustics_wordembeddings_age)== "Age"), 
                                                         which(colnames(affect_acoustics_wordembeddings_age)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics_wordembeddings_age)=="equivalentSoundLevel_dBp"))], 
                           target = "Age")

# gender

egemaps_wordembeddings_gender = TaskClassif$new(id = "egemaps_wordembeddings_gender", 
                                 backend = affect_acoustics_wordembeddings_gender[,c(which(colnames(affect_acoustics_wordembeddings_gender)=="user_id"), 
                                                               which(colnames(affect_acoustics_wordembeddings_gender)== "Gender"), 
                                                               which(colnames(affect_acoustics_wordembeddings_gender)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics_wordembeddings_gender)=="equivalentSoundLevel_dBp"))], 
                                 target = "Gender")

## state affect experience


# raw contentedness score
egemaps_wordembeddings_content = TaskRegr$new(id = "egemaps_wordembeddings_content", 
                               backend = affect_acoustics_wordembeddings[,c(which(colnames(affect_acoustics_wordembeddings)=="user_id"), 
                                                                            which(colnames(affect_acoustics_wordembeddings)== "content"), 
                                                                            which(colnames(affect_acoustics_wordembeddings)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics_wordembeddings)=="Dim1024"))], 
                               target = "content")

# raw sadness score
egemaps_wordembeddings_sad = TaskRegr$new(id = "egemaps_wordembeddings_sad", 
                           backend = affect_acoustics_wordembeddings[,c(which(colnames(affect_acoustics_wordembeddings)=="user_id"), 
                                                                        which(colnames(affect_acoustics_wordembeddings)== "sad"), 
                                                                        which(colnames(affect_acoustics_wordembeddings)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics_wordembeddings)=="Dim1024"))], 
                           target = "sad")

# raw arousal score
egemaps_wordembeddings_arousal = TaskRegr$new(id = "egemaps_wordembeddings_arousal", 
                               backend = affect_acoustics_wordembeddings[,c(which(colnames(affect_acoustics_wordembeddings)=="user_id"), 
                                                                            which(colnames(affect_acoustics_wordembeddings)== "arousal"), 
                                                                            which(colnames(affect_acoustics_wordembeddings)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics_wordembeddings)=="Dim1024"))], 
                               target = "arousal")

# content fluctuation from baseline
egemaps_wordembeddings_content_diff = TaskRegr$new(id = "egemaps_wordembeddings_content_diff", 
                                    backend = affect_acoustics_wordembeddings[,c(which(colnames(affect_acoustics_wordembeddings)=="user_id"), 
                                                                                 which(colnames(affect_acoustics_wordembeddings)== "diff_content"), 
                                                                                 which(colnames(affect_acoustics_wordembeddings)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics_wordembeddings)=="Dim1024"))],  
                                    target = "diff_content")

# sad fluctuation from baseline
egemaps_wordembeddings_sad_diff = TaskRegr$new(id = "egemaps_wordembeddings_sad_diff", 
                                backend =  affect_acoustics_wordembeddings[,c(which(colnames(affect_acoustics_wordembeddings)=="user_id"), 
                                                                              which(colnames(affect_acoustics_wordembeddings)== "diff_sad"), 
                                                                              which(colnames(affect_acoustics_wordembeddings)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics_wordembeddings)=="Dim1024"))],  
                                target = "diff_sad")

# arousal fluctuation from baseline
egemaps_wordembeddings_arousal_diff = TaskRegr$new(id = "egemaps_wordembeddings_arousal_diff", 
                                    backend =  affect_acoustics_wordembeddings[,c(which(colnames(affect_acoustics_wordembeddings)=="user_id"), 
                                                                                  which(colnames(affect_acoustics_wordembeddings)== "diff_arousal"), 
                                                                                  which(colnames(affect_acoustics_wordembeddings)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics_wordembeddings)=="Dim1024"))],  
                                    target = "diff_arousal")

## add blocking

# age
# Use participant id column as block factor
egemaps_wordembeddings_age$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_wordembeddings_age$col_roles$feature = setdiff(egemaps_age$col_roles$feature, "user_id")

# gender
# Use participant id column as block factor
egemaps_wordembeddings_gender$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_wordembeddings_gender$col_roles$feature = setdiff(egemaps_wordembeddings_gender$col_roles$feature, "user_id")

# raw content score
# Use participant id column as block factor
egemaps_wordembeddings_content$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_wordembeddings_content$col_roles$feature = setdiff(egemaps_wordembeddings_content$col_roles$feature, "user_id")

# raw sad score
# Use Id column as block factor
egemaps_wordembeddings_sad$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_wordembeddings_sad$col_roles$feature = setdiff(egemaps_wordembeddings_sad$col_roles$feature, "user_id")

# raw arousal score
# Use Id column as block factor
egemaps_wordembeddings_arousal$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_wordembeddings_arousal$col_roles$feature = setdiff(egemaps_wordembeddings_arousal$col_roles$feature, "user_id")

# content difference from baseline
# Use participant id column as block factor
egemaps_wordembeddings_content_diff$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_wordembeddings_content_diff$col_roles$feature = setdiff(egemaps_wordembeddings_content_diff$col_roles$feature, "user_id")

# sad difference from baseline
# Use Id column as block factor
egemaps_wordembeddings_sad_diff$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_wordembeddings_sad_diff$col_roles$feature = setdiff(egemaps_wordembeddings_sad_diff$col_roles$feature, "user_id")

# arousal difference from baseline
# Use Id column as block factor
egemaps_wordembeddings_arousal_diff$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_wordembeddings_arousal_diff$col_roles$feature = setdiff(egemaps_wordembeddings_arousal_diff$col_roles$feature, "user_id")


#### CREATE LEARNERS ####

lrn_fl = lrn("regr.featureless")
lrn_rf = lrn("regr.ranger", num.trees =1000) # random forest
lrn_rr = lrn("regr.cv_glmnet") # lasso

# enable parallelization
set_threads(lrn_fl, n = detectCores())
set_threads(lrn_rr, n = detectCores())
set_threads(lrn_rf, n = detectCores())

#### RESAMPLING ####

resampling = rsmp("cv", folds = 10L)

#### SET PERFORMANCE MEASURES ####

# measures for benchmark
mes = msrs(c("regr.rsq", "regr.srho"))

#### BENCHMARK ####

# avoid console output from mlr3tuning
logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")

# show progress
progressr::handlers(global = TRUE)
progressr::handlers("progress")

## age
bmgrid_age = benchmark_grid(
  task = c(egemaps_age,
           wordembeddings_age,
           egemaps_wordembeddings_age),
  learner = list(lrn_fl, lrn_rf, lrn_rr),
  resampling = resampling
)

future::plan("multisession", workers = detectCores()) # enable parallelization

bmr_age = benchmark(bmgrid_age, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_age, "results/bmr_age.RData") # save results

## gender
bmgrid_gender = benchmark_grid(
  task = c(egemaps_gender,
           wordembeddings_gender,
           egemaps_wordembeddings_gender),
  learner = list(lrn("classif.featureless", predict_type = "prob"), lrn("classif.ranger", num.trees =1000, predict_type = "prob"), lrn("classif.cv_glmnet", predict_type = "prob")),
  resampling = resampling
)

future::plan("multisession", workers = detectCores()) # enable parallelization

bmr_gender = benchmark(bmgrid_gender, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_gender, "results/bmr_gender.RData") # save results

## momentary affect experience

bmgrid_egemaps_wordembeddings = benchmark_grid(
  task = c(egemaps_content,
           egemaps_content_diff,
           egemaps_sad,
           egemaps_sad_diff,
           egemaps_arousal,
           egemaps_arousal_diff,
           wordembeddings_content,
           wordembeddings_content_diff,
           wordembeddings_sad,
           wordembeddings_sad_diff,
           wordembeddings_arousal,
           wordembeddings_arousal_diff,
           egemaps_wordembeddings_content,
           egemaps_wordembeddings_content_diff,
           egemaps_wordembeddings_sad,
           egemaps_wordembeddings_sad_diff,
           egemaps_wordembeddings_arousal,
           egemaps_wordembeddings_arousal_diff),
  learner = list(lrn_fl, lrn_rf, lrn_rr),
  resampling = resampling
)

future::plan("multisession", workers = detectCores()) # enable parallelization

bmr_egemaps_wordembeddings = benchmark(bmgrid_egemaps_wordembeddings, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_egemaps_wordembeddings, "results/bmr_egemaps_wordembeddings.RData") # save results

#### BENCHMARK RESULTS AND SIGNIFICANCE TESTS ####

# read in benchmark results

bmr_age <- readRDS("results/bmr_age.RData")
bmr_gender <- readRDS("results/bmr_gender.RData")
bmr_egemaps_wordembeddings  <- readRDS("results/bmr_egemaps_wordembeddings.RData")

## view aggregated performance
bmr_age$aggregate(mes)
bmr_gender$aggregate(msrs(c("classif.acc", "classif.auc")))
bmr_egemaps_wordembeddings$aggregate(mes)

## retrieve benchmark results across tasks and learners for single cv folds (this is needed for barplots)
bmr_results_folds <- extract_bmr_results(bmr_egemaps_wordembeddings, mes)

# create overview table of performance incl. significance tests
pred_table <- results_table(affect_acoustics_wordembeddings, bmr_results_folds)


### FINISH