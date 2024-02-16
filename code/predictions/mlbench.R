### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "parallel", "data.table", "ggplot2", "mlr3", "mlr3learners", "mlr3tuning","ranger", "glmnet", "future", "remotes", "bbotk", "mlr3mbo")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

set.seed(123, kind = "L'Ecuyer") # set seed to make sure all results are reproducible

# load required functions
source("code/functions/bmr_results.R")
source("code/functions/sign_test_folds.R")

### READ IN DATA ####

# study 1
affect_egemaps_study1  <- readRDS("data/study1/affect_egemaps.RData")
affect_compare_study1  <- readRDS("data/study1/affect_compare.RData")

# study 2
affect_egemaps_study2  <- readRDS("data/study2/affect_acoustics.RData")
affect_compare_study2  <- readRDS("data/study2/affect_acoustics.RData")
affect_wordembeddings_study2  <- readRDS("data/study2/affect_wordembeddings.RData")

#### CREATE TASKS: STUDY 1 ####

## benchmark predictions

# age
affect_egemaps_age <- affect_egemaps[!is.na(affect_egemaps$Demo_A1),] # create new df with no missing data for age

egemaps_age = TaskRegr$new(id = "egemaps_age", 
                           backend = affect_egemaps_age[,c(which(colnames(affect_egemaps_age)=="user_id"),
                                                           which(colnames(affect_egemaps_age)=="Demo_A1"),  
                                                           which(colnames(affect_egemaps_age)=="F0semitoneFrom275Hzsma3nzamean"):which(colnames(affect_egemaps_age)=="equivalentSoundLeveldBp"))], 
                           target = "Demo_A1")

# gender
affect_egemaps_gender <- affect_egemaps[!is.na(affect_egemaps$Demo_GE1),] # create new df with no missing data for gender
affect_egemaps_gender$Demo_GE1 <- as.factor(affect_egemaps_gender$Demo_GE1) # convert gender to factor

egemaps_gender = TaskClassif$new(id = "egemaps_gender", 
                                 backend = affect_egemaps_gender[,c(which(colnames(affect_egemaps_gender)=="user_id"),
                                                                    which(colnames(affect_egemaps_gender)=="Demo_GE1"),  
                                                                    which(colnames(affect_egemaps_gender)=="F0semitoneFrom275Hzsma3nzamean"):which(colnames(affect_egemaps_gender)=="equivalentSoundLeveldBp"))], 
                                 target = "Demo_GE1")

## state affect experience

# raw valence score 
egemaps_valence = TaskRegr$new(id = "egemaps_valence", 
                               backend = affect_egemaps[,c(which(colnames(affect_egemaps)=="user_id"), 
                                                           which(colnames(affect_egemaps)=="valence"),  
                                                           which(colnames(affect_egemaps)=="F0semitoneFrom275Hzsma3nzamean"):which(colnames(affect_egemaps)=="equivalentSoundLeveldBp"))], 
                               target = "valence")

# raw arousal score
egemaps_arousal = TaskRegr$new(id = "egemaps_arousal", 
                               backend = affect_egemaps[,c(which(colnames(affect_egemaps)=="user_id"), 
                                                           which(colnames(affect_egemaps)=="arousal"),  
                                                           which(colnames(affect_egemaps)=="F0semitoneFrom275Hzsma3nzamean"):which(colnames(affect_egemaps)=="equivalentSoundLeveldBp"))], 
                               target = "arousal")

## supplementary predictions: fluctuation from baseline

# valence fluctuation from baseline
egemaps_valence_diff = TaskRegr$new(id = "egemaps_valence_diff", 
                                    backend = affect_egemaps[,c(which(colnames(affect_egemaps)=="user_id"), 
                                                                which(colnames(affect_egemaps)=="diff_valence"),  
                                                                which(colnames(affect_egemaps)=="F0semitoneFrom275Hzsma3nzamean"):which(colnames(affect_egemaps)=="equivalentSoundLeveldBp"))], 
                                    target = "diff_valence")

# arousal fluctuation from baseline
egemaps_arousal_diff = TaskRegr$new(id = "egemaps_arousal_diff", 
                                    backend = affect_egemaps[,c(which(colnames(affect_egemaps)=="user_id"), 
                                                                which(colnames(affect_egemaps)=="diff_arousal"),  
                                                                which(colnames(affect_egemaps)=="F0semitoneFrom275Hzsma3nzamean"):which(colnames(affect_egemaps)=="equivalentSoundLeveldBp"))], 
                                    target = "diff_arousal")

## supplementary predictions: compare feature set

# valence
compare_valence = TaskRegr$new(id = "compare_valence", 
                               backend = affect_compare[,c(which(colnames(affect_compare)=="user_id"), 
                                                           which(colnames(affect_compare)=="valence"),  
                                                           which(colnames(affect_compare)=="audspeclengthL1normsmarange"):which(colnames(affect_compare)=="mfccsmade14stddevFallingSlope"))], 
                               target = "valence")

# arousal
compare_arousal = TaskRegr$new(id = "compare_arousal", 
                               backend = affect_compare[,c(which(colnames(affect_compare)=="user_id"), 
                                                           which(colnames(affect_compare)=="arousal"),  
                                                           which(colnames(affect_compare)=="audspeclengthL1normsmarange"):which(colnames(affect_compare)=="mfccsmade14stddevFallingSlope"))], 
                               target = "arousal")

## add blocking

# age
egemaps_age$col_roles$group = "user_id"
egemaps_age$col_roles$feature = setdiff(egemaps_age$col_roles$feature, "user_id")

# gender
egemaps_gender$col_roles$group = "user_id"
egemaps_gender$col_roles$feature = setdiff(egemaps_gender$col_roles$feature, "user_id")

# raw valence score
egemaps_valence$col_roles$group = "user_id"
egemaps_valence$col_roles$feature = setdiff(egemaps_valence$col_roles$feature, "user_id")

# raw arousal score
egemaps_arousal$col_roles$group = "user_id"
egemaps_arousal$col_roles$feature = setdiff(egemaps_arousal$col_roles$feature, "user_id")

# valence difference from baseline
egemaps_valence_diff$col_roles$group = "user_id"
egemaps_valence_diff$col_roles$feature = setdiff(egemaps_valence_diff$col_roles$feature, "user_id")

# arousal difference from baseline
egemaps_arousal_diff$col_roles$group = "user_id"
egemaps_arousal_diff$col_roles$feature = setdiff(egemaps_arousal_diff$col_roles$feature, "user_id")

# valence, compare feature set
compare_valence$col_roles$group = "user_id"
compare_valence$col_roles$feature = setdiff(compare_valence$col_roles$feature, "user_id")

# arousal, compare feature set

# Use Id column as block factor
compare_arousal$col_roles$group = "user_id"
compare_arousal$col_roles$feature = setdiff(compare_arousal$col_roles$feature, "user_id")

#### CREATE TASKS: STUDY 2 ####

## benchmark predictions

# age
egemaps_age = TaskRegr$new(id = "egemaps_age", 
                               backend = affect_acoustics[,c(which(colnames(affect_acoustics)=="user_id"), 
                                                             which(colnames(affect_acoustics)== "Age"), 
                                                             which(colnames(affect_acoustics)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics)=="equivalentSoundLevel_dBp"))], 
                               target = "Age")

# gender
affect_acoustics$Gender <- as.factor(affect_acoustics$Gender) # convert gender to factor

egemaps_gender = TaskClassif$new(id = "egemaps_gender", 
                           backend = affect_acoustics[,c(which(colnames(affect_acoustics)=="user_id"), 
                                                         which(colnames(affect_acoustics)== "Gender"), 
                                                         which(colnames(affect_acoustics)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics)=="equivalentSoundLevel_dBp"))], 
                           target = "Gender")

## egemaps feature set

# raw contentedness score
egemaps_content = TaskRegr$new(id = "egemaps_content", 
                               backend = affect_acoustics[,c(which(colnames(affect_acoustics)=="user_id"), 
                                                             which(colnames(affect_acoustics)== "content"), 
                                                             which(colnames(affect_acoustics)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics)=="equivalentSoundLevel_dBp"))], 
                               target = "content")

# raw sadness score
egemaps_sad = TaskRegr$new(id = "egemaps_sad", 
                           backend = affect_acoustics[,c(which(colnames(affect_acoustics)=="user_id"), 
                                                         which(colnames(affect_acoustics)== "sad"), 
                                                         which(colnames(affect_acoustics)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics)=="equivalentSoundLevel_dBp"))], 
                           target = "sad")

# raw arousal score
egemaps_arousal = TaskRegr$new(id = "egemaps_arousal", 
                           backend = affect_acoustics[,c(which(colnames(affect_acoustics)=="user_id"), 
                                                         which(colnames(affect_acoustics)== "arousal"), 
                                                         which(colnames(affect_acoustics)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics)=="equivalentSoundLevel_dBp"))], 
                           target = "arousal")

## supplementary analyses: fluctuations

# content fluctuation from baseline
egemaps_content_diff = TaskRegr$new(id = "egemaps_content_diff", 
                                    backend = affect_acoustics[,c(which(colnames(affect_acoustics)=="user_id"), 
                                                                  which(colnames(affect_acoustics)== "diff_content"), 
                                                                  which(colnames(affect_acoustics)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics)=="equivalentSoundLevel_dBp"))], 
                                    target = "diff_content")

# sad fluctuation from baseline
egemaps_sad_diff = TaskRegr$new(id = "egemaps_sad_diff", 
                                backend =  affect_acoustics[,c(which(colnames(affect_acoustics)=="user_id"), 
                                                               which(colnames(affect_acoustics)== "diff_sad"), 
                                                               which(colnames(affect_acoustics)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics)=="equivalentSoundLevel_dBp"))], 
                                target = "diff_sad")

# arousal fluctuation from baseline
egemaps_arousal_diff = TaskRegr$new(id = "egemaps_arousal_diff", 
                                backend =  affect_acoustics[,c(which(colnames(affect_acoustics)=="user_id"), 
                                                               which(colnames(affect_acoustics)== "diff_arousal"), 
                                                               which(colnames(affect_acoustics)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics)=="equivalentSoundLevel_dBp"))], 
                                target = "diff_arousal")

## supplementary analyses: compare feature set

# raw contentedness score
compare_content = TaskRegr$new(id = "compare_content", 
                               backend = ema_acoustics_data[,c(which(colnames(ema_acoustics_data)=="user_id"), which(colnames(ema_acoustics_data)== "content"), which(colnames(ema_acoustics_data)=="audspec_lengthL1norm_sma_range"):which(colnames(ema_acoustics_data)=="mfcc_sma_de.14._stddevFallingSlope"))], 
                               target = "content")

# raw sadness score
compare_sad = TaskRegr$new(id = "compare_sad", 
                           backend = ema_acoustics_data[,c(which(colnames(ema_acoustics_data)=="user_id"), which(colnames(ema_acoustics_data)== "sad"), which(colnames(ema_acoustics_data)== "sad_weight"), which(colnames(ema_acoustics_data)=="audspec_lengthL1norm_sma_range"):which(colnames(ema_acoustics_data)=="mfcc_sma_de.14._stddevFallingSlope"))], 
                           target = "sad")

# raw arousal score
compare_arousal = TaskRegr$new(id = "compare_arousal", 
                               backend = ema_acoustics_data[,c(which(colnames(ema_acoustics_data)=="user_id"), which(colnames(ema_acoustics_data)== "arousal"), which(colnames(ema_acoustics_data)=="audspec_lengthL1norm_sma_range"):which(colnames(ema_acoustics_data)=="mfcc_sma_de.14._stddevFallingSlope"))], 
                               target = "arousal")

## word embeddings 

# raw contentedness score
wordembeddings_content = TaskRegr$new(id = "wordembeddings_content", 
                                      backend = affect_wordembeddings[,c(which(colnames(affect_wordembeddings)=="user_id"), which(colnames(affect_wordembeddings)=="content"), which(colnames(affect_wordembeddings)=="Dim1"):which(colnames(affect_wordembeddings)=="Dim1024"))], 
                                      target = "content")

# raw sadness score
wordembeddings_sad = TaskRegr$new(id = "wordembeddings_sad", 
                                  backend = affect_wordembeddings[,c(which(colnames(affect_wordembeddings)=="user_id"), which(colnames(affect_wordembeddings)=="sad"), which(colnames(affect_wordembeddings)=="Dim1"):which(colnames(affect_wordembeddings)=="Dim1024"))], 
                                  target = "sad")

# raw arousal score
wordembeddings_arousal = TaskRegr$new(id = "wordembeddings_arousal", 
                                      backend = affect_wordembeddings[,c(which(colnames(affect_wordembeddings)=="user_id"), which(colnames(affect_wordembeddings)=="arousal"), which(colnames(affect_wordembeddings)=="Dim1"):which(colnames(affect_wordembeddings)=="Dim1024"))], 
                                      target = "arousal")

# supplementary analyses

# content fluctuation from baseline
wordembeddings_content_diff = TaskRegr$new(id = "wordembeddings_content_diff", 
                                           backend = affect_wordembeddings[,c(which(colnames(affect_wordembeddings)=="user_id"), which(colnames(affect_wordembeddings)=="diff_content"), which(colnames(affect_wordembeddings)=="Dim1"):which(colnames(affect_wordembeddings)=="Dim1024"))], 
                                           target = "diff_content")

# sad fluctuation from baseline
wordembeddings_sad_diff = TaskRegr$new(id = "wordembeddings_sad_diff", 
                                       backend = affect_wordembeddings[,c(which(colnames(affect_wordembeddings)=="user_id"), which(colnames(affect_wordembeddings)=="diff_sad"), which(colnames(affect_wordembeddings)=="Dim1"):which(colnames(affect_wordembeddings)=="Dim1024"))], 
                                       target = "diff_sad")

# arousal fluctuation from baseline
wordembeddings_arousal_diff = TaskRegr$new(id = "wordembeddings_arousal_diff", 
                                           backend = affect_wordembeddings[,c(which(colnames(affect_wordembeddings)=="user_id"), which(colnames(affect_wordembeddings)=="diff_arousal"), which(colnames(affect_wordembeddings)=="Dim1"):which(colnames(affect_wordembeddings)=="Dim1024"))], 
                                           target = "diff_arousal")


## add blocking

# age
egemaps_age$col_roles$group = "user_id"
egemaps_age$col_roles$feature = setdiff(egemaps_age$col_roles$feature, "user_id")

# gender
egemaps_gender$col_roles$group = "user_id"
egemaps_gender$col_roles$feature = setdiff(egemaps_gender$col_roles$feature, "user_id")

# raw content score
egemaps_content$col_roles$group = "user_id"
egemaps_content$col_roles$feature = setdiff(egemaps_content$col_roles$feature, "user_id")

# raw sad score
egemaps_sad$col_roles$group = "user_id"
egemaps_sad$col_roles$feature = setdiff(egemaps_sad$col_roles$feature, "user_id")

# raw arousal score
egemaps_arousal$col_roles$group = "user_id"
egemaps_arousal$col_roles$feature = setdiff(egemaps_arousal$col_roles$feature, "user_id")

# content difference from baseline
egemaps_content_diff$col_roles$group = "user_id"
egemaps_content_diff$col_roles$feature = setdiff(egemaps_content_diff$col_roles$feature, "user_id")

# sad difference from baseline
egemaps_sad_diff$col_roles$group = "user_id"
egemaps_sad_diff$col_roles$feature = setdiff(egemaps_sad_diff$col_roles$feature, "user_id")

# arousal difference from baseline
egemaps_arousal_diff$col_roles$group = "user_id"
egemaps_arousal_diff$col_roles$feature = setdiff(egemaps_arousal_diff$col_roles$feature, "user_id")

# raw content score, compare feature set
compare_content$col_roles$group = "user_id"
compare_content$col_roles$feature = setdiff(compare_content$col_roles$feature, "user_id")

# raw sad score, compare feature set
compare_sad$col_roles$group = "user_id"
compare_sad$col_roles$feature = setdiff(compare_sad$col_roles$feature, "user_id")

# raw arousal score, compare feature set
compare_arousal$col_roles$group = "user_id"
compare_arousal$col_roles$feature = setdiff(compare_arousal$col_roles$feature, "user_id")

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

# content difference from baseline
wordembeddings_content_diff$col_roles$group = "user_id"
wordembeddings_content_diff$col_roles$feature = setdiff(wordembeddings_content_diff$col_roles$feature, "user_id")

# sad difference from baseline
wordembeddings_sad_diff$col_roles$group = "user_id"
wordembeddings_sad_diff$col_roles$feature = setdiff(wordembeddings_sad_diff$col_roles$feature, "user_id")

# arousal difference from baseline
wordembeddings_arousal_diff$col_roles$group = "user_id"
wordembeddings_arousal_diff$col_roles$feature = setdiff(wordembeddings_arousal_diff$col_roles$feature, "user_id")

#### CREATE LEARNERS ####

lrn_fl = lrn("regr.featureless")
lrn_rf = lrn("regr.ranger", 
             mtry = to_tune(1, round(length(egemaps_arousal$col_roles$feature)*0.7)),
             num.trees =1000) # random forest
lrn_rr = lrn("regr.cv_glmnet",
             alpha= to_tune(0,1)) # lasso

# enable parallelization
set_threads(lrn_fl, n = detectCores())
set_threads(lrn_rr, n = detectCores())
set_threads(lrn_rf, n = detectCores())

#### HYPERPARAMETER TUNING ####

at_rf = AutoTuner$new(
  learner = lrn_rf,
  resampling = rsmp("cv", folds = 5),
  measure = msr("regr.mse"),
  terminator = trm("evals", n_evals = 50),
  tuner = tnr("random_search"),
  store_models = TRUE
)

at_rr = AutoTuner$new(
  learner = lrn_rr,
  resampling = rsmp("cv", folds = 5),
  measure = msr("regr.mse"),
  terminator = trm("evals", n_evals = 50),
  tuner = tnr("random_search"),
  store_models = TRUE
)

#### RESAMPLING ####

resampling = rsmp("cv", folds = 10L)

#### BENCHMARK: STUDY 1 ####

# avoid console output from mlr3tuning
logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")

# show progress
progressr::handlers(global = TRUE)
progressr::handlers("progress")

# age
bmgrid_egemaps_age = benchmark_grid(
  task = c(egemaps_age),
  learner = list(lrn_fl, at_rf, at_rr),
  resampling = resampling
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_egemaps_age = benchmark(bmgrid_egemaps_age, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_egemaps_age, "results/study1/bmr_egemaps_age.RData") # save results

# gender
bmgrid_egemaps_gender = benchmark_grid(
  task = c(egemaps_gender),
  learner = list(lrn("classif.featureless", predict_type = "prob"), lrn("classif.ranger", num.trees =1000, predict_type = "prob"), lrn("classif.cv_glmnet", predict_type = "prob")),
  resampling = resampling
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_egemaps_gender = benchmark(bmgrid_egemaps_gender, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_egemaps_gender, "results/study1/bmr_egemaps_gender.RData") # save results

## momentary affect experience

bmgrid_egemaps = benchmark_grid(
  task = c(egemaps_valence,
           egemaps_arousal,
           egemaps_valence_diff, # supplementary analyses 
           egemaps_arousal_diff
  ),
  learner = list(lrn_fl, at_rf, at_rr),
  resampling = resampling
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_egemaps = benchmark(bmgrid_egemaps, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_egemaps, "results/study1/bmr_egemaps.RData") # save results

## supplementary analysis: compare feature set

# add pca in preprocessing 
po_preproc = po("pca", param_vals = list(rank. = 88), scale. = T) # extract 88 dimension from pca

# combine training with pre-processing
lrn_rf_at_po = po_preproc %>>% at_rf 
lrn_rr_at_po = po_preproc %>>% at_rr 

bmgrid_compare = benchmark_grid(
  task = c(compare_valence, 
           compare_arousal),
  learner = list(lrn_fl, lrn_rf_at_po,  lrn_rr_at_po),
  resampling = resampling
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_compare_valence = benchmark(bmgrid_compare_valence, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_compare_valence, "results/study1/bmr_compare_valence.RData") # save results


#### BENCHMARK: STUDY 2 ####

# avoid console output from mlr3tuning
logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")

# show progress
progressr::handlers(global = TRUE)
progressr::handlers("progress")

## age
bmgrid_egemaps_age = benchmark_grid(
  task = egemaps_age,
  learner = list(lrn_fl, at_rf, at_rr),
  resampling = resampling
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_egemaps_age = benchmark(bmgrid_egemaps_age, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_egemaps_age, "results/study2/bmr_egemaps_age.RData") # save results

## gender
bmgrid_egemaps_gender = benchmark_grid(
  task = egemaps_gender,
  learner = list(lrn("classif.featureless", predict_type = "prob"), lrn("classif.ranger", num.trees =1000, predict_type = "prob"), lrn("classif.cv_glmnet", predict_type = "prob")),
  resampling = resampling
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_egemaps_gender = benchmark(bmgrid_egemaps_gender, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_egemaps_gender, "results/study2/bmr_egemaps_gender.RData") # save results

## momentary affect experience

bmgrid_egemaps = benchmark_grid(
  task = c(egemaps_arousal,
           egemaps_content,
           egemaps_sad
           ),
  learner = list(lrn_fl, at_rf, at_rr),
  resampling = resampling
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_egemaps = benchmark(bmgrid_egemaps, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_egemaps, "results/study2/bmr_egemaps.RData") # save results

## supplementary analysis: compare feature set

# add pca in preprocessing 
po_preproc = po("pca", param_vals = list(rank. = 88), scale. = T) # extract 88 dimension from pca

# combine training with pre-processing
lrn_rf_at_po = po_preproc %>>% at_rf 
lrn_rr_at_po = po_preproc %>>% at_rr 

bmgrid_compare = benchmark_grid(
  task = c(compare_valence, compare_arousal),
  learner = list(lrn_fl, lrn_rf_at_po,  lrn_rr_at_po),
  resampling = resampling
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_compare = benchmark(bmgrid_compare, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_compare_valence, "results/study2/bmr_compare.RData") # save results

# word embeddings 

bmgrid_wordembeddings = benchmark_grid(
  task = c(wordembeddings_arousal,
           wordembeddings_content,
           wordembeddings_sad
  ),
  learner = list(lrn_fl, at_rf, at_rr),
  resampling = resampling
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_wordembeddings = benchmark(bmgrid_wordembeddings, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_wordembeddings, "results/study2/bmr_wordembeddings.RData") # save results


#### BENCHMARK RESULTS AND SIGNIFICANCE TESTS ####

## read in benchmark results

# study 1
bmr_egemaps_age <- readRDS("results/study1/bmr_egemaps_age.RData")
bmr_egemaps_gender <- readRDS("results/study1/bmr_egemaps_gender.RData")

bmr_egemaps_study1 <- readRDS("results/study1/bmr_egemaps.RData")

# study 2
bmr_age <- readRDS("results/study2/bmr_egemaps_age.RData")
bmr_gender <- readRDS("results/study2/bmr_egemaps_gender.RData")

bmr_egemaps_study2 <- readRDS("results/study2/bmr_egemaps.RData")
bmr_wordembeddings_study2 <- readRDS("results/study2/bmr_wordembeddings.RData")


## retrieve benchmark results across tasks and learners for single cv folds (this is needed for barplots)

bmr_results_folds_egemaps_study1 <- extract_bmr_results(bmr_egemaps, mes)

bmr_results_folds_egemaps_study2 <- extract_bmr_results(bmr_egemaps, mes)
bmr_results_folds_wordembeddings_study2 <- extract_bmr_results(bmr_egemaps, mes)

# create combined overview table of performance incl. significance tests
pred_table <- results_table(affect_egemaps, bmr_results_folds)

# add column with p values
bmr_results_folds <- dplyr::left_join(bmr_results_folds, pred_table[,c("task_id", "learner_id", "p_rsq", "p_rsq_corrected")], by = c("task_id", "learner_id"))

# rbind into one table 

# create significance column
bmr_results_folds$significance <- as.factor(ifelse(bmr_results_folds$p_rsq_corrected >= 0.05 | is.na(bmr_results_folds$p_rsq_corrected), "no", "yes"))


# rename 
bmr_results_folds <- bmr_results_folds %>% 
  mutate(learner_id = case_when(
    learner_id == "regr.featureless" ~    "Baseline",
    learner_id == "regr.ranger" ~ "Random Forest",
    learner_id == "regr.cv_glmnet" ~ "LASSO")) %>% 
  mutate(task_id = case_when(
    task_id == "egemaps_valence" ~  "Valence",
    task_id == "egemaps_arousal" ~ "Arousal"))

# create figure with main results

bmr_overview_plot <- ggplot(bmr_results_folds, aes(x= factor(task_id, levels = c("Arousal Fluctuation","Arousal", "Valence Fluctuation",  "Valence")) , y= regr.rsq, color = significance, shape = learner_id)) + 
  geom_boxplot(width = 0.3,lwd = 1, aes(color = significance), alpha = 0.3, outlier.shape=NA, position=position_dodge(0.5)) +  
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5), size = 3) +
  scale_x_discrete(element_blank()) +
  scale_y_continuous(name = bquote(paste("Out-of-sample ", "R"^2)), limits = c(-0.15, 0.15)) + 
  geom_hline(yintercept=0, linetype='dotted') +
  theme_minimal(base_size = 25) +
  labs(colour = "Significance", shape = "Algorithm") + # change legend title
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) + # rotate x axis labels
  coord_flip() + # flip coordinates
  guides(color = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE)) +
  theme(legend.position="top", legend.key.size = unit(1, "cm"))

bmr_egemaps_plot

# save figure

png(file="figures/bmr_ger_egemaps_plot.png",width=1250, height=750)

bmr_egemaps_plot

dev.off()

# add column with p values
bmr_results_folds <- dplyr::left_join(bmr_results_folds, pred_table[,c("task_id", "learner_id", "p_rsq", "p_rsq_corrected")], by = c("task_id", "learner_id"))

# create significance column
bmr_results_folds$significance <- as.factor(ifelse(bmr_results_folds$p_rsq_corrected >= 0.05 | is.na(bmr_results_folds$p_rsq_corrected), "no", "yes"))

# rename 
bmr_results_folds <- bmr_results_folds %>% 
  mutate(learner_id = case_when(
    learner_id == "regr.featureless" ~    "Baseline",
    learner_id == "regr.ranger" ~ "Random Forest",
    learner_id == "regr.cv_glmnet" ~ "LASSO")) %>% 
  mutate(feature_set = case_when(
    task_id == "egemaps_content" ~    "Voice Acoustics",
    task_id == "egemaps_content_diff" ~ "Voice Acoustics",
    task_id == "egemaps_sad" ~    "Voice Acoustics",
    task_id == "egemaps_sad_diff" ~ "Voice Acoustics",
    task_id == "egemaps_arousal" ~ "Voice Acoustics",
    task_id == "egemaps_arousal_diff" ~ "Voice Acoustics",
    task_id == "wordembeddings_content" ~    "Word Embeddings",
    task_id == "wordembeddings_content_diff" ~ "Word Embeddings",
    task_id == "wordembeddings_sad" ~    "Word Embeddings",
    task_id == "wordembeddings_sad_diff" ~ "Word Embeddings",
    task_id == "wordembeddings_arousal" ~ "Word Embeddings",
    task_id == "wordembeddings_arousal_diff" ~ "Word Embeddings",
    task_id == "egemaps_wordembeddings_content" ~    "All Features",
    task_id == "egemaps_wordembeddings_content_diff" ~ "All Features",
    task_id == "egemaps_wordembeddings_sad" ~    "All Features",
    task_id == "egemaps_wordembeddings_sad_diff" ~ "All Features",
    task_id == "egemaps_wordembeddings_arousal" ~ "All Features",
    task_id == "egemaps_wordembeddings_arousal_diff" ~ "All Features")) %>% 
  mutate(task_id = case_when(
    task_id == "egemaps_content" ~    "Contentedness",
    task_id == "egemaps_content_diff" ~ "Contentedness Fluctuation",
    task_id == "egemaps_sad" ~    "Sadness",
    task_id == "egemaps_sad_diff" ~ "Sadness Fluctuation",
    task_id == "egemaps_arousal" ~ "Arousal",
    task_id == "egemaps_arousal_diff" ~ "Arousal Fluctuation",
    task_id == "wordembeddings_content" ~    "Contentedness",
    task_id == "wordembeddings_content_diff" ~ "Contentedness Fluctuation",
    task_id == "wordembeddings_sad" ~    "Sadness",
    task_id == "wordembeddings_sad_diff" ~ "Sadness Fluctuation",
    task_id == "wordembeddings_arousal" ~ "Arousal",
    task_id == "wordembeddings_arousal_diff" ~ "Arousal Fluctuation",
    task_id == "egemaps_wordembeddings_content" ~    "Contentedness",
    task_id == "egemaps_wordembeddings_content_diff" ~ "Contentedness Fluctuation",
    task_id == "egemaps_wordembeddings_sad" ~    "Sadness",
    task_id == "egemaps_wordembeddings_sad_diff" ~ "Sadness Fluctuation",
    task_id == "egemaps_wordembeddings_arousal" ~ "Arousal",
    task_id == "egemaps_wordembeddings_arousal_diff" ~ "Arousal Fluctuation")) 

# create figure

bmr_plot <- ggplot(bmr_results_folds, aes(x= factor(interaction(task_id, feature_set),
                                                    levels = c("Arousal Fluctuation.Voice Acoustics","Arousal.Voice Acoustics", "Sadness Fluctuation.Voice Acoustics",  "Sadness.Voice Acoustics", "Contentedness Fluctuation.Voice Acoustics",  "Contentedness.Voice Acoustics",
                                                               "Arousal Fluctuation.Word Embeddings","Arousal.Word Embeddings", "Sadness Fluctuation.Word Embeddings",  "Sadness.Word Embeddings", "Contentedness Fluctuation.Word Embeddings",  "Contentedness.Word Embeddings",
                                                               "Arousal Fluctuation.All Features","Arousal.All Features", "Sadness Fluctuation.All Features",  "Sadness.All Features", "Contentedness Fluctuation.All Features",  "Contentedness.All Features")) , 
                                          y= regr.rsq, 
                                          color = significance, 
                                          shape = learner_id)) + 
  geom_boxplot(width = 0.3,lwd = 1, aes(color = significance), alpha = 0.3, outlier.shape=NA, position=position_dodge(0.5)) +  
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5), size = 3) +
  scale_x_discrete(element_blank(), labels = c("Arousal Fluctuation (Voice Acoustics)","Arousal (Voice Acoustics)", "Sadness Fluctuation (Voice Acoustics)",  "Sadness (Voice Acoustics)", "Contentedness Fluctuation (Voice Acoustics)",  "Contentedness (Voice Acoustics)",
                                               "Arousal Fluctuation (Word Embeddings)","Arousal (Word Embeddings)", "Sadness Fluctuation (Word Embeddings)",  "Sadness (Word Embeddings)", "Contentedness Fluctuation (Word Embeddings)",  "Contentedness (Word Embeddings)",
                                               "Arousal Fluctuation (All Features)","Arousal (All Features)", "Sadness Fluctuation (All Features)",  "Sadness (All Features)", "Contentedness Fluctuation (All Features)",  "Contentedness (All Features)")) +
  scale_y_continuous(name = bquote(paste("Out-of-sample ", "R"^2)), limits = c(-0.15, 0.15)) + 
  geom_hline(yintercept=0, linetype='dotted') +
  theme_minimal(base_size = 25) +
  labs(colour = "Significance", shape = "Algorithm") + # change legend title
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) + # rotate x axis labels
  coord_flip() + # flip coordinates
  guides(color = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE)) +
  theme(legend.position="top", legend.key.size = unit(1, "cm"))

# save figure

png(file="figures/bmr_us_egemaps_wordembeddings_plot.png",width=1500, height=2000)

bmr_plot

dev.off()



### FINISH