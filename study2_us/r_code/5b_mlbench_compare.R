### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "data.table", "xgboost", "ggplot2", "mlr3", "mlr3learners", "mlr3tuning","ranger", "glmnet", "future", "e1071")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

### READ IN DATA ####

# read in data frames
affect_compare  <- readRDS("data/affect_compare.RData")

#### CREATE TASKS ####

## compare feature set

# valence
compare_valence = TaskRegr$new(id = "compare_valence", backend = affect_compare[,c(4, 6, 9:ncol(affect_compare))], target = "valence")

# arousal
compare_arousal = TaskRegr$new(id = "compare_arousal", backend = affect_compare[,c(4, 7, 9:ncol(affect_compare))], target = "arousal")

# add blocking

# valence
# Use participant id column as block factor
compare_valence$col_roles$group = "user_id"

# Remove Id from feature space
compare_valence$col_roles$feature = setdiff(compare_valence$col_roles$feature, "user_id")


# arousal

# Use Id column as block factor
compare_arousal$col_roles$group = "user_id"

# Remove Id from feature space
compare_arousal$col_roles$feature = setdiff(compare_arousal$col_roles$feature, "user_id")


#### CREATE LEARNERS ####

lrn_fl = lrn("regr.featureless")
lrn_rf = lrn("regr.ranger", num.trees =1000)
lrn_en = lrn("regr.cv_glmnet")
# lrn_xgb = mlr_learners$get("regr.xgboost")
# lrn_svm = mlr_learners$get("regr.svm")

# set parameters

ps_en <- ParamSet$new(list(
  ParamDbl$new("alpha", lower = 0, upper = 1)
))


ps_rf <- ParamSet$new(list(
  ParamInt$new("mtry", lower = 1, upper = round(sqrt((compare_arousal$ncol))))
))

# ps_svm <- ParamSet$new(list(
#   ParamDbl$new("cost", lower = 0.01, upper = 10),
#   ParamDbl$new("gamma", lower = 0.01, upper = 10),
#   ))
# 
# 
# ps_xgboost <- ParamSet$new(list(
#   ParamInt$new("nrounds", lower = 1, upper = 500),
#   ParamDbl$new("eta", lower = 0.001, upper = 0.01),
#   ParamDbl$new("lambda", lower = 0.001, upper = 0.1),
#   ParamDbl$new("colsample_bytree", lower = 0.5, upper = 1),
#   ParamInt$new("max_depth", lower = 1, upper = 10)
# ))


# enable parallelization
set_threads(lrn_fl, n = 4)
set_threads(lrn_en, n = 4)
set_threads(lrn_rf, n = 4)
set_threads(lrn_xgb, n = 4)
set_threads(lrn_svm, n = 4)

#### RESAMPLING ####

resampling_outer = rsmp("cv", folds = 10L)
resampling_inner = rsmp("cv", folds = 5L)

#### SET PERFORMANCE MEASURES ####

# measures for benchmark
mes = msrs(c("regr.mae","regr.srho", "regr.rsq"))

# measure for tuning
mes_tuning = msr("regr.mae")

#### PARAMETER TUNING ####

# define tuning
terminator <- trm("evals", n_evals = 10)
tuner <- tnr("random_search")

# create autotuners

at_en <- AutoTuner$new(
  learner = lrn_en,
  resampling = resampling_inner,
  measure = mes_tuning,
  search_space = ps_en,
  terminator = terminator,
  tuner = tuner)

at_rf <- AutoTuner$new(
  learner = lrn_rf,
  resampling = resampling_inner,
  measure = mes_tuning,
  search_space = ps_rf,
  terminator = terminator,
  tuner = tuner)

# at_xgb <- AutoTuner$new(
#   learner = lrn_xgb,
#   resampling = resampling_inner,
#   measure = mes_tuning,
#   search_space = ps_xgboost,
#   terminator = terminator,
#   tuner = tuner)
# 
# at_svm <- AutoTuner$new(
#   learner = lrn_svm,
#   resampling = resampling_inner,
#   measure = mes_tuning,
#   search_space = ps_svm,
#   terminator = terminator,
#   tuner = tuner)


#### BENCHMARK ####

# avoid console output from mlr3tuning
logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")

future::plan("multisession", workers = 4)

## valence
bmgrid_compare_valence = benchmark_grid(
  task = compare_valence,
  learner = list(lrn_fl, at_rf, at_en),
  resampling = resampling_outer
)

bmr_compare_valence = benchmark(bmgrid_compare_valence) # execute the benchmark

# save results
saveRDS(bmr_compare_valence, "results/bmr_compare_valence.RData")

## arousal

future::plan("multisession", workers = 4)

bmgrid_compare_arousal = benchmark_grid(
  task = compare_arousal,
  learner = list(lrn_fl, at_rf, at_en),
  resampling = resampling_outer
)

bmr_compare_arousal = benchmark(bmgrid_compare_arousal) # execute the benchmark

# save results
saveRDS(bmr_compare_arousal, "results/bmr_compare_arousal.RData")

#### BENCHMARK RESULTS AND SIGNIFICANCE TESTS ####

# read in benchmark results

bmr_compare_valence <- readRDS("results/bmr_compare_valence.RData")
bmr_compare_arousal <- readRDS("results/bmr_compare_arousal.RData")

# view aggregated performance measures
mes_arousal = bmr_compare_arousal$aggregate(mes)

mes_arousal

mes_valence = bmr_compare_valence$aggregate(mes)

mes_valence


# create function for significance tests

sign_test_folds = function(x1, x2, n, split = 9/10) { #9/10 split because of 10-fold cross-validation
  J = length(x1)
  N_train = n * split
  N_test = n * (1-split)
  d = x1 - x2
  m = mean(d)
  v = var(d)
  
  t = m / sqrt(v * (1/J + N_test/N_train))
  df = J - 1
  
  # two.sided p-value
  2*pt(abs(t), df, lower.tail = FALSE)
}

n = nrow(affect_compare)


# get performance of learners across cv folds

bmr_results_folds_valence <- bmr_compare_valence$score(
  measures = mes,
  ids = TRUE,
  conditions = FALSE,
  predict_sets = "test"
)

bmr_results_folds_arousal <- bmr_compare_arousal$score(
  measures = mes,
  ids = TRUE,
  conditions = FALSE,
  predict_sets = "test"
)

# get fl results

fl_folds_valence <- bmr_results_folds_valence %>% 
  filter(learner_id == "regr.featureless")

fl_folds_arousal <- bmr_results_folds_arousal %>% 
  filter(learner_id == "regr.featureless")

# get rf results

rf_folds_valence <- bmr_results_folds_valence %>% 
  filter(learner_id == "regr.ranger.tuned")

rf_folds_arousal <- bmr_results_folds_arousal %>% 
  filter(learner_id == "regr.ranger.tuned")

# get en results

en_folds_valence <- bmr_results_folds_valence %>% 
  filter(learner_id == "regr.cv_glmnet.tuned")

en_folds_arousal <- bmr_results_folds_arousal %>% 
  filter(learner_id == "regr.cv_glmnet.tuned")

# # get xgb results
# 
# xgb_folds_valence <- bmr_results_folds_valence %>% 
#   filter(learner_id == "regr.xgboost")
# 
# xgb_folds_arousal <- bmr_results_folds_arousal %>% 
#   filter(learner_id == "regr.xgboost")
# 
# # get svm results
# 
# svm_folds_valence <- bmr_results_folds_valence %>% 
#   filter(learner_id == "regr.svm")
# 
# svm_folds_arousal <- bmr_results_folds_arousal %>% 
#   filter(learner_id == "regr.svm")


spearmanr_fl = c(rep(0, 10)) #spearman correlation for the baseline learner is NA which is why we test against 0
en_folds_valence$regr.srho = ifelse(is.na(en_folds_valence$regr.srho), median(en_folds_valence$regr.srho, na.rm = T), en_folds_valence$regr.srho) # replace NA for en with median
en_folds_arousal$regr.srho = ifelse(is.na(en_folds_arousal$regr.srho), median(en_folds_arousal$regr.srho, na.rm = T), en_folds_arousal$regr.srho) # replace NA for en with median

# replace NAs with zero for en

# run significance tests for valence

p_rf_valence_mae <- sign_test_folds(mae_fl, rf_folds_valence$regr.mae, n) # execute test featureless vs. rf
p_en_valence_mae <- sign_test_folds(mae_fl, en_folds_valence$regr.mae, n) # execute test featureless vs. Elastic Net
# p_xgb_valence_mae <- sign_test_folds(mae_fl, xgb_folds_valence$regr.mae, n) # execute test featureless vs. xgboost
# p_svm_valence_mae <- sign_test_folds(mae_fl, svm_folds_valence$regr.mae, n) # execute test featureless vs. svm


p_rf_valence_spearmanr <- sign_test_folds(spearmanr_fl, rf_folds_valence$regr.srho, n) # execute test featureless vs. rf
p_en_valence_spearmanr <- sign_test_folds(spearmanr_fl, en_folds_valence$regr.srho, n) # execute test featureless vs. Elastic Net
# p_xgb_valence_spearmanr <- sign_test_folds(spearmanr_fl, xgb_folds_valence$regr.srho, n) # execute test featureless vs. xgboost
# p_svm_valence_spearmanr <- sign_test_folds(spearmanr_fl, svm_folds_valence$regr.srho, n) # execute test featureless vs. svm


# holm correction
p_valence_mae_corrected <- p.adjust(c(p_rf_valence_mae, p_en_valence_mae), 
                                    method = "holm")

p_valence_spearmanr_corrected <- p.adjust(c(p_rf_valence_spearmanr, p_en_valence_spearmanr), 
                                          method = "holm")

# run significance tests for arousal

# execute significance tests for mae
p_rf_arousal_mae <- sign_test_folds(fl_arousal_folds$regr.mae, rf_folds_arousal$regr.mae, n) # execute test featureless vs. rf
p_en_arousal_mae <- sign_test_folds(fl_arousal_folds$regr.mae, en_folds_arousal$regr.mae, n) # execute test featureless vs. Elastic Net
# p_xgb_arousal_mae <- sign_test_folds(fl_arousal_folds$regr.mae, en_arousal_folds$regr.mae, n) # execute test featureless vs. Elastic Net
# p_svm_arousal_mae <- sign_test_folds(fl_arousal_folds$regr.mae, en_arousal_folds$regr.mae, n) # execute test featureless vs. Elastic Net

# execute significance tests for spearman correlation
p_rf_arousal_spearmanr <- sign_test_folds(spearmanr_fl, rf_folds_arousal$regr.srho, n) # execute test featureless vs. rf
p_en_arousal_spearmanr <- sign_test_folds(spearmanr_fl, en_folds_arousal$regr.srho, n) # execute test featureless vs. Elastic Net
# p_xgb_arousal_spearmanr <- sign_test_folds(spearmanr_fl, xgb_folds_arousal$regr.srho, n) # execute test featureless vs. xgboost
# p_svm_arousal_spearmanr <- sign_test_folds(spearmanr_fl, svm_folds_arousal$regr.srho, n) # execute test featureless vs. SVM

# holm correction
p_arousal_mae_corrected <- p.adjust(c(p_rf_arousal_mae, p_en_arousal_mae), 
                                    method = "holm")

p_arousal_spearmanr_corrected <- p.adjust(c(p_rf_arousal_spearmanr, p_en_arousal_spearmanr), 
                                          method = "holm")

### FINISH