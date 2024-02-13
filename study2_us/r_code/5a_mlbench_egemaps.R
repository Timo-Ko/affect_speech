### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "data.table", "ggplot2", "mlr3", "mlr3learners", "mlr3tuning","ranger", "glmnet", "future", "remotes")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

### READ IN DATA ####

# read in data frames
affect_egemaps  <- readRDS("study1_ger/data/affect_egemaps.RData")

#### CREATE TASKS ####

## eGeMAPS feature set

# raw valence score
egemaps_valence = TaskRegr$new(id = "egemaps_valence", backend = affect_egemaps[,c(4, 6, 13:ncol(affect_egemaps))], target = "valence")

# raw arousal score
egemaps_arousal = TaskRegr$new(id = "egemaps_arousal", backend = affect_egemaps[,c(4, 9, 13:ncol(affect_egemaps))], target = "arousal")

# valence fluctuation from baseline
egemaps_valence_diff = TaskRegr$new(id = "egemaps_valence_diff", backend = affect_egemaps[,c(4, 8, 13:ncol(affect_egemaps))], target = "diff_valence")

# arousal fluctuation from baseline
egemaps_arousal_diff = TaskRegr$new(id = "egemaps_arousal_diff", backend = affect_egemaps[,c(4, 11, 13:ncol(affect_egemaps))], target = "diff_arousal")

## add blocking

# raw valence score
# Use participant id column as block factor
egemaps_valence$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_valence$col_roles$feature = setdiff(egemaps_valence$col_roles$feature, "user_id")

# raw arousal score
# Use Id column as block factor
egemaps_arousal$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_arousal$col_roles$feature = setdiff(egemaps_arousal$col_roles$feature, "user_id")

# valence difference from baseline
# Use participant id column as block factor
egemaps_valence_diff$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_valence_diff$col_roles$feature = setdiff(egemaps_valence_diff$col_roles$feature, "user_id")

# arousal difference from baseline
# Use Id column as block factor
egemaps_arousal_diff$col_roles$group = "user_id"

# Remove Id from feature space
egemaps_arousal_diff$col_roles$feature = setdiff(egemaps_arousal_diff$col_roles$feature, "user_id")


#### CREATE LEARNERS ####

lrn_fl = lrn("regr.featureless")
lrn_rf = lrn("regr.ranger", num.trees =1000) # random forest
lrn_en = lrn("regr.cv_glmnet") # elastic net

# set parameters

ps_en <- ParamSet$new(list(
  ParamDbl$new("alpha", lower = 0, upper = 1)
))


ps_rf <- ParamSet$new(list(
  ParamInt$new("mtry", lower = 1, upper = round(sqrt((egemaps_arousal$ncol))))
))

# enable parallelization
set_threads(lrn_fl, n = 4)
set_threads(lrn_en, n = 4)
set_threads(lrn_rf, n = 4)

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

#### BENCHMARK ####

# avoid console output from mlr3tuning
logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")

future::plan("multisession", workers = 4)

## raw valence score
bmgrid_egemaps_valence = benchmark_grid(
  task = egemaps_valence,
  learner = list(lrn_fl, at_rf, at_en),
  resampling = resampling_outer
)

bmr_egemaps_valence = benchmark(bmgrid_egemaps_valence) # execute the benchmark

# save results
saveRDS(bmr_egemaps_valence, "results/bmr_egemaps_valence.RData")

## raw arousal score
future::plan("multisession", workers = 4)

bmgrid_egemaps_arousal = benchmark_grid(
  task = egemaps_arousal,
  learner = list(lrn_fl, at_rf, at_en),
  resampling = resampling_outer
)

bmr_egemaps_arousal = benchmark(bmgrid_egemaps_arousal) # execute the benchmark

# save results
saveRDS(bmr_egemaps_arousal, "results/bmr_egemaps_arousal.RData")


## valence difference from baseline
bmgrid_egemaps_valence_diff = benchmark_grid(
  task = egemaps_valence_diff,
  learner = list(lrn_fl, at_rf, at_en),
  resampling = resampling_outer
)

bmr_egemaps_valence_diff = benchmark(bmgrid_egemaps_valence_diff) # execute the benchmark

# save results
saveRDS(bmr_egemaps_valence_diff, "results/bmr_egemaps_valence_diff.RData")

## arousal difference from baseline
future::plan("multisession", workers = 4)

bmgrid_egemaps_arousal_diff = benchmark_grid(
  task = egemaps_arousal_diff,
  learner = list(lrn_fl, at_rf, at_en),
  resampling = resampling_outer
)

bmr_egemaps_arousal_diff = benchmark(bmgrid_egemaps_arousal_diff) # execute the benchmark

# save results
saveRDS(bmr_egemaps_arousal_diff, "results/bmr_egemaps_arousal_diff.RData")

#### BENCHMARK RESULTS AND SIGNIFICANCE TESTS ####

# read in benchmark results

bmr_egemaps_valence <- readRDS("study1_ger/results/bmr_egemaps_valence.RData")
bmr_egemaps_arousal <- readRDS("study1_ger/results/bmr_egemaps_arousal.RData")
bmr_egemaps_valence_diff <- readRDS("study1_ger/results/bmr_egemaps_valence_diff.RData")
bmr_egemaps_arousal_diff <- readRDS("study1_ger/results/bmr_egemaps_arousal_diff.RData")

bmr_egemaps<- readRDS("study1_ger/results/bmr_egemaps.RData")

bmr_egemaps_wordembeddings <- readRDS("study2_us/results/bmr_egemaps_wordembeddings.RData")

# view aggregated performance measures

mes_egemaps = bmr_egemaps$aggregate(mes)

mes_egemaps_wordembeddings = bmr_egemaps_wordembeddings$aggregate(mes)



##

mes_egemaps_arousal = bmr_egemaps_arousal$aggregate(mes)

mes_egemaps_arousal

mes_egemaps_valence = bmr_egemaps_valence$aggregate(mes)

mes_egemaps_valence

mes_egemaps_arousal_diff = bmr_egemaps_arousal_diff$aggregate(mes)

mes_egemaps_arousal_diff

mes_egemaps_valence_diff = bmr_egemaps_valence_diff$aggregate(mes)

mes_egemaps_valence_diff


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

n = nrow(affect_egemaps)


# get performance of learners across cv folds

bmr_results_folds_valence <- bmr_egemaps_valence$score(
  measures = mes,
  ids = TRUE,
  conditions = FALSE,
  predict_sets = "test"
)

bmr_results_folds_arousal <- bmr_egemaps_arousal$score(
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


spearmanr_fl = c(rep(0, 10)) #spearman correlation for the baseline learner is NA which is why we test against 0
en_folds_valence$regr.srho = ifelse(is.na(en_folds_valence$regr.srho), median(en_folds_valence$regr.srho, na.rm = T), en_folds_valence$regr.srho) # replace NA for en with median
en_folds_arousal$regr.srho = ifelse(is.na(en_folds_arousal$regr.srho), median(en_folds_arousal$regr.srho, na.rm = T), en_folds_arousal$regr.srho) # replace NA for en with median

# replace NAs with zero for en

# run significance tests for valence

p_rf_valence_mae <- sign_test_folds(fl_folds_valence$regr.mae, rf_folds_valence$regr.mae, n) # execute test featureless vs. rf
p_en_valence_mae <- sign_test_folds(fl_folds_valence$regr.mae, en_folds_valence$regr.mae, n) # execute test featureless vs. Elastic Net

p_rf_valence_spearmanr <- sign_test_folds(spearmanr_fl, rf_folds_valence$regr.srho, n) # execute test featureless vs. rf
p_en_valence_spearmanr <- sign_test_folds(spearmanr_fl, en_folds_valence$regr.srho, n) # execute test featureless vs. Elastic Net

# holm correction
p_valence_mae_corrected <- p.adjust(c(p_rf_valence_mae, p_en_valence_mae), 
                            method = "holm")

p_valence_spearmanr_corrected <- p.adjust(c(p_rf_valence_spearmanr, p_en_valence_spearmanr), 
                                    method = "holm")

# run significance tests for arousal

# execute significance tests for mae
p_rf_arousal_mae <- sign_test_folds(fl_folds_arousal$regr.mae, rf_folds_arousal$regr.mae, n) # execute test featureless vs. rf
p_en_arousal_mae <- sign_test_folds(fl_folds_arousal$regr.mae, en_folds_arousal$regr.mae, n) # execute test featureless vs. Elastic Net

# execute significance tests for spearman correlation
p_rf_arousal_spearmanr <- sign_test_folds(spearmanr_fl, rf_folds_arousal$regr.srho, n) # execute test featureless vs. rf
p_en_arousal_spearmanr <- sign_test_folds(spearmanr_fl, en_folds_arousal$regr.srho, n) # execute test featureless vs. Elastic Net

# holm correction
p_arousal_mae_corrected <- p.adjust(c(p_rf_arousal_mae, p_en_arousal_mae), 
                                    method = "holm")

p_arousal_spearmanr_corrected <- p.adjust(c(p_rf_arousal_spearmanr, p_en_arousal_spearmanr), 
                                          method = "holm")

### FINISH