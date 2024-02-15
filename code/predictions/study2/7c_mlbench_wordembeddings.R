### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "data.table", "ggplot2", "mlr3", "mlr3learners", "mlr3tuning","ranger", "glmnet", "future", "remotes", "parallel", "progressr", "progress")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

set.seed(123, kind = "L'Ecuyer") # set seed to make sure all results are reproducible

### READ IN DATA ####

# read in data frames
affect_wordembeddings  <- readRDS("data/affect_wordembeddings.RData")

# remove missings
#affect_wordembeddings <- na.omit(affect_wordembeddings)

# load required functions
source("r_code/functions/sign_test_folds.R")

#### CREATE TASKS ####

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
#resampling_inner = rsmp("cv", folds = 5L)

#### SET PERFORMANCE MEASURES ####

# measures for benchmark
mes = msrs(c("regr.rsq","regr.srho", "regr.mae"))

# measure for tuning
#mes_tuning = msr("regr.mae")

#### PARAMETER TUNING ####

# # set parameters
# 
# ps_en <- ParamSet$new(list(
#   ParamDbl$new("alpha", lower = 0, upper = 1)
# ))
# 
# 
# ps_rf <- ParamSet$new(list(
#   ParamInt$new("mtry", lower = 1, upper = round(sqrt((wordembeddings_sad$ncol))))
# ))
# 
# # define tuning
# terminator <- trm("evals", n_evals = 10)
# tuner <- tnr("random_search")
# 
# # create autotuners
# 
# at_en <- AutoTuner$new(
#   learner = lrn_rr,
#   resampling = resampling_inner,
#   measure = mes_tuning,
#   search_space = ps_en,
#   terminator = terminator,
#   tuner = tuner)
# 
# at_rf <- AutoTuner$new(
#   learner = lrn_rf,
#   resampling = resampling_inner,
#   measure = mes_tuning,
#   search_space = ps_rf,
#   terminator = terminator,
#   tuner = tuner)

#### BENCHMARK ####

# avoid console output from mlr3tuning
logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")

# show progress
progressr::handlers(global = TRUE)
progressr::handlers("progress")


## raw content score
bmgrid_wordembeddings_content = benchmark_grid(
  task = wordembeddings_content,
  learner = list(lrn_fl, lrn_rf, lrn_rr),
  resampling = resampling
)

future::plan("multisession", workers = detectCores())

bmr_wordembeddings_content = benchmark(bmgrid_wordembeddings_content, store_models = F, store_backends = F ) # execute the benchmark

saveRDS(bmr_wordembeddings_content, "results/bmr_wordembeddings_content.RData") # save results

## raw sad score

bmgrid_wordembeddings_sad = benchmark_grid(
  task = wordembeddings_sad,
  learner = list(lrn_fl, lrn_rf, lrn_rr),
  resampling = resampling
)

future::plan("multisession", workers = detectCores())

bmr_wordembeddings_sad = benchmark(bmgrid_wordembeddings_sad, store_models = F, store_backends = F ) # execute the benchmark

saveRDS(bmr_wordembeddings_sad, "results/bmr_wordembeddings_sad.RData") # save results

## raw arousal score

bmgrid_wordembeddings_arousal = benchmark_grid(
  task = wordembeddings_arousal,
  learner = list(lrn_fl, lrn_rf, lrn_rr),
  resampling = resampling
)

future::plan("multisession", workers = detectCores())

bmr_wordembeddings_arousal = benchmark(bmgrid_wordembeddings_arousal, store_models = F, store_backends = F ) # execute the benchmark

saveRDS(bmr_wordembeddings_arousal, "results/bmr_wordembeddings_arousal.RData") # save results


## content difference from baseline

bmgrid_wordembeddings_content_diff = benchmark_grid(
  task = wordembeddings_content_diff,
  learner = list(lrn_fl, lrn_rf, lrn_rr),
  resampling = resampling
)

future::plan("multisession", workers = detectCores())

bmr_wordembeddings_content_diff = benchmark(bmgrid_wordembeddings_content_diff, store_models = F, store_backends = F ) # execute the benchmark

saveRDS(bmr_wordembeddings_content_diff, "results/bmr_wordembeddings_content_diff.RData") # save results

## sad difference from baseline

bmgrid_wordembeddings_sad_diff = benchmark_grid(
  task = wordembeddings_sad_diff,
  learner = list(lrn_fl, lrn_rf, lrn_rr),
  resampling = resampling
)

future::plan("multisession", workers = detectCores())

bmr_wordembeddings_sad_diff = benchmark(bmgrid_wordembeddings_sad_diff, store_models = F, store_backends = F ) # execute the benchmark

saveRDS(bmr_wordembeddings_sad_diff, "results/bmr_wordembeddings_sad_diff.RData") # save results

## arousal difference from baseline

bmgrid_wordembeddings_arousal_diff = benchmark_grid(
  task = wordembeddings_arousal_diff,
  learner = list(lrn_fl, lrn_rf, lrn_rr),
  resampling = resampling
)

future::plan("multisession", workers = detectCores())

bmr_wordembeddings_arousal_diff = benchmark(bmgrid_wordembeddings_arousal_diff, store_models = F, store_backends = F ) # execute the benchmark

saveRDS(bmr_wordembeddings_arousal_diff, "results/bmr_wordembeddings_arousal_diff.RData") # save results


#### BENCHMARK RESULTS AND SIGNIFICANCE TESTS ####

# read in benchmark results

bmr_wordembeddings_content <- readRDS("results/bmr_wordembeddings_content.RData")
bmr_wordembeddings_sad <- readRDS("results/bmr_wordembeddings_sad.RData")
bmr_wordembeddings_arousal <- readRDS("results/bmr_wordembeddings_arousal.RData")
bmr_wordembeddings_content_diff <- readRDS("results/bmr_wordembeddings_content_diff.RData")
bmr_wordembeddings_sad_diff <- readRDS("results/bmr_wordembeddings_sad_diff.RData")
bmr_wordembeddings_arousal_diff <- readRDS("results/bmr_wordembeddings_arousal_diff.RData")

# create one overview table for results
mes_wordembeddings <- rbind(bmr_wordembeddings_content$aggregate(mes),
                            bmr_wordembeddings_content_diff$aggregate(mes),
                            bmr_wordembeddings_sad$aggregate(mes),
                            bmr_wordembeddings_sad_diff$aggregate(mes),
                            bmr_wordembeddings_arousal$aggregate(mes),
                            bmr_wordembeddings_arousal_diff$aggregate(mes))

# get performance of learners across cv folds

bmr_results_folds_content <- bmr_wordembeddings_content$score(
  measures = mes,
  ids = TRUE,
  conditions = FALSE,
  predict_sets = "test"
)

bmr_results_folds_sad <- bmr_wordembeddings_sad$score(
  measures = mes,
  ids = TRUE,
  conditions = FALSE,
  predict_sets = "test"
)

bmr_results_folds_arousal <- bmr_wordembeddings_arousal$score(
  measures = mes,
  ids = TRUE,
  conditions = FALSE,
  predict_sets = "test"
)

bmr_results_folds_content_diff <- bmr_wordembeddings_content_diff$score(
  measures = mes,
  ids = TRUE,
  conditions = FALSE,
  predict_sets = "test"
)

bmr_results_folds_sad_diff <- bmr_wordembeddings_sad_diff$score(
  measures = mes,
  ids = TRUE,
  conditions = FALSE,
  predict_sets = "test"
)

bmr_results_folds_arousal_diff <- bmr_wordembeddings_arousal_diff$score(
  measures = mes,
  ids = TRUE,
  conditions = FALSE,
  predict_sets = "test"
)

# get fl results

fl_folds_content <- bmr_results_folds_content %>% 
  filter(learner_id == "regr.featureless")

fl_folds_content_diff <- bmr_results_folds_content_diff %>% 
  filter(learner_id == "regr.featureless")

fl_folds_sad <- bmr_results_folds_sad %>% 
  filter(learner_id == "regr.featureless")

fl_folds_sad_diff <- bmr_results_folds_sad_diff %>% 
  filter(learner_id == "regr.featureless")

fl_folds_arousal <- bmr_results_folds_arousal %>% 
  filter(learner_id == "regr.featureless")

fl_folds_arousal_diff <- bmr_results_folds_arousal_diff %>% 
  filter(learner_id == "regr.featureless")

# get rf results

rf_folds_content <- bmr_results_folds_content %>% 
  filter(learner_id == "regr.ranger")

rf_folds_content_diff <- bmr_results_folds_content_diff %>% 
  filter(learner_id == "regr.ranger")

rf_folds_sad <- bmr_results_folds_sad %>% 
  filter(learner_id == "regr.ranger")

rf_folds_sad_diff <- bmr_results_folds_sad_diff %>% 
  filter(learner_id == "regr.ranger")

rf_folds_arousal <- bmr_results_folds_arousal %>% 
  filter(learner_id == "regr.ranger")

rf_folds_arousal_diff <- bmr_results_folds_arousal_diff %>% 
  filter(learner_id == "regr.ranger")

# get lasso results

rr_folds_content <- bmr_results_folds_content %>% 
  filter(learner_id == "regr.cv_glmnet")

rr_folds_content_diff <- bmr_results_folds_content_diff %>% 
  filter(learner_id == "regr.cv_glmnet")

rr_folds_sad <- bmr_results_folds_sad %>% 
  filter(learner_id == "regr.cv_glmnet")

rr_folds_sad_diff <- bmr_results_folds_sad_diff %>% 
  filter(learner_id == "regr.cv_glmnet")

rr_folds_arousal <- bmr_results_folds_arousal %>% 
  filter(learner_id == "regr.cv_glmnet")

rr_folds_arousal_diff <- bmr_results_folds_arousal_diff %>% 
  filter(learner_id == "regr.cv_glmnet")


# run significance tests 

n = nrow(affect_wordembeddings)

p_rf_content_rsq <- sign_test_folds(fl_folds_content$regr.rsq, rf_folds_content$regr.rsq, n) # execute test featureless vs. rf
p_rr_content_rsq <- sign_test_folds(fl_folds_content$regr.rsq, rr_folds_content$regr.rsq, n) # execute test featureless vs. LASSO

p_rf_content_diff_rsq <- sign_test_folds(fl_folds_content_diff$regr.rsq, rf_folds_content_diff$regr.rsq, n) # execute test featureless vs. rf
p_rr_content_diff_rsq <- sign_test_folds(fl_folds_content_diff$regr.rsq, rr_folds_content_diff$regr.rsq, n) # execute test featureless vs. LASSO

p_rf_sad_rsq <- sign_test_folds(fl_folds_sad$regr.rsq, rf_folds_sad$regr.rsq, n) # execute test featureless vs. rf
p_rr_sad_rsq <- sign_test_folds(fl_folds_sad$regr.rsq, rr_folds_sad$regr.rsq, n) # execute test featureless vs. LASSO

p_rf_sad_diff_rsq <- sign_test_folds(fl_folds_sad_diff$regr.rsq, rf_folds_sad_diff$regr.rsq, n) # execute test featureless vs. rf
p_rr_sad_diff_rsq <- sign_test_folds(fl_folds_sad_diff$regr.rsq, rr_folds_sad_diff$regr.rsq, n) # execute test featureless vs. LASSO

p_rf_arousal_rsq <- sign_test_folds(fl_folds_arousal$regr.rsq, rf_folds_arousal$regr.rsq, n) # execute test featureless vs. rf
p_rr_arousal_rsq <- sign_test_folds(fl_folds_arousal$regr.rsq, rr_folds_arousal$regr.rsq, n) # execute test featureless vs. LASSO

p_rf_arousal_diff_rsq <- sign_test_folds(fl_folds_arousal_diff$regr.rsq, rf_folds_arousal_diff$regr.rsq, n) # execute test featureless vs. rf
p_rr_arousal_diff_rsq <- sign_test_folds(fl_folds_arousal_diff$regr.rsq, rr_folds_arousal_diff$regr.rsq, n) # execute test featureless vs. LASSO

# holm correction
p_content_rsq_corrected <- p.adjust(c(p_rf_content_rsq, p_rr_content_rsq), method = "holm", n = 12)
p_content_diff_rsq_corrected <- p.adjust(c(p_rf_content_diff_rsq, p_rr_content_diff_rsq), method = "holm", n = 12)
p_sad_rsq_corrected <- p.adjust(c(p_rf_sad_rsq, p_rr_sad_rsq), method = "holm", n = 12)
p_sad_diff_rsq_corrected <- p.adjust(c(p_rf_sad_diff_rsq, p_rr_sad_diff_rsq), method = "holm", n = 12)
p_arousal_rsq_corrected <- p.adjust(c(p_rf_arousal_rsq, p_rr_arousal_rsq), method = "holm", n = 12)
p_arousal_diff_rsq_corrected <- p.adjust(c(p_rf_arousal_diff_rsq, p_rr_arousal_diff_rsq), method = "holm", n = 12)

# append sign results to measures table

mes_wordembeddings$p_rsq <- c(NA, p_content_rsq_corrected, 
                              NA, p_content_diff_rsq_corrected, 
                              NA, p_sad_rsq_corrected, 
                              NA, p_sad_diff_rsq_corrected,
                              NA, p_arousal_rsq_corrected,
                              NA, p_arousal_diff_rsq_corrected
                              )

### VISUALIZE PREDICTION PERFORMANCE ACROSS LEARNERS, TASKS, AND FOLDS ####

# rbind results together
bmr_results_folds <- rbind(bmr_results_folds_content, 
                           bmr_results_folds_content_diff,
                           bmr_results_folds_sad, 
                           bmr_results_folds_sad_diff, 
                           bmr_results_folds_arousal, 
                           bmr_results_folds_arousal_diff
                           )

# add column wwith p values
bmr_results_folds <- base::merge(bmr_results_folds, mes_wordembeddings[,c("task_id", "learner_id", "p_rsq")], by = c("task_id", "learner_id"))

# create significance column
bmr_results_folds$significance <- as.factor(ifelse(bmr_results_folds$p_rsq >= 0.05 | is.na(bmr_results_folds$p_rsq), "no", "yes"))

# rename 
bmr_results_folds <- bmr_results_folds %>% 
  mutate(learner_id = case_when(
    learner_id == "regr.featureless" ~    "Baseline",
    learner_id == "regr.ranger" ~ "Random Forest",
    learner_id == "regr.cv_glmnet" ~ "LASSO")) %>% 
  mutate(task_id = case_when(
    task_id == "wordembeddings_content" ~    "Contentedness",
    task_id == "wordembeddings_content_diff" ~ "Contentedness Difference",
    task_id == "wordembeddings_sad" ~    "Sadness",
    task_id == "wordembeddings_sad_diff" ~ "Sadness Difference",
    task_id == "wordembeddings_arousal" ~ "Arousal",
    task_id == "wordembeddings_arousal_diff" ~ "Arousal Difference"))

# create figure
bmr_wordembeddings_plot <- ggplot(bmr_results_folds, aes(x= task_id , y= regr.rsq, color = significance, shape = learner_id)) + 
  #geom_boxplot() +
  geom_boxplot(width = 0.3,lwd = 1, aes(color = significance), alpha = 0.3, outlier.shape=NA, position=position_dodge(0.5)) +  
  #geom_point(aes(shape = learner_id, group = task_id), position=position_jitterdodge()) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5), size = 2) +
  scale_x_discrete(element_blank()) +
  scale_y_continuous(name = bquote("R"^2), limits = c(-0.15, 0.15)) + 
  theme_minimal(base_size = 20) +
  labs(title = "Prediction of affect experience from wordembeddings") + # add title
  labs(colour = "Significance", shape = "Algorithm") + # change legend title
  # scale_shape_manual(name = "Algorithm",
  #                    labels = c("Baseline", "LASSO", "Random Forest"),
  #                    values = c(17, 18, 19)) + 
  #guides(shape = guide_legend(override.aes = list(size = 2))) +
  #scale_shape_manual(values = c(15,16,17)) + #add shapes for different learners
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) # rotate x axis labels

bmr_wordembeddings_plot

# save figure

png(file="figures/bmr_wordembeddings_plot.png",width=700, height=500)

bmr_egemaps_plot

dev.off()

### FINISH