### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "parallel", "data.table", "ggplot2", "mlr3", "mlr3learners", "mlr3tuning","ranger", "glmnet", "future", "remotes")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

set.seed(123, kind = "L'Ecuyer") # set seed to make sure all results are reproducible

### READ IN DATA ####

# read in data frames
affect_acoustics  <- readRDS("data/affect_acoustics.RData")

# load required functions
source("r_code/functions/sign_test_folds.R")

#### CREATE TASKS ####

## benchmark check predictions

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

# remove data set from working memory to clear up memory
rm(affect_acoustics)

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

#### CREATE LEARNERS ####

lrn_fl = lrn("regr.featureless")
lrn_rf = lrn("regr.ranger", 
             mtry = to_tune(1, 50),
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
  measure = msr("regr.rsq"),
  terminator = trm("evals", n_evals = 10),
  tuner = tnr("random_search"),
  store_models = TRUE
)

at_rr = AutoTuner$new(
  learner = lrn_rr,
  resampling = rsmp("cv", folds = 5),
  measure = msr("regr.rsq"),
  terminator = trm("evals", n_evals = 10),
  tuner = tnr("random_search"),
  store_models = TRUE
)

#### RESAMPLING ####

resampling = rsmp("cv", folds = 10L)

# measures for benchmark
mes = msrs(c("regr.rsq","regr.srho"))

#### BENCHMARK ####

# avoid console output from mlr3tuning
logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")

# show progress
progressr::handlers(global = TRUE)
progressr::handlers("progress")

## age
bmgrid_egemaps_content = benchmark_grid(
  task = egemaps_age,
  learner = list(lrn_fl, lrn_rf, lrn_rr),
  resampling = resampling
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_egemaps_age = benchmark(bmgrid_egemaps_age, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_egemaps_age, "results/bmr_egemaps_age.RData") # save results

## gender
bmgrid_egemaps_gender = benchmark_grid(
  task = egemaps_gender,
  learner = list(lrn("classif.featureless", predict_type = "prob"), lrn("classif.ranger", num.trees =1000, predict_type = "prob"), lrn("classif.cv_glmnet", predict_type = "prob")),
  resampling = resampling
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_egemaps_gender = benchmark(bmgrid_egemaps_gender, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_egemaps_gender, "results/bmr_egemaps_gender.RData") # save results

## momentary affect experience

bmgrid_egemaps = benchmark_grid(
  task = c(egemaps_arousal,
           #egemaps_arousal_diff,
           egemaps_content,
           #egemaps_content_diff,
           egemaps_sad#,
           #egemaps_sad_diff
           ),
  learner = list(lrn_fl, at_rf, at_rr),
  resampling = resampling
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_egemaps = benchmark(bmgrid_egemaps, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_egemaps, "results/bmr_egemaps.RData") # save results

#### BENCHMARK RESULTS AND SIGNIFICANCE TESTS ####

# read in benchmark results

bmr_egemaps_content <- readRDS("results/bmr_egemaps_content.RData")
bmr_egemaps_sad <- readRDS("results/bmr_egemaps_sad.RData")


bmr_egemaps$aggregate(mes)

# create one overview table for results
mes_egemaps <- rbind(bmr_egemaps_content$aggregate(mes),
                     bmr_egemaps_content_diff$aggregate(mes),
                     bmr_egemaps_sad$aggregate(mes),
                     bmr_egemaps_sad_diff$aggregate(mes),
                     bmr_egemaps_arousal$aggregate(mes),
                     bmr_egemaps_arousal_diff$aggregate(mes))

# get performance of learners across cv folds

bmr_results_folds_content <- bmr_egemaps_content$score(
  measures = mes,
  ids = TRUE,
  conditions = FALSE,
  predict_sets = "test"
)

bmr_results_folds_sad <- bmr_egemaps_sad$score(
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

bmr_results_folds_content_diff <- bmr_egemaps_content_diff$score(
  measures = mes,
  ids = TRUE,
  conditions = FALSE,
  predict_sets = "test"
)

bmr_results_folds_sad_diff <- bmr_egemaps_sad_diff$score(
  measures = mes,
  ids = TRUE,
  conditions = FALSE,
  predict_sets = "test"
)

bmr_results_folds_arousal_diff <- bmr_egemaps_arousal_diff$score(
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

n = nrow(affect_acoustics)

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

mes_egemaps$p_rsq <- c(NA, p_content_rsq_corrected, 
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
bmr_results_folds <- base::merge(bmr_results_folds, mes_egemaps[,c("task_id", "learner_id", "p_rsq")], by = c("task_id", "learner_id"))

# create significance column
bmr_results_folds$significance <- as.factor(ifelse(bmr_results_folds$p_rsq >= 0.05 | is.na(bmr_results_folds$p_rsq), "no", "yes"))

# rename 
bmr_results_folds <- bmr_results_folds %>% 
  mutate(learner_id = case_when(
    learner_id == "regr.featureless" ~    "Baseline",
    learner_id == "regr.ranger" ~ "Random Forest",
    learner_id == "regr.cv_glmnet" ~ "LASSO")) %>% 
  mutate(task_id = case_when(
    task_id == "egemaps_content" ~    "Contentedness",
    task_id == "egemaps_content_diff" ~ "Contentedness Difference",
    task_id == "egemaps_sad" ~    "Sadness",
    task_id == "egemaps_sad_diff" ~ "Sadness Difference",
    task_id == "egemaps_arousal" ~ "Arousal",
    task_id == "egemaps_arousal_diff" ~ "Arousal Difference"))

# create figure
bmr_egemaps_plot <- ggplot(bmr_results_folds, aes(x= task_id , y= regr.rsq, color = significance, shape = learner_id)) + 
  #geom_boxplot() +
  geom_boxplot(width = 0.3,lwd = 1, aes(color = significance), alpha = 0.3, outlier.shape=NA, position=position_dodge(0.5)) +  
  #geom_point(aes(shape = learner_id, group = task_id), position=position_jitterdodge()) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5), size = 2) +
  scale_x_discrete(element_blank()) +
  scale_y_continuous(name = bquote("R"^2), limits = c(-0.15, 0.15)) + 
  theme_minimal(base_size = 20) +
  labs(title = "Prediction performance for affect experience from voice cues") + # add title
  labs(colour = "Significance", shape = "Algorithm") + # change legend title
  # scale_shape_manual(name = "Algorithm",
  #                    labels = c("Baseline", "LASSO", "Random Forest"),
  #                    values = c(17, 18, 19)) + 
  #guides(shape = guide_legend(override.aes = list(size = 2))) +
  #scale_shape_manual(values = c(15,16,17)) + #add shapes for different learners
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) # rotate x axis labels

bmr_egemaps_plot

# save figure

png(file="figures/bmr_egemaps_plot.png",width=1000, height=700)

bmr_egemaps_plot

dev.off()

### FINISH