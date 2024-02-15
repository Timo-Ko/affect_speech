### PREPARATION ####

rm(list = ls()) # clear memory

# Install and load required packages 

packages <- c( "dplyr", "data.table", "ggplot2", "mlr3", "mlr3learners", "mlr3tuning","ranger", "glmnet", "future", "remotes", "parallel", "mlr3pipelines", "progressr", "progress")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

set.seed(123, kind = "L'Ecuyer") # set seed to make sure all results are reproducible

### READ IN DATA ####

# read in data frames
ema_acoustics_data  <- readRDS("data/ema_audio_data.RData")

# check for missing values

is.na(ema_acoustics_data)

is.finite(ema_acoustics_data$user_id)

colSums(is.na(ema_acoustics_data))

which(colSums(is.na(ema_acoustics_data))>1)

table(is.na(ema_acoustics_data$Between.words.SD.of.pause.duration))

names(which(colSums(is.na(ema_acoustics_data))>0))

# remove rows w missings for targets or predictors
ema_acoustics_data <- ema_acoustics_data[complete.cases(ema_acoustics_data[,c(which(colnames(ema_acoustics_data)=="content"):which(colnames(ema_acoustics_data)=="diff_arousal"), 
                                                        which(colnames(ema_acoustics_data)=="audspec_lengthL1norm_sma_range"):which(colnames(ema_acoustics_data)=="mfcc_sma_de.14._stddevFallingSlope"))]), ]

### CREATE REGRESSION WEIGHTS ####

## for sadness

# share_sad = table(ema_acoustics_data$sad)/sum(table(ema_acoustics_data$sad))
# 
# share_sad_df = as.data.frame(t(share_sad))[,c(2:3)] # create df
# 
# names(share_sad_df)[names(share_sad_df) == 'Var2'] <- 'score'
# 
# # weight is inverse of frequency
# share_sad_df$weight = 1/share_sad_df$Freq
# 
# # scale weights to largest bin 
# share_sad_df$weight_scaled = share_sad_df$weight/min(share_sad_df$weight)
# 
# # cap weights (not larger than five!!)
# share_sad_df$weight_scaled_cap <- replace(share_sad_df$weight_scaled, share_sad_df$weight_scaled > 5, 5)
# 
# # add weight variable to df
# 
# ema_acoustics_data$sad_weight  <-  case_when(ema_acoustics_data$sad == 0 ~ share_sad_df$weight_scaled_cap[1], 
#                                                ema_acoustics_data$sad == 1 ~ share_sad_df$weight_scaled_cap[2], 
#                                                ema_acoustics_data$sad == 2 ~ share_sad_df$weight_scaled_cap[3], 
#                                                ema_acoustics_data$sad == 3 ~ share_sad_df$weight_scaled_cap[4] 
# )

#### CREATE TASKS ####

## compare feature set

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

# content fluctuation from baseline
compare_content_diff = TaskRegr$new(id = "compare_content_diff", 
                                    backend = ema_acoustics_data[,c(which(colnames(ema_acoustics_data)=="user_id"), which(colnames(ema_acoustics_data)== "diff_content"), which(colnames(ema_acoustics_data)=="audspec_lengthL1norm_sma_range"):which(colnames(ema_acoustics_data)=="mfcc_sma_de.14._stddevFallingSlope"))], 
                                    target = "diff_content")

# sad fluctuation from baseline
compare_sad_diff = TaskRegr$new(id = "compare_sad_diff", 
                                backend =  ema_acoustics_data[,c(which(colnames(ema_acoustics_data)=="user_id"), which(colnames(ema_acoustics_data)== "diff_sad"), which(colnames(ema_acoustics_data)=="audspec_lengthL1norm_sma_range"):which(colnames(ema_acoustics_data)=="mfcc_sma_de.14._stddevFallingSlope"))], 
                                target = "diff_sad")

# arousal fluctuation from baseline
compare_arousal_diff = TaskRegr$new(id = "compare_arousal_diff", 
                                backend =  ema_acoustics_data[,c(which(colnames(ema_acoustics_data)=="user_id"), which(colnames(ema_acoustics_data)== "diff_arousal"), which(colnames(ema_acoustics_data)=="audspec_lengthL1norm_sma_range"):which(colnames(ema_acoustics_data)=="mfcc_sma_de.14._stddevFallingSlope"))], 
                                target = "diff_arousal")

# remove data set from working memory to clear up memory
rm(ema_acoustics_data)

## add blocking

# raw content score
# Use participant id column as block factor
compare_content$col_roles$group = "user_id"

# Remove Id from feature space
compare_content$col_roles$feature = setdiff(compare_content$col_roles$feature, "user_id")

# raw sad score
# Use Id column as block factor
compare_sad$col_roles$group = "user_id"

# Remove Id from feature space
compare_sad$col_roles$feature = setdiff(compare_sad$col_roles$feature, "user_id")

# raw arousal score
# Use Id column as block factor
compare_arousal$col_roles$group = "user_id"

# Remove Id from feature space
compare_arousal$col_roles$feature = setdiff(compare_arousal$col_roles$feature, "user_id")

# content difference from baseline
# Use participant id column as block factor
compare_content_diff$col_roles$group = "user_id"

# Remove Id from feature space
compare_content_diff$col_roles$feature = setdiff(compare_content_diff$col_roles$feature, "user_id")

# sad difference from baseline
# Use Id column as block factor
compare_sad_diff$col_roles$group = "user_id"

# Remove Id from feature space
compare_sad_diff$col_roles$feature = setdiff(compare_sad_diff$col_roles$feature, "user_id")

# arousal difference from baseline
# Use Id column as block factor
compare_arousal_diff$col_roles$group = "user_id"

# Remove Id from feature space
compare_arousal_diff$col_roles$feature = setdiff(compare_arousal_diff$col_roles$feature, "user_id")

## add regression weights for sadness

compare_sad$set_col_roles("sad_weight", role = "weight")
compare_sad$col_roles$feature = setdiff(compare_sad$col_roles$feature, "sad_weight") # remove from feature space

#### CREATE LEARNERS ####

# regular regression learners
lrn_fl = lrn("regr.featureless")
lrn_rf = lrn("regr.ranger", num.trees =1000 ) # random forest
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

# # set parameters to tune
# 
# ps_en <- ParamSet$new(list(
#   ParamDbl$new("alpha", lower = 0, upper = 1)
# ))
# 
# 
# ps_rf <- ParamSet$new(list(
#   ParamInt$new("mtry", lower = 1, upper = round(sqrt((compare_sad$ncol))))
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

### PREPROCESSING IN CV ####

# drop constant features and then do a pca in preprocessing 
po_preproc = #po("removeconstants", rel_tol = 0.02) %>>% 
  po("pca", scale. = T
     ,rank. = 88
     ) # extract 88 dimensions 

# check parameters
po_preproc$param_set

# apply graph to task
# abc = lrn_rf_po$train(compare_content)
# 
# 
# po_lrn = as_learner(po_preproc)

# test pca

#testpca <- prcomp(ema_acoustics_data[,c( which(colnames(ema_acoustics_data)=="audspec_lengthL1norm_sma_range"):which(colnames(ema_acoustics_data)=="mfcc_sma_de.14._stddevFallingSlope"))])


# combine training with pre-processing
lrn_rf_po = po_preproc %>>% lrn_rf 
lrn_rr_po = po_preproc %>>% lrn_rr 

#### BENCHMARK ####

# avoid console output from mlr3tuning
logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")

# show progress
progressr::handlers(global = TRUE)
progressr::handlers("progress")

## raw content score
bmgrid_compare_content = benchmark_grid(
  task = compare_content,
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = 5) # enable parallelization

bmr_compare_content = benchmark(bmgrid_compare_content, store_models = F, store_backends = F ) # execute the benchmark

saveRDS(bmr_compare_content, "results/bmr_compare_content.RData") # save results

## raw sad score

bmgrid_compare_sad = benchmark_grid(
  task = compare_sad,
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = 5) # enable parallelization

bmr_compare_sad = benchmark(bmgrid_compare_sad , store_models = F, store_backends = F ) # execute the benchmark

saveRDS(bmr_compare_sad, "results/bmr_compare_sad.RData") # save results

## raw arousal score

# create benchmark
bmgrid_compare_arousal = benchmark_grid(
  task = compare_arousal,
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = 5) # enable parallelization

bmr_compare_arousal = benchmark(bmgrid_compare_arousal, store_models = F, store_backends = F ) # execute the benchmark (do not store models and backend to reduce memory usage)

saveRDS(bmr_compare_arousal, "results/bmr_compare_arousal.RData") # save results


## content difference from baseline
bmgrid_compare_content_diff = benchmark_grid(
  task = compare_content_diff,
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = detectCores())

bmr_compare_content_diff = benchmark(bmgrid_compare_content_diff, store_models = F, store_backends = F ) # execute the benchmark

saveRDS(bmr_compare_content_diff, "results/bmr_compare_content_diff.RData") # save results

## sad difference from baseline

bmgrid_compare_sad_diff = benchmark_grid(
  task = compare_sad_diff,
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = detectCores())

bmr_compare_sad_diff = benchmark(bmgrid_compare_sad_diff, store_models = F, store_backends = F ) # execute the benchmark

saveRDS(bmr_compare_sad_diff, "results/bmr_compare_sad_diff.RData") # save results

## arousal difference from baseline

bmgrid_compare_arousal_diff = benchmark_grid(
  task = compare_arousal_diff,
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = detectCores())

bmr_compare_arousal_diff = benchmark(bmgrid_compare_arousal_diff, store_models = F, store_backends = F ) # execute the benchmark

saveRDS(bmr_compare_arousal_diff, "results/bmr_compare_arousal_diff.RData") # save results


#### BENCHMARK RESULTS AND SIGNIFICANCE TESTS ####

# read in benchmark results

bmr_compare_content <- readRDS("results/bmr_compare_content.RData")
bmr_compare_sad <- readRDS("results/bmr_compare_sad.RData")
bmr_compare_arousal <- readRDS("results/bmr_compare_arousal.RData")
bmr_compare_content_diff <- readRDS("results/bmr_compare_content_diff.RData")
bmr_compare_sad_diff <- readRDS("results/bmr_compare_sad_diff.RData")
bmr_compare_arousal_diff <- readRDS("results/bmr_compare_arousal_diff.RData")

# create one overview table for results
mes_compare <- rbind(bmr_compare_content$aggregate(mes),
                            bmr_compare_content_diff$aggregate(mes),
                            bmr_compare_sad$aggregate(mes),
                            bmr_compare_sad_diff$aggregate(mes),
                            bmr_compare_arousal$aggregate(mes),
                            bmr_compare_arousal_diff$aggregate(mes))

# get performance of learners across cv folds

bmr_results_folds_content <- bmr_compare_content$score(
  measures = mes,
  ids = TRUE,
  conditions = FALSE,
  predict_sets = "test"
)

bmr_results_folds_sad <- bmr_compare_sad$score(
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

bmr_results_folds_content_diff <- bmr_compare_content_diff$score(
  measures = mes,
  ids = TRUE,
  conditions = FALSE,
  predict_sets = "test"
)

bmr_results_folds_sad_diff <- bmr_compare_sad_diff$score(
  measures = mes,
  ids = TRUE,
  conditions = FALSE,
  predict_sets = "test"
)

bmr_results_folds_arousal_diff <- bmr_compare_arousal_diff$score(
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

n = nrow(ema_compare_data)

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
p_content_rsq_corrected <- p.adjust(c(p_rf_content_rsq, p_rr_content_rsq), method = "holm")
p_content_diff_rsq_corrected <- p.adjust(c(p_rf_content_diff_rsq, p_rr_content_diff_rsq), method = "holm")
p_sad_rsq_corrected <- p.adjust(c(p_rf_sad_rsq, p_rr_sad_rsq), method = "holm")
p_sad_diff_rsq_corrected <- p.adjust(c(p_rf_sad_diff_rsq, p_rr_sad_diff_rsq), method = "holm")
p_arousal_rsq_corrected <- p.adjust(c(p_rf_arousal_rsq, p_rr_arousal_rsq), method = "holm")
p_arousal_diff_rsq_corrected <- p.adjust(c(p_rf_arousal_diff_rsq, p_rr_arousal_diff_rsq), method = "holm")

# append sign results to measures table

mes_compare$p_rsq <- c(NA, p_content_rsq_corrected, 
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
bmr_results_folds <- base::merge(bmr_results_folds, mes_compare[,c("task_id", "learner_id", "p_rsq")], by = c("task_id", "learner_id"))

# create significance column
bmr_results_folds$significance <- as.factor(ifelse(bmr_results_folds$p_rsq >= 0.05 | is.na(bmr_results_folds$p_rsq), "no", "yes"))

# rename 
bmr_results_folds <- bmr_results_folds %>% 
  mutate(learner_id = case_when(
    learner_id == "regr.featureless" ~    "Baseline",
    learner_id == "regr.ranger" ~ "Random Forest",
    learner_id == "regr.cv_glmnet" ~ "LASSO")) %>% 
  mutate(task_id = case_when(
    task_id == "compare_content" ~    "Contentedness",
    task_id == "compare_content_diff" ~ "Contentedness Difference",
    task_id == "compare_sad" ~    "Sadness",
    task_id == "compare_sad_diff" ~ "Sadness Difference",
    task_id == "compare_arousal" ~ "Arousal",
    task_id == "compare_arousal_diff" ~ "Arousal Difference"))

# create figure
bmr_compare_plot <- ggplot(bmr_results_folds, aes(x= task_id , y= regr.rsq, color = significance, shape = learner_id)) + 
  #geom_boxplot() +
  geom_boxplot(width = 0.3,lwd = 1, aes(color = significance), alpha = 0.3, outlier.shape=NA, position=position_dodge(0.5)) +  
  #geom_point(aes(shape = learner_id, group = task_id), position=position_jitterdodge()) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5), size = 2) +
  scale_x_discrete(element_blank()) +
  scale_y_continuous(name = bquote("R"^2), limits = c(-0.15, 0.15)) + 
  theme_minimal(base_size = 20) +
  labs(title = "State affect experience") + # add title
  labs(colour = "Significance", shape = "Algorithm") + # change legend title
  # scale_shape_manual(name = "Algorithm",
  #                    labels = c("Baseline", "LASSO", "Random Forest"),
  #                    values = c(17, 18, 19)) + 
  #guides(shape = guide_legend(override.aes = list(size = 2))) +
  #scale_shape_manual(values = c(15,16,17)) + #add shapes for different learners
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) # rotate x axis labels

bmr_compare_plot

# save figure

png(file="figures/bmr_compare_plot.png",width=700, height=500)

bmr_egemaps_plot

dev.off()

### FINISH