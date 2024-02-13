### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "data.table", "ggplot2", "mlr3", "mlr3learners", "mlr3tuning","ranger", "glmnet", "future", "mlr3pipelines", "parallel", "progressr", "progress")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

set.seed(123, kind = "L'Ecuyer") # set seed to make sure all results are reproducible

### READ IN DATA ####

# read in data frames
affect_compare  <- readRDS("study1_ger/data/affect_compare.RData")

# load required functions
source("study1_ger/r_code/functions/sign_test_folds.R")

#### RUN PCA ####

# affect_compare_pca <- prcomp(affect_compare[,c(which(colnames(affect_compare)=="audspeclengthL1normsmarange"):which(colnames(affect_compare)=="mfccsmade14stddevFallingSlope"))],
#              center = TRUE,
#              scale. = TRUE)
# 
# # select components for 99% of variance explained
# var_explained <- affect_compare_pca$sdev^2 / sum(affect_compare_pca$sdev^2)
# 
# #create scree plot
# plot(var_explained, xlab = "Principal Component",
#      ylab = "Proportion of Variance Explained",
#      type = "b")
# 
# #cumulative scree plot
# plot(cumsum(var_explained), xlab = "Principal Component",
#        ylab = "Cumulative Proportion of Variance Explained",
#        type = "b")

#### CREATE TASKS ####

## compare feature set
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

# valence difference from baseline
compare_valence_diff = TaskRegr$new(id = "compare_valence_diff", 
                               backend = affect_compare[,c(which(colnames(affect_compare)=="user_id"), 
                                                           which(colnames(affect_compare)=="diff_valence"),  
                                                           which(colnames(affect_compare)=="audspeclengthL1normsmarange"):which(colnames(affect_compare)=="mfccsmade14stddevFallingSlope"))], 
                               target = "diff_valence")

# arousal difference from baseline
compare_arousal_diff = TaskRegr$new(id = "compare_arousal_diff", 
                               backend = affect_compare[,c(which(colnames(affect_compare)=="user_id"), 
                                                           which(colnames(affect_compare)=="diff_arousal"),  
                                                           which(colnames(affect_compare)=="audspeclengthL1normsmarange"):which(colnames(affect_compare)=="mfccsmade14stddevFallingSlope"))], 
                               target = "diff_arousal")

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

# valence difference from baseline
# Use participant id column as block factor
compare_valence_diff$col_roles$group = "user_id"

# Remove Id from feature space
compare_valence_diff$col_roles$feature = setdiff(compare_valence_diff$col_roles$feature, "user_id")

# arousal difference from baseline

# Use Id column as block factor
compare_arousal_diff$col_roles$group = "user_id"

# Remove Id from feature space
compare_arousal_diff$col_roles$feature = setdiff(compare_arousal_diff$col_roles$feature, "user_id")

#### CREATE LEARNERS ####

lrn_fl = lrn("regr.featureless")
lrn_rf = lrn("regr.ranger", num.trees =1000) # random forest
lrn_rr = lrn("regr.cv_glmnet") # lasso regularized regression

# enable parallelization
set_threads(lrn_fl, n = detectCores())
set_threads(lrn_rr, n = detectCores())
set_threads(lrn_rf, n = detectCores())

#### RESAMPLING ####

resampling = rsmp("cv", folds = 10L)

#### SET PERFORMANCE MEASURES ####

# measures for benchmark
mes = msrs(c("regr.rsq", "regr.srho", "regr.mae"))

### PREPROCESSING IN CV ####

# do a pca in preprocessing 
po_preproc = po("pca", param_vals = list(rank. = 88), scale. = T) # extract 88 dimension from pca

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

## raw valence score

bmgrid_compare_valence = benchmark_grid(
  task = compare_valence,
  learner = list(lrn_fl, lrn_rf_po,  lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = detectCores()) # enable parallelization

bmr_compare_valence = benchmark(bmgrid_compare_valence, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_compare_valence, "study1_ger/results/bmr_compare_valence.RData") # save results

## raw arousal score

bmgrid_compare_arousal = benchmark_grid(
  task = compare_arousal,
  learner = list(lrn_fl, lrn_rf_po,  lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = detectCores()) # enable parallelization

bmr_compare_arousal = benchmark(bmgrid_compare_arousal, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_compare_arousal, "study1_ger/results/bmr_compare_arousal.RData") # save results

## valence difference from baseline

bmgrid_compare_valence_diff = benchmark_grid(
  task = compare_valence_diff,
  learner = list(lrn_fl, lrn_rf_po,  lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = detectCores()) # enable parallelization

bmr_compare_valence_diff = benchmark(bmgrid_compare_valence_diff, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_compare_valence_diff, "study1_ger/results/bmr_compare_valence_diff.RData") # save results

## arousal difference from baseline

bmgrid_compare_arousal_diff = benchmark_grid(
  task = compare_arousal_diff,
  learner = list(lrn_fl, lrn_rf_po,  lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = detectCores()) # enable parallelization

bmr_compare_arousal_diff = benchmark(bmgrid_compare_arousal_diff, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_compare_arousal_diff, "study1_ger/results/bmr_compare_arousal_diff.RData") # save results


#### BENCHMARK RESULTS AND SIGNIFICANCE TESTS ####

# read in benchmark results

bmr_compare_valence <- readRDS("study1_ger/results/bmr_compare_valence.RData")
bmr_compare_arousal <- readRDS("study1_ger/results/bmr_compare_arousal.RData")
bmr_compare_valence_diff <- readRDS("study1_ger/results/bmr_compare_valence_diff.RData")
bmr_compare_arousal_diff <- readRDS("study1_ger/results/bmr_compare_arousal_diff.RData")

# create one overview table for results
mes_compare <- rbind(bmr_compare_valence$aggregate(mes),
                     bmr_compare_valence_diff$aggregate(mes),
                     bmr_compare_arousal$aggregate(mes),
                     bmr_compare_arousal_diff$aggregate(mes)
)


# get performance of learners across cv folds

bmr_results_folds_valence <- bmr_compare_valence$score(
  measures = mes,
  ids = TRUE,
  conditions = FALSE,
  predict_sets = "test"
)

bmr_results_folds_valence_diff <- bmr_compare_valence_diff$score(
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

bmr_results_folds_arousal_diff <- bmr_compare_arousal_diff$score(
  measures = mes,
  ids = TRUE,
  conditions = FALSE,
  predict_sets = "test"
)


# get fl results

fl_folds_valence <- bmr_results_folds_valence %>% 
  filter(learner_id == "regr.featureless")

fl_folds_valence_diff <- bmr_results_folds_valence_diff %>% 
  filter(learner_id == "regr.featureless")

fl_folds_arousal <- bmr_results_folds_arousal %>% 
  filter(learner_id == "regr.featureless")

fl_folds_arousal_diff <- bmr_results_folds_arousal_diff %>% 
  filter(learner_id == "regr.featureless")

# get rf results

rf_folds_valence <- bmr_results_folds_valence %>% 
  filter(learner_id == "pca.regr.ranger")

rf_folds_valence_diff <- bmr_results_folds_valence_diff %>% 
  filter(learner_id == "pca.regr.ranger")

rf_folds_arousal <- bmr_results_folds_arousal %>% 
  filter(learner_id == "pca.regr.ranger")

rf_folds_arousal_diff <- bmr_results_folds_arousal_diff %>% 
  filter(learner_id == "pca.regr.ranger")

# get lasso results

rr_folds_valence <- bmr_results_folds_valence %>% 
  filter(learner_id == "pca.regr.cv_glmnet")

rr_folds_valence_diff <- bmr_results_folds_valence_diff %>% 
  filter(learner_id == "pca.regr.cv_glmnet")

rr_folds_arousal <- bmr_results_folds_arousal %>% 
  filter(learner_id == "pca.regr.cv_glmnet")

rr_folds_arousal_diff <- bmr_results_folds_arousal_diff %>% 
  filter(learner_id == "pca.regr.cv_glmnet")

# run significance tests

n = nrow(affect_compare)

p_rf_valence_rsq <- sign_test_folds(fl_folds_valence$regr.rsq, rf_folds_valence$regr.rsq, n) # execute test featureless vs. rf
p_rr_valence_rsq <- sign_test_folds(fl_folds_valence$regr.rsq, rr_folds_valence$regr.rsq, n) # execute test featureless vs. Elastic Net

p_rf_valence_diff_rsq <- sign_test_folds(fl_folds_valence_diff$regr.rsq, rf_folds_valence_diff$regr.rsq, n) # execute test featureless vs. rf
p_rr_valence_diff_rsq <- sign_test_folds(fl_folds_valence_diff$regr.rsq, rr_folds_valence_diff$regr.rsq, n) # execute test featureless vs. Elastic Net

p_rf_arousal_rsq <- sign_test_folds(fl_folds_arousal$regr.rsq, rf_folds_arousal$regr.rsq, n) # execute test featureless vs. rf
p_rr_arousal_rsq <- sign_test_folds(fl_folds_arousal$regr.rsq, rr_folds_arousal$regr.rsq, n) # execute test featureless vs. Elastic Net

p_rf_arousal_diff_rsq <- sign_test_folds(fl_folds_arousal_diff$regr.rsq, rf_folds_arousal_diff$regr.rsq, n) # execute test featureless vs. rf
p_rr_arousal_diff_rsq <- sign_test_folds(fl_folds_arousal_diff$regr.rsq, rr_folds_arousal_diff$regr.rsq, n) # execute test featureless vs. Elastic Net

# correct for multiple comparison (holm) (n=2)

p_valence_rsq_corrected <- p.adjust(c(p_rf_valence_rsq, p_rr_valence_rsq), method = "holm", n = 8)
p_valence_diff_rsq_corrected <- p.adjust(c(p_rf_valence_diff_rsq, p_rr_valence_diff_rsq), method = "holm", n = 8)
p_arousal_rsq_corrected <- p.adjust(c(p_rf_arousal_rsq, p_rr_arousal_rsq), method = "holm", n = 8)
p_arousal_diff_rsq_corrected <- p.adjust(c(p_rf_arousal_diff_rsq, p_rr_arousal_diff_rsq), method = "holm", n = 8)

# append sign results to measures table

mes_compare$p_rsq <- c(NA, p_valence_rsq_corrected, 
                       NA, p_valence_diff_rsq_corrected, 
                       NA, p_arousal_rsq_corrected, 
                       NA, p_arousal_diff_rsq_corrected)


### VISUALIZE PREDICTION PERFORMANCE ACROSS LEARNERS AND TASKS ####

# rbind results together
bmr_results_folds <- rbind(bmr_results_folds_valence, 
                           bmr_results_folds_valence_diff, 
                           bmr_results_folds_arousal, 
                           bmr_results_folds_arousal_diff)

# add column wwith p values
bmr_results_folds <- base::merge(bmr_results_folds, mes_compare[,c("task_id", "learner_id", "p_rsq")], by = c("task_id", "learner_id"))

# create significance column
bmr_results_folds$significance <- as.factor(ifelse(bmr_results_folds$p_rsq >= 0.05 | is.na(bmr_results_folds$p_rsq), "no", "yes"))

# rename 
bmr_results_folds <- bmr_results_folds %>% 
  mutate(learner_id = case_when(
    learner_id == "regr.featureless" ~    "Baseline",
    learner_id == "pca.regr.ranger" ~ "Random Forest",
    learner_id == "pca.regr.cv_glmnet" ~ "LASSO")) %>% 
  mutate(task_id = case_when(
    task_id == "compare_valence" ~    "Valence",
    task_id == "compare_valence_diff" ~ "Valence Difference",
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

bmr_compare_plot

dev.off()

### FINISH