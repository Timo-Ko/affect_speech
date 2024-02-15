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


