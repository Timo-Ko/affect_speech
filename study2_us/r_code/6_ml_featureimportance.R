### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "data.table", "ranger", "ggplot2", "mlr3", "mlr3learners", "mlr3tuning", "DALEX", "DALEXtra")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

### READ IN DATA ####

# read in data frames
affect_egemaps  <- readRDS("data/affect_egemaps.RData")

#### CREATE AROUSAL TASK ####

# arousal
egemaps_arousal = TaskRegr$new(id = "egemaps_arousal", backend = affect_egemaps[,c(7, 9:ncol(affect_egemaps))], target = "arousal")

#### CREATE LEARNERS ####

lrn_rf = lrn("regr.ranger", num.trees =1000)
lrn_en = lrn("regr.cv_glmnet")

lrn_rf$param_set

# enable parallelization
set_threads(lrn_rf, n = 4)
set_threads(lrn_en, n = 4)

#### PARAMETER TUNING #####

# set parameter space
ps_en <- ParamSet$new(list(
  ParamDbl$new("alpha", lower = 0, upper = 1)
))

ps_rf <- ParamSet$new(list(
  ParamInt$new("mtry", lower = 1, upper = round(sqrt((egemaps_arousal$ncol))))
))

# define tuning
terminator <- trm("evals", n_evals = 10)
tuner <- tnr("random_search")

# resampling
resampling_inner = rsmp("cv", folds = 5L)

# measure for tuning
mes_tuning = msr("regr.mae")

# create autotuner
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

#### TRAIN MODELS #####

future::plan("multisession", workers = 4)

# arousal
model_rf_egemaps_arousal <- lrn_rf$train(egemaps_arousal)
model_en_egemaps_arousal <- lrn_en$train(egemaps_arousal)

# save trained models
saveRDS(model_rf_egemaps_arousal, "results/model_rf_egemaps_arousal.RData")
saveRDS(model_en_egemaps_arousal, "results/model_en_egemaps_arousal.RData")

#### IMPORTANCE OF SINGLE FEATURES: AROUSAL ####

# load model

model_rf_egemaps_arousal <- readRDS( "results/model_rf_egemaps_arousal.RData")

# arousal
rf_exp_arousal <- explain_mlr3(model_rf_egemaps_arousal,
                               data     = affect_egemaps[,c(7, 9:ncol(affect_egemaps))],
                               y        = affect_egemaps$arousal,
                               label    = "Ranger RF",
                               colorize = FALSE)

# permutation importance
importance_rf_arousal <- DALEX::model_parts(explainer = rf_exp_arousal)
head(importance_rf_arousal )

importance_rf_arousal_plot <- plot(importance_rf_arousal , max_vars = 10, show_boxplots = FALSE)

# save plot

# save plots

png(file="figures/importance_rf_arousal_plot.png",width=500, height=500)

importance_rf_arousal_plot 

dev.off()

# save results
saveRDS(importance_rf_arousal, "results/importance_rf_arousal.RData")

#### IMPORTANCE OF FEATURE GROUPS: AROUSAL ####

# load feature importance for rf arousal predictions
model_rf_egemaps_arousal <- readRDS( "results/model_rf_egemaps_arousal.RData")

# load table with feature groups 
egemaps_feature_groups <- read.csv2("data/egemaps_feature_groups.csv", header = T)

# add column with feature importance for arousal in rf model
egemaps_feature_groups_importance <- base::merge(egemaps_feature_groups, importance_rf_arousal[c(1:100),c(1,2)] , by.x = "feature", by.y = "variable" )

## compute grouped feature importance
# this part of the script is run using mlr and not mlr3!
# original code see github: https://github.com/JuliaHerbinger/grouped_feat_imp_and_effects

library(mlrCPO)
library(future.apply)
library(dplyr)
library(mlr)

source("r_code/functions/gimp.R") # load functions

# task = personalityTasks$C #choose Task (E, C, or O are ok)
# target = getTaskTargetNames(task)
# data = getTaskData(task)
# data = mlr::impute(data, classes = list(integer = imputeMedian(), factor = imputeMode(), numeric = imputeMedian()))$data
# data[, which(colnames(data) != target)] = scale(data[, which(colnames(data) != target)])

#learnerRF = makeLearner("regr.ranger")
#task = makeRegrTask(data = data, target = target)
#mod = train(learner = learnerRF, task = task)

# create task in mlr

egemaps_arousal <- makeRegrTask(id = "egemaps_arousal",
                         data = affect_egemaps[,c(7, 9:ncol(affect_egemaps))],
                         target = "arousal")


resampling_outer <- mlr::makeResampleDesc("CV", iters = 10)

lrn_rf <- makeLearner("regr.ranger",  par.vals = list("num.trees" = 1000)) 

lrn_rf <- makeLearner("regr.ranger") 

configureMlr(on.learner.error = "warn")

# do i need a wrapped learner here??
res = mlr::resample (task = egemaps_arousal, learner = lrn_rf, resampling = resampling_outer, models = T)
# , store_models = TRUE, this part has been removed 
gimp = Gimp$new(task = egemaps_arousal, res = res, mod = model_rf_egemaps_arousal, lrn = lrn_rf)

# get single features from category table and their respective group
cat_table = egemaps_feature_groups_importance %>% dplyr::select(feature, lld) %>% distinct(feature, .keep_all = TRUE)
features = egemaps_arousal$feature_names %>% data.frame()
colnames(features) = "feature"
cat_table = features %>% left_join(cat_table)
colnames(cat_table)[2] = "group"
group_df = cat_table

## compute grouped feature importance measures

#gpfi
gpfi = gimp$group_permutation_feat_imp(cat_table, s = 50, n.feat.perm = 10, regr.measure = mse)

#gopfi
gopfi = gimp$group_only_permutation_feat_imp(cat_table, s = 50, n.feat.perm = 10, regr.measure = mse)

# dgi
dgi = gimp$drop_group_importance(cat_table, resampling = cv10, measures = mse)

# goi
goi = gimp$group_only_importance(cat_table, resampling = cv10, measures = mse)

# shapley
shap = gimp$shapley(group_df = cat_table, res = res, n.shapley.perm = 120)

## save results
saveRDS(gpfi, "results/gpfis_arousal.RData")

## create plots


# This script creates Figure 5 of the paper (section 3.3)

library(ggplot2)
library(gridExtra)

# parameters
path = "results/simulation_results/sim_sparsity/results/"
vec = 1:20

# create Figure 5: Comparison of Shapley importance on group and feature level
df_shap = get_shapley_imp(vec, path)
shap_feat = df_shap[[1]]
shap_group = df_shap[[2]]


p1 = ggplot(shap_group, aes(x = group, y = mse, fill = group)) + geom_boxplot() +
  scale_x_discrete(breaks = c("G1", "G2"), labels = c(expression("G"[1], "G"[2]))) +
  labs(x = "Group", y = "MSE", fill = "Group") +
  scale_fill_discrete(labels = c(expression("G"[1]), expression("G"[2])))

p2 = ggplot(shap_feat, aes(x = feature, y = mse, fill = group)) + geom_boxplot() +
  scale_x_discrete(breaks = paste0("V", 1:8), labels = c(expression("X"[1]), expression("X"[2]),
                                                         expression("X"[3]), expression("X"[4]),
                                                         expression("X"[5]), expression("X"[6]),
                                                         expression("X"[7]), expression("X"[8]))) +
  labs(x = "Feature", y = "MSE", fill = "Group") +
  scale_fill_discrete(labels = c(expression("G"[1]), expression("G"[2])))


p = gridExtra::grid.arrange(p1, p2, nrow = 1)
ggsave("results/figures/sim_varying_size_shap.png", p, width = 9, height = 5)


## FINISH