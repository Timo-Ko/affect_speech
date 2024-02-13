### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "parallel", "data.table", "ggplot2", "mlr3", "mlr3learners", "mlr3tuning","ranger", "glmnet", "future", "remotes", "stringr")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

set.seed(123, kind = "L'Ecuyer") # set seed to make sure all results are reproducible

### READ IN DATA ####

# read in data frames
affect_egemaps  <- readRDS("study1_ger/data/affect_egemaps.RData")

# load required functions
source("study1_ger/r_code/functions/bmr_results.R")
source("study1_ger/r_code/functions/sign_test_folds.R")

# remove all punctuation in colnames for ml 
names(affect_egemaps)[16:length(names(affect_egemaps))] <- str_replace_all(names(affect_egemaps)[16:length(names(affect_egemaps))], "[:punct:]", "")

#### CREATE TASKS ####

## benchmark check predictions

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
lrn_rf = lrn("regr.ranger", 
             mtry = to_tune(1, 50),
             num.trees =500) # random forest
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

## benchmark check predictions

# age
bmgrid_egemaps_age = benchmark_grid(
  task = c(egemaps_age),
  learner = list(lrn_fl, lrn_rf, lrn_rr),
  resampling = resampling
)

future::plan("multisession", workers = 5) # enable parallelization

bmr_egemaps_age = benchmark(bmgrid_egemaps_age, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_egemaps_age, "study1_ger/results/bmr_egemaps_age.RData") # save results

# gender
bmgrid_egemaps_gender = benchmark_grid(
  task = c(egemaps_gender),
  learner = list(lrn("classif.featureless", predict_type = "prob"), lrn("classif.ranger", num.trees =1000, predict_type = "prob"), lrn("classif.cv_glmnet", predict_type = "prob")),
  resampling = resampling
)

future::plan("multisession", workers = 5) # enable parallelization

bmr_egemaps_gender = benchmark(bmgrid_egemaps_gender, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_egemaps_gender, "study1_ger/results/bmr_egemaps_gender.RData") # save results

## momentary affect experience

bmgrid_egemaps = benchmark_grid(
  task = c(egemaps_valence,
           #egemaps_valence_diff,
           egemaps_arousal#,
           #egemaps_arousal_diff
           ),
  learner = list(lrn_fl, at_rf, at_rr),
  resampling = resampling
)

future::plan("multisession", workers = 5) # enable parallelization

bmr_egemaps = benchmark(bmgrid_egemaps, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_egemaps, "study1_ger/results/bmr_egemaps.RData") # save results

#### BENCHMARK RESULTS AND SIGNIFICANCE TESTS ####

# read in benchmark results
bmr_egemaps_age <- readRDS("study1_ger/results/bmr_egemaps_age.RData")
bmr_egemaps_gender <- readRDS("study1_ger/results/bmr_egemaps_gender.RData")
bmr_egemaps <- readRDS("study1_ger/results/bmr_egemaps.RData")

## view aggregated performance
bmr_egemaps_age$aggregate(mes)
bmr_egemaps_gender$aggregate(msrs(c("classif.acc", "classif.auc")))
bmr_egemaps$aggregate(mes)

## retrieve benchmark results across tasks and learners for single cv folds (this is needed for barplots)
bmr_results_folds <- extract_bmr_results(bmr_egemaps, mes)

# create overview table of performance incl. significance tests
pred_table <- results_table(affect_egemaps, bmr_results_folds)

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
  mutate(task_id = case_when(
    task_id == "egemaps_valence" ~    "Valence",
    task_id == "egemaps_valence_diff" ~ "Valence Fluctuation",
    task_id == "egemaps_arousal" ~ "Arousal",
    task_id == "egemaps_arousal_diff" ~ "Arousal Fluctuation"))

# create figure

bmr_egemaps_plot <- ggplot(bmr_results_folds, aes(x= factor(task_id, levels = c("Arousal Fluctuation","Arousal", "Valence Fluctuation",  "Valence")) , y= regr.rsq, color = significance, shape = learner_id)) + 
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


### FINISH