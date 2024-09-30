### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "parallel", "data.table", "ggplot2", "ggtext", "mlr3", "mlr3verse", "mlr3learners", "mlr3tuning","ranger", "glmnet", "future", "remotes", "bbotk", "patchwork")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

set.seed(123, kind = "L'Ecuyer") # set seed to make sure all results are reproducible

# load required functions
source("code/functions/bmr_results.R")
source("code/functions/sign_test_folds.R")

### READ IN DATA ####

# study 1
affect_voice_study1  <- readRDS("data/study1/affect_voice_study1_ml.rds")

# remove illegal characters from colnames 
colnames(affect_voice_study1) <- make.names(colnames(affect_voice_study1), unique = TRUE)

# study 2
affect_voice_wordembeddings_study2 <- readRDS("data/study2/affect_voice_wordembeddings_study2_ml.rds")

# remove illegal characters from colnames 
colnames(affect_voice_wordembeddings_study2) <- make.names(colnames(affect_voice_wordembeddings_study2), unique = TRUE)

#### CREATE TASKS: STUDY 1 ####

# raw valence score 
egemaps_valence_study1 <- TaskRegr$new(
  id = "egemaps_valence", 
  backend = affect_voice_study1 %>%
    select(user_id, valence, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp),
  target = "valence"
)

# raw arousal score
egemaps_arousal_study1 <- TaskRegr$new(
  id = "egemaps_arousal", 
  backend = affect_voice_study1 %>%
    select(user_id, arousal, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp),
  target = "arousal"
)

## supplementary predictions: predictions per sentence condition

egemaps_valence_poscond_study1 <- TaskRegr$new(
  id = "egemaps_valence",
  backend = affect_voice_study1 %>%
    filter(condition == "positive") %>%
    select(user_id, valence, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp),
  target = "valence"
)

egemaps_valence_neucond_study1 <- TaskRegr$new(
  id = "egemaps_valence", 
  backend = affect_voice_study1 %>%
    filter(condition == "neutral") %>%
    select(user_id, valence, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp),
  target = "valence"
)

egemaps_valence_negcond_study1 <- TaskRegr$new(
  id = "egemaps_valence", 
  backend = affect_voice_study1 %>%
    filter(condition == "negative") %>%
    select(user_id, valence, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp),
  target = "valence"
)

egemaps_arousal_poscond_study1 <- TaskRegr$new(
  id = "egemaps_arousal",
  backend = affect_voice_study1 %>%
    filter(condition == "positive") %>%
    select(user_id, arousal, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp),
  target = "arousal"
)

egemaps_arousal_neucond_study1 <- TaskRegr$new(
  id = "egemaps_arousal", 
  backend = affect_voice_study1 %>%
    filter(condition == "neutral") %>%
    select(user_id, arousal, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp),
  target = "arousal"
)

egemaps_arousal_negcond_study1 <- TaskRegr$new(
  id = "egemaps_arousal", 
  backend = affect_voice_study1 %>%
    filter(condition == "negative") %>%
    select(user_id, arousal, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp),
  target = "arousal"
)

## supplementary predictions: fluctuation from baseline

# valence fluctuation from baseline
egemaps_valence_diff_study1 <- TaskRegr$new(
  id = "egemaps_valence_diff", 
  backend = affect_voice_study1 %>%
    select(user_id, diff_valence, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp),
  target = "diff_valence"
)

# arousal fluctuation from baseline
egemaps_arousal_diff_study1 <- TaskRegr$new(
  id = "egemaps_arousal_diff", 
  backend = affect_voice_study1 %>%
    select(user_id, diff_arousal, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp),
  target = "diff_arousal"
)

## supplementary predictions: compare feature set

# valence
compare_valence_study1 <- TaskRegr$new(
  id = "compare_valence", 
  backend = affect_voice_study1 %>%
    select(user_id, valence, audspec_lengthL1norm_sma_range:mfcc_sma_de.14._stddevFallingSlope),
  target = "valence"
)

# arousal
compare_arousal_study1 <- TaskRegr$new(
  id = "compare_arousal", 
  backend = affect_voice_study1 %>%
    select(user_id, arousal, audspec_lengthL1norm_sma_range:mfcc_sma_de.14._stddevFallingSlope),
  target = "arousal"
)

# supplementary prediction: gender
affect_voice_gender_study1 <- affect_voice_study1 %>%
  filter(!is.na(Demo_GE1)) %>% # remove rows with missing gender data
  mutate(Demo_GE1 = as.factor(Demo_GE1)) # convert gender to factor

egemaps_gender_study1 <- TaskClassif$new(
  id = "egemaps_gender_study1", 
  backend = affect_voice_gender_study1 %>%
    select(user_id, Demo_GE1, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp), 
  target = "Demo_GE1"
)

## add blocking

# raw valence score
egemaps_valence_study1$col_roles$group = "user_id"
egemaps_valence_study1$col_roles$feature = setdiff(egemaps_valence_study1$col_roles$feature, "user_id")

# raw arousal score
egemaps_arousal_study1$col_roles$group = "user_id"
egemaps_arousal_study1$col_roles$feature = setdiff(egemaps_arousal_study1$col_roles$feature, "user_id")

# supplementary analyses

# raw valence score - positive sentences
egemaps_valence_poscond_study1$col_roles$group = "user_id"
egemaps_valence_poscond_study1$col_roles$feature = setdiff(egemaps_valence_poscond_study1$col_roles$feature, "user_id")

# raw valence score - neutral sentences
egemaps_valence_neucond_study1$col_roles$group = "user_id"
egemaps_valence_neucond_study1$col_roles$feature = setdiff(egemaps_valence_neucond_study1$col_roles$feature, "user_id")

# raw valence score - negative sentences
egemaps_valence_negcond_study1$col_roles$group = "user_id"
egemaps_valence_negcond_study1$col_roles$feature = setdiff(egemaps_valence_negcond_study1$col_roles$feature, "user_id")

# raw arousal score - positive sentences
egemaps_arousal_poscond_study1$col_roles$group = "user_id"
egemaps_arousal_poscond_study1$col_roles$feature = setdiff(egemaps_arousal_poscond_study1$col_roles$feature, "user_id")

# raw arousal score - neutral sentences
egemaps_arousal_neucond_study1$col_roles$group = "user_id"
egemaps_arousal_neucond_study1$col_roles$feature = setdiff(egemaps_arousal_neucond_study1$col_roles$feature, "user_id")

# raw arousal score - negative sentences
egemaps_arousal_negcond_study1$col_roles$group = "user_id"
egemaps_arousal_negcond_study1$col_roles$feature = setdiff(egemaps_arousal_negcond_study1$col_roles$feature, "user_id")


# valence difference from baseline
egemaps_valence_diff_study1$col_roles$group = "user_id"
egemaps_valence_diff_study1$col_roles$feature = setdiff(egemaps_valence_diff_study1$col_roles$feature, "user_id")

# arousal difference from baseline
egemaps_arousal_diff_study1$col_roles$group = "user_id"
egemaps_arousal_diff_study1$col_roles$feature = setdiff(egemaps_arousal_diff_study1$col_roles$feature, "user_id")

# valence, compare feature set
compare_valence_study1$col_roles$group = "user_id"
compare_valence_study1$col_roles$feature = setdiff(compare_valence_study1$col_roles$feature, "user_id")

# arousal, compare feature set
compare_arousal_study1$col_roles$group = "user_id"
compare_arousal_study1$col_roles$feature = setdiff(compare_arousal_study1$col_roles$feature, "user_id")

# gender
egemaps_gender_study1$col_roles$group = "user_id"
egemaps_gender_study1$col_roles$feature = setdiff(egemaps_gender_study1$col_roles$feature, "user_id")

#### CREATE TASKS: STUDY 2 ####

# raw contentedness score
egemaps_content_study2 <- TaskRegr$new(
  id = "egemaps_content", 
  backend = affect_voice_wordembeddings_study2 %>%
    select(user_id, content, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp),
  target = "content"
)

# raw sadness score
egemaps_sad_study2 <- TaskRegr$new(
  id = "egemaps_sad", 
  backend = affect_voice_wordembeddings_study2 %>%
    select(user_id, sad, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp),
  target = "sad"
)

# raw arousal score
egemaps_arousal_study2 <- TaskRegr$new(
  id = "egemaps_arousal", 
  backend = affect_voice_wordembeddings_study2 %>%
    select(user_id, arousal, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp),
  target = "arousal"
)

## supplementary analyses: fluctuations

# content fluctuation from baseline
egemaps_content_diff_study2 <- TaskRegr$new(
  id = "egemaps_content_diff", 
  backend = affect_voice_wordembeddings_study2 %>%
    select(user_id, diff_content, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp),
  target = "diff_content"
)

# sad fluctuation from baseline
egemaps_sad_diff_study2 <- TaskRegr$new(
  id = "egemaps_sad_diff", 
  backend = affect_voice_wordembeddings_study2 %>%
    select(user_id, diff_sad, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp),
  target = "diff_sad"
)

# arousal fluctuation from baseline
egemaps_arousal_diff_study2 <- TaskRegr$new(
  id = "egemaps_arousal_diff", 
  backend = affect_voice_wordembeddings_study2 %>%
    select(user_id, diff_arousal, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp),
  target = "diff_arousal"
)

## supplementary analyses: compare feature set

# raw contentedness score
compare_content_study2 <- TaskRegr$new(
  id = "compare_content", 
  backend = affect_voice_wordembeddings_study2 %>%
    select(user_id, content, audspec_lengthL1norm_sma_range:mfcc_sma_de.14._stddevFallingSlope),
  target = "content"
)

# raw sadness score
compare_sad_study2 <- TaskRegr$new(
  id = "compare_sad", 
  backend = affect_voice_wordembeddings_study2 %>%
    select(user_id, sad, audspec_lengthL1norm_sma_range:mfcc_sma_de.14._stddevFallingSlope),
  target = "sad"
)

# raw arousal score
compare_arousal_study2 <- TaskRegr$new(
  id = "compare_arousal", 
  backend = affect_voice_wordembeddings_study2 %>%
    select(user_id, arousal, audspec_lengthL1norm_sma_range:mfcc_sma_de.14._stddevFallingSlope),
  target = "arousal"
)

# supplementary analysis: gender
affect_voice_wordembeddings_gender_study2 <- affect_voice_wordembeddings_study2 %>%
  filter(!is.na(Gender)) %>% # create new df with no missing data for gender
  mutate(Gender = as.factor(Gender)) # convert gender to factor

egemaps_gender_study2 <- TaskClassif$new(
  id = "egemaps_gender", 
  backend = affect_voice_wordembeddings_gender_study2 %>%
    select(user_id, Gender, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp), 
  target = "Gender"
)

## word embeddings 

# raw contentedness score
wordembeddings_content <- TaskRegr$new(
  id = "wordembeddings_content", 
  backend = affect_voice_wordembeddings_study2 %>%
    select(user_id, content, Dim1_texts:Dim1024_texts),
  target = "content"
)

# raw sadness score
wordembeddings_sad <- TaskRegr$new(
  id = "wordembeddings_sad", 
  backend = affect_voice_wordembeddings_study2 %>%
    select(user_id, sad, Dim1_texts:Dim1024_texts),
  target = "sad"
)

# raw arousal score
wordembeddings_arousal <- TaskRegr$new(
  id = "wordembeddings_arousal", 
  backend = affect_voice_wordembeddings_study2 %>%
    select(user_id, arousal, Dim1_texts:Dim1024_texts),
  target = "arousal"
)

## egemaps + word embeddings 

# raw contentedness score
egemaps_wordembeddings_content <- TaskRegr$new(
  id = "egemaps_wordembeddings_content", 
  backend = affect_voice_wordembeddings_study2 %>%
    select(user_id, content, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp, 
           Dim1_texts:Dim1024_texts),
  target = "content"
)

# raw sadness score
egemaps_wordembeddings_sad <- TaskRegr$new(
  id = "egemaps_wordembeddings_sad", 
  backend = affect_voice_wordembeddings_study2 %>%
    select(user_id, sad, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp, 
           Dim1_texts:Dim1024_texts),
  target = "sad"
)

# raw arousal score
egemaps_wordembeddings_arousal <- TaskRegr$new(
  id = "egemaps_wordembeddings_arousal", 
  backend = affect_voice_wordembeddings_study2 %>%
    select(user_id, arousal, F0semitoneFrom27.5Hz_sma3nz_amean:equivalentSoundLevel_dBp, 
           Dim1_texts:Dim1024_texts),
  target = "arousal"
)

## add blocking

# gender
egemaps_gender_study2$col_roles$group = "user_id"
egemaps_gender_study2$col_roles$feature = setdiff(egemaps_gender_study2$col_roles$feature, "user_id")

# raw content score
egemaps_content_study2$col_roles$group = "user_id"
egemaps_content_study2$col_roles$feature = setdiff(egemaps_content_study2$col_roles$feature, "user_id")

# raw sad score
egemaps_sad_study2$col_roles$group = "user_id"
egemaps_sad_study2$col_roles$feature = setdiff(egemaps_sad_study2$col_roles$feature, "user_id")

# raw arousal score
egemaps_arousal_study2$col_roles$group = "user_id"
egemaps_arousal_study2$col_roles$feature = setdiff(egemaps_arousal_study2$col_roles$feature, "user_id")

# content difference from baseline
egemaps_content_diff_study2$col_roles$group = "user_id"
egemaps_content_diff_study2$col_roles$feature = setdiff(egemaps_content_diff_study2$col_roles$feature, "user_id")

# sad difference from baseline
egemaps_sad_diff_study2$col_roles$group = "user_id"
egemaps_sad_diff_study2$col_roles$feature = setdiff(egemaps_sad_diff_study2$col_roles$feature, "user_id")

# arousal difference from baseline
egemaps_arousal_diff_study2$col_roles$group = "user_id"
egemaps_arousal_diff_study2$col_roles$feature = setdiff(egemaps_arousal_diff_study2$col_roles$feature, "user_id")

# raw content score, compare feature set
compare_content_study2$col_roles$group = "user_id"
compare_content_study2$col_roles$feature = setdiff(compare_content_study2$col_roles$feature, "user_id")

# raw sad score, compare feature set
compare_sad_study2$col_roles$group = "user_id"
compare_sad_study2$col_roles$feature = setdiff(compare_sad_study2$col_roles$feature, "user_id")

# raw arousal score, compare feature set
compare_arousal_study2$col_roles$group = "user_id"
compare_arousal_study2$col_roles$feature = setdiff(compare_arousal_study2$col_roles$feature, "user_id")

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

# egemaps + word embeddings 

# raw content 
egemaps_wordembeddings_content$col_roles$group = "user_id"
egemaps_wordembeddings_content$col_roles$feature = setdiff(egemaps_wordembeddings_content$col_roles$feature, "user_id")

# raw sad score
egemaps_wordembeddings_sad$col_roles$group = "user_id"
egemaps_wordembeddings_sad$col_roles$feature = setdiff(egemaps_wordembeddings_sad$col_roles$feature, "user_id")

# raw arousal score
egemaps_wordembeddings_arousal$col_roles$group = "user_id"
egemaps_wordembeddings_arousal$col_roles$feature = setdiff(egemaps_wordembeddings_arousal$col_roles$feature, "user_id")

#### CREATE LEARNERS ####

lrn_fl = lrn("regr.featureless")
lrn_rf = lrn("regr.ranger", 
             num.trees = 1000) # random forest with 1,000 trees

# Implement Elastic Net using cv_glmnet
lrn_en = lrn("regr.cv_glmnet", 
             alpha = 0.5)  # Adjust alpha (0.5 for equal mix of L1 and L2)

### PREPROCESSING IN CV ####

po_impute_hist = po("imputehist") # hist imputation
po_impute_oor = po("imputeoor")   # out of range imputation

# combine training with pre-processing
lrn_rf_po = po_impute_oor %>>% lrn_rf
lrn_en_po = po_impute_hist %>>% lrn_en

#### RESAMPLING ####

# repeated cv
repeated_cv = rsmp("repeated_cv", repeats = 5, folds = 10)

#### BENCHMARK: STUDY 1 ####

# avoid console output from mlr3tuning
logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")

# show progress
progressr::handlers(global = TRUE)
progressr::handlers("progress")

## momentary affect experience

bmgrid_study1 = benchmark_grid(
  task = list(egemaps_valence_study1,
           egemaps_arousal_study1,
           egemaps_valence_poscond_study1, # supplementary analyses: separate conditions
           egemaps_valence_neucond_study1,
           egemaps_valence_negcond_study1,
           egemaps_valence_diff_study1, # supplementary analyses: affect fluctuation
           egemaps_arousal_diff_study1,
           compare_valence_study1, # supplementary analyses: compare2016 feature set
           compare_arousal_study1
  ),
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = repeated_cv
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_study1 = benchmark(bmgrid_study1, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_study1, "results/study1/bmr_study1.rds") # save results

# supplementary prediction: gender

# create classification learner 
lrn_classif_rf_po = po_impute_oor  %>>% lrn("classif.ranger", num.trees =1000, predict_type = "prob")
lrn_classif_rr_po = po_impute_hist  %>>% lrn("classif.cv_glmnet", predict_type = "prob")

bmgrid_egemaps_gender_study1 = benchmark_grid(
  task = c(egemaps_gender_study1),
  learner = list(lrn("classif.featureless", predict_type = "prob"), lrn_classif_rf_po, lrn_classif_rr_po ),
  resampling = repeated_cv
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_egemaps_gender_study1 = benchmark(bmgrid_egemaps_gender_study1, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_egemaps_gender_study1, "results/study1/bmr_egemaps_gender_study1.rds") # save results

#### BENCHMARK: STUDY 2 ####

# avoid console output from mlr3tuning
logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")

# show progress
progressr::handlers(global = TRUE)
progressr::handlers("progress")

## momentary affect experience

bmgrid_study2 = benchmark_grid(
  task = c(egemaps_arousal_study2, # main analyses: voice
           egemaps_content_study2,
           egemaps_sad_study2, 
           egemaps_arousal_diff_study2,  # supplementary analyses: affect fluctuation
           egemaps_content_diff_study2,
           egemaps_sad_diff_study2,
           compare_arousal_study2, # supplementary analyses: compare2016 feature set
           compare_content_study2,
           compare_sad_study2,
           wordembeddings_arousal, # main analses: semantic content (wordembeddings)
           wordembeddings_content,
           wordembeddings_sad, 
           egemaps_wordembeddings_arousal, # supplementary analyses: wordembeddings and voice combined
           egemaps_wordembeddings_content,
           egemaps_wordembeddings_sad
           ),
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = repeated_cv
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_study2 = benchmark(bmgrid_study2, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_study2, "results/study2/bmr_study2.rds") # save results


## supplementary prediction: gender
bmgrid_egemaps_gender_study2 = benchmark_grid(
  task = egemaps_gender_study2,
  learner = list(lrn("classif.featureless", predict_type = "prob"), lrn_classif_rf_po,lrn_classif_rr_po ),
  resampling = repeated_cv
)

future::plan("multisession", workers = 10) # enable parallelization

bmr_egemaps_gender_study2 = benchmark(bmgrid_egemaps_gender_study2, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_egemaps_gender_study2, "results/study2/bmr_egemaps_gender_study2.rds") # save results

#### DEEP DIVE BENCHMARK RESULTS AND SIGNIFICANCE TESTS ####

## read in benchmark results

# study 1
bmr_study1 <- readRDS("results/study1/bmr_study1.rds")

bmr_egemaps_gender_study1 <- readRDS("results/study1/bmr_egemaps_gender_study1.rds")

# study 2
bmr_study2 <- readRDS("results/study2/bmr_study2.rds")

bmr_egemaps_gender_study2 <- readRDS("results/study2/bmr_egemaps_gender_study2.rds")

## view aggregated performance 

## modify rho to handle NAs and aggregate to the median 
md_rho = msr("regr.srho")
md_rho$aggregator = function(x) median(x, na.rm = TRUE)

md_rsq = msr("regr.rsq")
md_rsq$aggregator = function(x) median(x, na.rm = TRUE)

md_rmse = msr("regr.rmse")
md_rmse$aggregator = function(x) median(x, na.rm = TRUE)

md_mae = msr("regr.mae")
md_mae$aggregator = function(x) median(x, na.rm = TRUE)

mes = c(md_rho, md_rsq, md_rmse, md_mae) # set performance measures

# study 1
bmr_study1$aggregate(mes)

bmr_egemaps_gender_study1$aggregate(msr("classif.acc"))

# study 2
bmr_study2$aggregate(mes)

bmr_egemaps_gender_study2$aggregate(msr("classif.acc"))

## deep dive: retrieve benchmark results across tasks and learners for single cv folds

# study 1
bmr_results_folds_study1 <- extract_bmr_results(bmr_study1, mes)

# study 2
bmr_results_folds_study2 <- extract_bmr_results(bmr_study2, mes)

## create combined overview table of performance incl. significance tests
pred_table_study1 <- results_table(affect_voice_study1, bmr_results_folds_study1)

pred_table_study2 <- results_table(affect_voice_wordembeddings_study2, bmr_results_folds_study2)

# correct p values for multiple comparison for main analyses 
combined_p_values <- c(pred_table_study1$p_rsq, pred_table_study2$p_rsq)

# Apply Holm correction
adjusted_p_values <- p.adjust(combined_p_values, method = "holm")

# Split the adjusted p-values back into their respective tables
pred_table_study1$p_adj <- adjusted_p_values[1:nrow(pred_table_study1)]
pred_table_study2$p_adj <- adjusted_p_values[(nrow(pred_table_study1) + 1):length(adjusted_p_values)]

# save prediction tables
write.csv2(pred_table_study1, "results/pred_table_study1.csv")
write.csv2(pred_table_study2, "results/pred_table_study2.csv")

## create plots

# subset relevant tasks for plotting 
bmr_results_folds_study1_plot <- bmr_results_folds_study1 %>% filter(task_id %in% c("egemaps_valence", "egemaps_arousal")) # only keep relevant main tasks 

bmr_results_folds_study2_plot <- bmr_results_folds_study2 %>% filter(task_id %in% c("egemaps_content", "egemaps_sad", "egemaps_arousal",
                                                                               "wordembeddings_content", "wordembeddings_sad", "wordembeddings_arousal",
                                                                               "egemaps_wordembeddings_content", "egemaps_wordembeddings_sad", "egemaps_wordembeddings_arousal"))  # only keep relevant main tasks 

# rename

bmr_results_folds_study1_plot <- bmr_results_folds_study1_plot  %>% 
  mutate(study = "study1") %>% 
  mutate(learner_id = case_when(
    learner_id == "regr.featureless" ~    "Baseline",
    learner_id == "imputeoor.regr.ranger" ~ "Random Forest",
    learner_id == "imputehist.regr.cv_glmnet" ~ "Elastic Net")) %>% 
  mutate(feature_set = case_when(
    task_id == "egemaps_valence" ~    "Voice Acoustics",
    task_id == "egemaps_arousal" ~    "Voice Acoustics")) %>% 
  mutate(task_id = case_when(
    task_id == "egemaps_valence" ~    "Valence",
    task_id == "egemaps_arousal" ~ "Arousal"))

bmr_results_folds_study2_plot <- bmr_results_folds_study2_plot %>% 
  filter(!task_id %in% c("egemaps_arousal_diff", "egemaps_content_diff", "egemaps_sad_diff",
                      "wordembeddings_arousal_diff", "wordembeddings_content_diff", "wordembeddings_sad_diff",
                      "egemaps_wordembeddings_arousal_diff", "egemaps_wordembeddings_content_diff", "egemaps_wordembeddings_sad_diff",
                      "egemaps_wordembeddings_arousal", "egemaps_wordembeddings_content", "egemaps_wordembeddings_sad")) %>% 
  mutate(study = "study2") %>% 
  mutate(learner_id = case_when(
    learner_id == "regr.featureless" ~    "Baseline",
    learner_id == "imputeoor.regr.ranger" ~ "Random Forest",
    learner_id == "imputehist.regr.cv_glmnet" ~ "Elastic Net")) %>% 
  mutate(feature_set = case_when(
    task_id == "egemaps_content" ~    "Voice Acoustics",
    task_id == "egemaps_sad" ~    "Voice Acoustics",
    task_id == "egemaps_arousal" ~ "Voice Acoustics",
    task_id == "wordembeddings_content" ~    "Word Embeddings",
    task_id == "wordembeddings_sad" ~    "Word Embeddings",
    task_id == "wordembeddings_arousal" ~ "Word Embeddings")) %>% 
  mutate(task_id = case_when(
    task_id == "egemaps_content" ~    "Contentment",
    task_id == "egemaps_sad" ~    "Sadness",
    task_id == "egemaps_arousal" ~ "Arousal",
    task_id == "wordembeddings_content" ~    "Contentment",
    task_id == "wordembeddings_sad" ~    "Sadness",
    task_id == "wordembeddings_arousal" ~ "Arousal")) %>% 
  mutate(regr.srho = if_else(learner_id == "Elastic Net" & is.na(regr.srho), 0, regr.srho)) # replace NA with zero for EN

# rbind both studies
bmr_results_folds <- rbind(bmr_results_folds_study1_plot, bmr_results_folds_study2_plot)

# find combinations of task, learner, and study
existing_combinations <- unique(interaction(bmr_results_folds$task_id, bmr_results_folds$feature_set, bmr_results_folds$study))

# Define only the combinations that exist in the data
correct_levels <- rev(c(
  "Valence.Voice Acoustics.study1",
  "Arousal.Voice Acoustics.study1",
  "Contentment.Voice Acoustics.study2",
  "Sadness.Voice Acoustics.study2",
  "Arousal.Voice Acoustics.study2",
  "Contentment.Word Embeddings.study2",
  "Sadness.Word Embeddings.study2",
  "Arousal.Word Embeddings.study2"
))

# Keep only the levels that exist in the data
correct_levels <- correct_levels[correct_levels %in% existing_combinations]

# create four figures with main results - two columns for performance measures and separated by study (study 1 on top then study 2 below), Pearson r on the left and r2 on the right, sign pred in bold

bmr_plot_srho <-
  ggplot(
    bmr_results_folds,
    aes(
      x = factor(
        interaction(task_id, feature_set, study),
        levels = correct_levels,
        ) ,
      y = regr.srho,
      color = learner_id
      #shape = learner_id)
  )) +
  geom_boxplot(
    width = 0.3,
    lwd = 1,
    aes(color = learner_id),
    alpha = 0.3,
    position = position_dodge(0.5)
  ) +
  scale_x_discrete(
    element_blank(),
    labels = rev(c(
      "Valence (Prosody)",
      "**Arousal (Prosody)**",
      "**Contentment (Prosody)**",
      "Sadness (Prosody)",
      "**Arousal (Prosody)**",
      "**Contentment (Semantics)**",
      "**Sadness (Semantics)**",
      "**Arousal (Semantics)**"
    )
    )) +  
  scale_color_manual(values = c("#a6cee3", "#1f78b4"), name = "Algorithm", labels = c("Elastic Net", "Random Forest")) +
  scale_y_continuous(name = bquote("Spearman correlation (r)"), limits = c(-0.1, 0.5)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  theme_minimal(base_size = 25) +
  labs(colour = "Algorithm") + # change legend title
  theme(axis.text.x = element_text(angle = -45, hjust = 0)) + # rotate x axis labels
  coord_flip() + # flip coordinates
  guides(color = guide_legend(reverse = TRUE),
         shape = guide_legend(reverse = TRUE)) +
  theme(axis.text.x = element_markdown(), 
        axis.text.y = element_markdown(hjust = 0),
        legend.position = "top", 
        legend.key.size = unit(1.5, "cm"))

# plot rsq
bmr_plot_rsq <-
  ggplot(bmr_results_folds, aes(
    x = factor(
      interaction(task_id, feature_set, study),
      levels = correct_levels
    ),
    y = regr.rsq,
    color = learner_id
  )) +
  geom_boxplot(
    width = 0.3,
    lwd = 1,
    aes(color = learner_id),
    alpha = 0.3,
    position = position_dodge(0.5)
  ) +
  scale_color_manual(values = c("#b2df8a", "#a6cee3", "#1f78b4" ), name = "Algorithm", labels = c("Baseline", "Elastic Net", "Random Forest")) +
  scale_y_continuous(name = bquote(paste(italic(R) ^ 2)), limits = c(-0.15, 0.15)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  theme_minimal(base_size = 25) +
  labs(colour = "Algorithm") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0), axis.text.y = element_blank()) +
  coord_flip() +
  guides(color = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE)) +
  theme(legend.position = "top", 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(1.5, "cm"))

# use patchwork to combine plots for spearman r and rsq into one figure

get_legend <- function(myplot) {
  # Extract legends
  tmp <- ggplotGrob(myplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


# Extract the legend
legend <- get_legend(bmr_plot_rsq)

bmr_plot_srho <- bmr_plot_srho + theme(legend.position = "none") # remove legend 

bmr_plot_rsq <- bmr_plot_rsq + theme(legend.position = "none") # remove legend 

# arrange plots
bmr_plot  <- (wrap_elements(full = legend) / (bmr_plot_srho + bmr_plot_rsq)) + 
  plot_layout(heights = c(1, 10)) # Adjust heights as needed

# save figure

png(file="figures/bmr_plot.png",width=1000, height=1000)

bmr_plot

dev.off()

### FINISH