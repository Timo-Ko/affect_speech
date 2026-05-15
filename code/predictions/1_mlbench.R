### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "parallel", "data.table", "ggplot2", "ggtext", "mlr3", "mlr3learners", "mlr3pipelines","ranger", "glmnet", "future", "remotes", "bbotk", "patchwork")
#install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

set.seed(123, kind = "L'Ecuyer") # set seed to make sure all results are reproducible

# load required functions
source("code/functions/bmr_results.R")
source("code/functions/plot_theme.R")

### READ IN DATA ####

audio_ema_features <- readRDS("data/audio_ema_matched_cleaned.rds")

# remove illegal characters from colnames 
colnames(audio_ema_features) <- make.names(colnames(audio_ema_features), unique = TRUE)

#### CREATE TASKS  ####

## TEXT EMBEDDINGS ---------------------------------------------

# contentment
wordembeddings_content <- TaskRegr$new(
  id = "wordembeddings_content",
  backend = audio_ema_features %>%
    filter(!is.na(ema_content)) %>%
    select(participant_id, ema_content, roberta_1:roberta_1024),
  target = "ema_content"
)

# sadness
wordembeddings_sad <- TaskRegr$new(
  id = "wordembeddings_sad",
  backend = audio_ema_features %>%
    filter(!is.na(ema_sad)) %>%
    select(participant_id, ema_sad, roberta_1:roberta_1024),
  target = "ema_sad"
)

# arousal
wordembeddings_arousal <- TaskRegr$new(
  id = "wordembeddings_arousal",
  backend = audio_ema_features %>%
    filter(!is.na(ema_energy)) %>%
    select(participant_id, ema_energy, roberta_1:roberta_1024),
  target = "ema_energy"
)


## TEXT EMBEDDINGS (MASKED) ---------------------------------------------

# contentment
wordembeddings_masked_content <- TaskRegr$new(
  id = "wordembeddings_masked_content",
  backend = audio_ema_features %>%
    filter(!is.na(ema_content)) %>%
    select(participant_id, ema_content, roberta_1_masked:roberta_1024_masked),
  target = "ema_content"
)

# sadness
wordembeddings_masked_sad <- TaskRegr$new(
  id = "wordembeddings_masked_sad",
  backend = audio_ema_features %>%
    filter(!is.na(ema_sad)) %>%
    select(participant_id, ema_sad, roberta_1_masked:roberta_1024_masked),
  target = "ema_sad"
)

# arousal
wordembeddings_masked_arousal <- TaskRegr$new(
  id = "wordembeddings_masked_arousal",
  backend = audio_ema_features %>%
    filter(!is.na(ema_energy)) %>%
    select(participant_id, ema_energy, roberta_1_masked:roberta_1024_masked),
  target = "ema_energy"
)


## SPEECH EMBEDDINGS -------------------------------------------

# contentment
speechembeddings_content <- TaskRegr$new(
  id = "speechembeddings_content",
  backend = audio_ema_features %>%
    filter(!is.na(ema_content)) %>%
    select(participant_id, ema_content, wav2vec2_1:wav2vec2_1024),
  target = "ema_content"
)

# sadness
speechembeddings_sad <- TaskRegr$new(
  id = "speechembeddings_sad",
  backend = audio_ema_features %>%
    filter(!is.na(ema_sad)) %>%
    select(participant_id, ema_sad, wav2vec2_1:wav2vec2_1024),
  target = "ema_sad"
)

# arousal
speechembeddings_arousal <- TaskRegr$new(
  id = "speechembeddings_arousal",
  backend = audio_ema_features %>%
    filter(!is.na(ema_energy)) %>%
    select(participant_id, ema_energy, wav2vec2_1:wav2vec2_1024),
  target = "ema_energy"
)

## LIWC ---------------------------------------------------------

# contentment
liwc_content <- TaskRegr$new(
  id = "liwc_content",
  backend = audio_ema_features %>%
    filter(!is.na(ema_content)) %>%
    select(participant_id, ema_content, function.:filler),
  target = "ema_content"
)

# sadness
liwc_sad <- TaskRegr$new(
  id = "liwc_sad",
  backend = audio_ema_features %>%
    filter(!is.na(ema_sad)) %>%
    select(participant_id, ema_sad, function.:filler),
  target = "ema_sad"
)

# arousal
liwc_arousal <- TaskRegr$new(
  id = "liwc_arousal",
  backend = audio_ema_features %>%
    filter(!is.na(ema_energy)) %>%
    select(participant_id, ema_energy, function.:filler),
  target = "ema_energy"
)


## LIWC (MASKED) ---------------------------------------------------------

# contentment
liwc_masked_content <- TaskRegr$new(
  id = "liwc_masked_content",
  backend = audio_ema_features %>%
    filter(!is.na(ema_content)) %>%
    select(participant_id, ema_content, function._masked:filler_masked),
  target = "ema_content"
)

# sadness
liwc_masked_sad <- TaskRegr$new(
  id = "liwc_masked_sad",
  backend = audio_ema_features %>%
    filter(!is.na(ema_sad)) %>%
    select(participant_id, ema_sad, function._masked:filler_masked),
  target = "ema_sad"
)

# arousal
liwc_masked_arousal <- TaskRegr$new(
  id = "liwc_masked_arousal",
  backend = audio_ema_features %>%
    filter(!is.na(ema_energy)) %>%
    select(participant_id, ema_energy, function._masked:filler_masked),
  target = "ema_energy"
)

## eGeMAPS PROSODIC DESCRIPTORS --------------------------------

# ensure numeric

audio_ema_features <- audio_ema_features %>%
  mutate(
    across(
      starts_with("egemaps__"),
      ~ suppressWarnings(as.numeric(.x))
    )
  )


# contentment
egemaps_content <- TaskRegr$new(
  id = "egemaps_content",
  backend = audio_ema_features %>%
    filter(!is.na(ema_content)) %>%
    select(participant_id, ema_content, egemaps__F0semitoneFrom27.5Hz_sma3nz_amean:egemaps__equivalentSoundLevel_dBp),
  target = "ema_content"
)

# sadness
egemaps_sad <- TaskRegr$new(
  id = "egemaps_sad",
  backend = audio_ema_features %>%
    filter(!is.na(ema_sad)) %>%
    select(participant_id, ema_sad, egemaps__F0semitoneFrom27.5Hz_sma3nz_amean:egemaps__equivalentSoundLevel_dBp),
  target = "ema_sad"
)

# arousal
egemaps_arousal <- TaskRegr$new(
  id = "egemaps_arousal",
  backend = audio_ema_features %>%
    filter(!is.na(ema_energy)) %>%
    select(participant_id, ema_energy, egemaps__F0semitoneFrom27.5Hz_sma3nz_amean:egemaps__equivalentSoundLevel_dBp),
  target = "ema_energy"
)


## ALL COMBINED ------------------------------------

# contentment
ensemble_content <- TaskRegr$new(
  id = "ensemble_content",
  backend = audio_ema_features %>%
    filter(!is.na(ema_content)) %>%
    select(participant_id, ema_content, wav2vec2_1:wav2vec2_1024, roberta_1:roberta_1024, function.:filler, egemaps__F0semitoneFrom27.5Hz_sma3nz_amean:egemaps__equivalentSoundLevel_dBp),
  target = "ema_content"
)

# sadness
ensemble_sad <- TaskRegr$new(
  id = "ensemble_sad",
  backend = audio_ema_features %>%
    filter(!is.na(ema_sad)) %>%
    select(participant_id, ema_sad, wav2vec2_1:wav2vec2_1024, roberta_1:roberta_1024, function.:filler, egemaps__F0semitoneFrom27.5Hz_sma3nz_amean:egemaps__equivalentSoundLevel_dBp),
  target = "ema_sad"
)

# arousal
ensemble_arousal <- TaskRegr$new(
  id = "ensemble_arousal",
  backend = audio_ema_features %>%
    filter(!is.na(ema_energy)) %>%
    select(participant_id, ema_energy, wav2vec2_1:wav2vec2_1024, roberta_1:roberta_1024, function.:filler, egemaps__F0semitoneFrom27.5Hz_sma3nz_amean:egemaps__equivalentSoundLevel_dBp),
  target = "ema_energy"
)



## add blocking

# text embeddings 

#  content 
wordembeddings_content$col_roles$group = "participant_id"
wordembeddings_content$col_roles$feature = setdiff(wordembeddings_content$col_roles$feature, "participant_id")

#  sad score
wordembeddings_sad$col_roles$group = "participant_id"
wordembeddings_sad$col_roles$feature = setdiff(wordembeddings_sad$col_roles$feature, "participant_id")

#  arousal score
wordembeddings_arousal$col_roles$group = "participant_id"
wordembeddings_arousal$col_roles$feature = setdiff(wordembeddings_arousal$col_roles$feature, "participant_id")

# text embeddings (masked)

#  content 
wordembeddings_masked_content$col_roles$group = "participant_id"
wordembeddings_masked_content$col_roles$feature = setdiff(wordembeddings_masked_content$col_roles$feature, "participant_id")

#  sad score
wordembeddings_masked_sad$col_roles$group = "participant_id"
wordembeddings_masked_sad$col_roles$feature = setdiff(wordembeddings_masked_sad$col_roles$feature, "participant_id")

#  arousal score
wordembeddings_masked_arousal$col_roles$group = "participant_id"
wordembeddings_masked_arousal$col_roles$feature = setdiff(wordembeddings_masked_arousal$col_roles$feature, "participant_id")

# speech embeddings 

#  content 
speechembeddings_content$col_roles$group = "participant_id"
speechembeddings_content$col_roles$feature = setdiff(speechembeddings_content$col_roles$feature, "participant_id")

#  sad score
speechembeddings_sad$col_roles$group = "participant_id"
speechembeddings_sad$col_roles$feature = setdiff(speechembeddings_sad$col_roles$feature, "participant_id")

#  arousal score
speechembeddings_arousal$col_roles$group = "participant_id"
speechembeddings_arousal$col_roles$feature = setdiff(speechembeddings_arousal$col_roles$feature, "participant_id")

# liwc

#  content score
liwc_content$col_roles$group = "participant_id"
liwc_content$col_roles$feature = setdiff(liwc_content$col_roles$feature, "participant_id")

#  sad score
liwc_sad$col_roles$group = "participant_id"
liwc_sad$col_roles$feature = setdiff(liwc_sad$col_roles$feature, "participant_id")

#  arousal score
liwc_arousal$col_roles$group = "participant_id"
liwc_arousal$col_roles$feature = setdiff(liwc_arousal$col_roles$feature, "participant_id")


# liwc (masked)

#  content score
liwc_masked_content$col_roles$group = "participant_id"
liwc_masked_content$col_roles$feature = setdiff(liwc_masked_content$col_roles$feature, "participant_id")

#  sad score
liwc_masked_sad$col_roles$group = "participant_id"
liwc_masked_sad$col_roles$feature = setdiff(liwc_masked_sad$col_roles$feature, "participant_id")

#  arousal score
liwc_masked_arousal$col_roles$group = "participant_id"
liwc_masked_arousal$col_roles$feature = setdiff(liwc_masked_arousal$col_roles$feature, "participant_id")


# egemaps

#  content score
egemaps_content$col_roles$group = "participant_id"
egemaps_content$col_roles$feature = setdiff(egemaps_content$col_roles$feature, "participant_id")

#  sad score
egemaps_sad$col_roles$group = "participant_id"
egemaps_sad$col_roles$feature = setdiff(egemaps_sad$col_roles$feature, "participant_id")

#  arousal score
egemaps_arousal$col_roles$group = "participant_id"
egemaps_arousal$col_roles$feature = setdiff(egemaps_arousal$col_roles$feature, "participant_id")

# speech + word embeddings 

#  content 
ensemble_content$col_roles$group = "participant_id"
ensemble_content$col_roles$feature = setdiff(ensemble_content$col_roles$feature, "participant_id")

#  sad score
ensemble_sad$col_roles$group = "participant_id"
ensemble_sad$col_roles$feature = setdiff(ensemble_sad$col_roles$feature, "participant_id")

#  arousal score
ensemble_arousal$col_roles$group = "participant_id"
ensemble_arousal$col_roles$feature = setdiff(ensemble_arousal$col_roles$feature, "participant_id")

#### LEARNERS ####

# featureless learner 
lrn_fl = lrn("regr.featureless")

lrn_rf <- lrn("regr.ranger", num.trees = 1000)

lrn_en <- lrn("regr.cv_glmnet", alpha = 0.5)

#### PCA PIPELINE ####
po_pca100 <- po("pca", id = "pca100", rank. = 100, center = TRUE, scale. = TRUE)

# Random forest with PCA 
lrn_rf_pca <- GraphLearner$new(po_pca100 %>>% lrn_rf)
lrn_rf_pca$id <- "rf_pca100"

# Elastic net (cv_glmnet) with PCA 
lrn_en_pca <- GraphLearner$new(po_pca100 %>>% lrn_en)
lrn_en_pca$id <- "en_pca100"


#### OUTER RESAMPLING: fixed participant-blocked custom CV ####

create_repeated_participant_cv_resampling <- function(participant_ids, folds = 10, repeats = 5, seed = 123) {
  set.seed(seed, kind = "L'Ecuyer")
  
  participant_ids <- sort(unique(participant_ids))
  
  train_participant_sets <- list()
  test_participant_sets  <- list()
  
  iter <- 1L
  for (r in seq_len(repeats)) {
    shuffled <- sample(participant_ids)
    fold_id <- rep(seq_len(folds), length.out = length(shuffled))
    
    for (f in seq_len(folds)) {
      test_participants <- shuffled[fold_id == f]
      
      test_participant_sets[[iter]]  <- test_participants
      train_participant_sets[[iter]] <- setdiff(participant_ids, test_participants)
      
      iter <- iter + 1L
    }
  }
  
  list(
    train_participant_sets = train_participant_sets,
    test_participant_sets  = test_participant_sets
  )
}

participant_sets_to_row_sets <- function(task, participant_cv_sets) {
  task_participants <- task$data(cols = "participant_id")$participant_id
  row_ids <- task$row_ids
  
  train_sets <- lapply(participant_cv_sets$train_participant_sets, function(pids) {
    row_ids[task_participants %in% pids]
  })
  
  test_sets <- lapply(participant_cv_sets$test_participant_sets, function(pids) {
    row_ids[task_participants %in% pids]
  })
  
  list(train_sets = train_sets, test_sets = test_sets)
}

make_resamplings <- function(tasks, participant_cv_sets) {
  custom_cv <- rsmp("custom")
  resamplings <- list()
  
  for (task_name in names(tasks)) {
    row_sets <- participant_sets_to_row_sets(tasks[[task_name]], participant_cv_sets)
    
    res <- custom_cv$clone()
    res$instantiate(
      tasks[[task_name]],
      row_sets$train_sets,
      row_sets$test_sets
    )
    
    resamplings[[task_name]] <- res
  }
  
  resamplings
}

make_design_from_resamplings <- function(tasks, learners, resamplings) {
  rows <- list()
  iter <- 1L
  
  for (task_name in names(tasks)) {
    for (learner in learners) {
      rows[[iter]] <- data.table::data.table(
        task = list(tasks[[task_name]]),
        learner = list(learner$clone(deep = TRUE)),
        resampling = list(resamplings[[task_name]]$clone(deep = TRUE))
      )
      iter <- iter + 1L
    }
  }
  
  data.table::rbindlist(rows)
}

# one participant-level fold assignment reused everywhere
participant_cv_sets <- create_repeated_participant_cv_resampling(
  participant_ids = audio_ema_features$participant_id,
  folds = 10,
  repeats = 5,
  seed = 123
)

tasks_main <- list(
  wordembeddings_arousal = wordembeddings_arousal,
  wordembeddings_content = wordembeddings_content,
  wordembeddings_sad = wordembeddings_sad,
  wordembeddings_masked_arousal = wordembeddings_masked_arousal,
  wordembeddings_masked_content = wordembeddings_masked_content,
  wordembeddings_masked_sad = wordembeddings_masked_sad,
  speechembeddings_arousal = speechembeddings_arousal,
  speechembeddings_content = speechembeddings_content,
  speechembeddings_sad = speechembeddings_sad,
  liwc_arousal = liwc_arousal,
  liwc_content = liwc_content,
  liwc_sad = liwc_sad,
  liwc_masked_arousal = liwc_masked_arousal,
  liwc_masked_content = liwc_masked_content,
  liwc_masked_sad = liwc_masked_sad,
  egemaps_arousal = egemaps_arousal,
  egemaps_content = egemaps_content,
  egemaps_sad = egemaps_sad,
  ensemble_arousal = ensemble_arousal,
  ensemble_content = ensemble_content,
  ensemble_sad = ensemble_sad
)

tasks_pca <- list(
  wordembeddings_arousal = wordembeddings_arousal,
  wordembeddings_content = wordembeddings_content,
  wordembeddings_sad = wordembeddings_sad,
  speechembeddings_arousal = speechembeddings_arousal,
  speechembeddings_content = speechembeddings_content,
  speechembeddings_sad = speechembeddings_sad
)

resamplings_main <- make_resamplings(tasks_main, participant_cv_sets)
resamplings_pca  <- make_resamplings(tasks_pca, participant_cv_sets)

saveRDS(participant_cv_sets, "results/participant_cv_sets.rds")
saveRDS(resamplings_main, "results/resamplings_main.rds")
saveRDS(resamplings_pca, "results/resamplings_pca.rds")


### BENCHMARK DESIGN ####

design_main <- make_design_from_resamplings(
  tasks = tasks_main,
  learners = list(lrn_fl, lrn_rf, lrn_en),
  resamplings = resamplings_main
)

design_pca <- make_design_from_resamplings(
  tasks = tasks_pca,
  learners = list(lrn_rf_pca, lrn_en_pca),
  resamplings = resamplings_pca
)

bmgrid <- rbind(design_main, design_pca)


#### RUN BENCHMARK ####

# avoid console output from mlr3tuning
logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")

# show progress
progressr::handlers(global = TRUE)
progressr::handlers("progress")

future::plan("multisession", workers = 10) # enable parallelization

bmr_main = benchmark(bmgrid, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_main, "results/bmr_main.rds") # save results

## separate benchmark using en (to extract regression weights from later)

tasks_en <- list(
  ensemble_arousal = ensemble_arousal,
  ensemble_content = ensemble_content,
  ensemble_sad = ensemble_sad,
  wordembeddings_arousal = wordembeddings_arousal,
  wordembeddings_content = wordembeddings_content,
  wordembeddings_sad = wordembeddings_sad,
  liwc_arousal = liwc_arousal,
  liwc_content = liwc_content,
  liwc_sad = liwc_sad
)

resamplings_en <- make_resamplings(tasks_en, participant_cv_sets)
saveRDS(resamplings_en, "results/resamplings_en.rds")

bmgrid_en <- make_design_from_resamplings(
  tasks = tasks_en,
  learners = list(lrn_en),
  resamplings = resamplings_en
)

#  run benchmark

future::plan("multisession", workers = 10) # enable parallelization

bmr_en = benchmark(bmgrid_en, store_models = T, store_backends = F) # execute the benchmark

saveRDS(bmr_en, "results/bmr_en.rds") # save results


#### DEEP DIVE BENCHMARK RESULTS ####

## read in benchmark results

bmr_main <- readRDS("results/bmr_main.rds")

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

bmr_main$aggregate(mes)

## deep dive: retrieve benchmark results across tasks and learners for single cv folds

bmr_results_folds <- extract_bmr_results(bmr_main, mes)

## create combined overview table of performance

pred_table <- results_table(audio_ema_features, bmr_results_folds)

# save prediction tables
write.csv(pred_table, "results/pred_table.csv")

## create performance figure 

feat_levels <- c("All", "Text embeddings", "Speech embeddings", "LIWC", "Prosodic descriptors")

bmr_results_fig <- bmr_results_folds %>%
  filter(task_id %in% c(
    "egemaps_content", "egemaps_sad", "egemaps_arousal",
    "liwc_content", "liwc_sad", "liwc_arousal",
    "wordembeddings_content", "wordembeddings_sad", "wordembeddings_arousal",
    "speechembeddings_content", "speechembeddings_sad", "speechembeddings_arousal",
    "ensemble_content", "ensemble_sad", "ensemble_arousal"
  )) %>%
  filter(learner_id == "regr.cv_glmnet") %>%
  mutate(
    feature_set = case_when(
      task_id %in% c("egemaps_content", "egemaps_sad", "egemaps_arousal")              ~ "Prosodic descriptors",
      task_id %in% c("liwc_content", "liwc_sad", "liwc_arousal")              ~ "LIWC",
      task_id %in% c("wordembeddings_content", "wordembeddings_sad", "wordembeddings_arousal") ~ "Text embeddings",
      task_id %in% c("speechembeddings_content", "speechembeddings_sad", "speechembeddings_arousal") ~ "Speech embeddings",
      task_id %in% c("ensemble_content", "ensemble_sad", "ensemble_arousal") ~ "All"
    ),
    task_id = case_when(
      task_id == "egemaps_content"          ~ "Contentment",
      task_id == "egemaps_sad"              ~ "Sadness",
      task_id == "egemaps_arousal"          ~ "Arousal",
      task_id == "liwc_content"          ~ "Contentment",
      task_id == "liwc_sad"              ~ "Sadness",
      task_id == "liwc_arousal"          ~ "Arousal",
      task_id == "wordembeddings_content"   ~ "Contentment",
      task_id == "wordembeddings_sad"       ~ "Sadness",
      task_id == "wordembeddings_arousal"   ~ "Arousal",
      task_id == "speechembeddings_content" ~ "Contentment",
      task_id == "speechembeddings_sad"     ~ "Sadness",
      task_id == "speechembeddings_arousal" ~ "Arousal",
      task_id == "ensemble_content" ~ "Contentment",
      task_id == "ensemble_sad"     ~ "Sadness",
      task_id == "ensemble_arousal" ~ "Arousal"
    )
  ) %>%
  # # Replace NA in regr.srho (Elastic Net) with zero
  # mutate(
  #   regr.srho = if_else(learner_id == "regr.cv_glmnet" & is.na(regr.srho), 0, regr.srho)
  # ) %>% # order tasks and feature sets
  mutate(
    task_id = factor(task_id, levels = c("Contentment", "Sadness", "Arousal")),
    feature_set = factor(feature_set, levels = feat_levels) 
  )

## new violin plot code
## --- from bmr_results_fig onward: muted PNAS-style plot ---

# (Optional but recommended) remove NA/Inf to avoid violin warnings
bmr_results_fig <- bmr_results_fig %>%
  filter(is.finite(regr.srho))

# --- choose the legend/order once ---
feat_levels <- c("All", "Text embeddings", "Speech embeddings", "LIWC", "Prosodic descriptors")

bmr_results_fig <- bmr_results_fig %>%
  mutate(
    task_id = factor(task_id, levels = c("Contentment", "Sadness", "Arousal")),
    feature_set = factor(feature_set, levels = feat_levels)
  )

# --- summary for median + 95% interval ---
perf_summary <- bmr_results_fig %>%
  group_by(task_id, feature_set) %>%
  summarise(
    q025 = quantile(regr.srho, 0.025, na.rm = TRUE),
    med  = median(regr.srho,          na.rm = TRUE),
    q975 = quantile(regr.srho, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    task_id     = factor(task_id,     levels = c("Contentment", "Sadness", "Arousal")),
    feature_set = factor(feature_set, levels = feat_levels)
  )

pd <- position_dodge(width = 0.75)

# --- muted, print-friendly palette (PNAS-like) ---
pnas_palette <- c(
  "All"                  = "#3B5B92",  # muted navy
  "Text embeddings"      = "#4C8C6A",  # muted green
  "Speech embeddings"    = "#A94A3F",  # muted brick red
  "LIWC"                 = "#C18F3A",  # muted ochre
  "Prosodic descriptors" = "#6B6E77"   # muted gray-blue
)

perf_plot <- ggplot(
  bmr_results_fig,
  aes(x = task_id, y = regr.srho, fill = feature_set)
) +
  # Violin distribution (muted)
  geom_violin(
    position = pd,
    width    = 0.85,
    trim     = FALSE,
    alpha    = 0.60,
    color    = NA
  ) +
  # 95% interval line (neutral)
  geom_linerange(
    data = perf_summary,
    aes(x = task_id, ymin = q025, ymax = q975, group = feature_set),
    position = pd,
    linewidth = 0.6,
    colour = "grey20",
    inherit.aes = FALSE
  ) +
  # Median dot (white fill, neutral outline)
  geom_point(
    data = perf_summary,
    aes(x = task_id, y = med, group = feature_set),
    position = pd,
    size = 2.2,
    shape = 21,
    fill = "white",
    colour = "grey20",
    stroke = 0.4,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(
    name   = "Variable set",
    values = pnas_palette,
    breaks = feat_levels,
    labels = c(
      "All",
      "Text\nembeddings",
      "Speech\nembeddings",
      "LIWC",
      "Prosodic\ndescriptors"
    )
  ) +
  labs(
    x = NULL,
    y = bquote("Spearman correlation (" * italic(rho) * ")"),
    title = NULL
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey30") +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_custom() +
  theme(
    legend.position = "top",
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )

print(perf_plot)


ggsave(
  filename = "figures/fig1_bmr_plot_violin.png",
  plot     = perf_plot,
  width    = 12,
  height   = 8,
  units    = "in",
  dpi      = 300
)

### FINISH