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

#### OUTER RESAMPLING ####
repeated_cv <- rsmp("repeated_cv", repeats = 5, folds = 10)

#### PCA PIPELINE ####
po_pca100 <- po("pca", id = "pca100", rank. = 100, center = TRUE, scale. = TRUE)

# Random forest with PCA 
lrn_rf_pca <- GraphLearner$new(po_pca100 %>>% lrn("regr.ranger",
                                                  num.trees = 1000))
lrn_rf_pca$id <- "rf_pca100"

# Elastic net (cv_glmnet) with PCA 
lrn_en_pca <- GraphLearner$new(po_pca100 %>>% lrn("regr.cv_glmnet", alpha = 0.5))
lrn_en_pca$id <- "en_pca100"

#### BENCHMARK ####

# avoid console output from mlr3tuning
logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")

# show progress
progressr::handlers(global = TRUE)
progressr::handlers("progress")

## main analyses

design_main = benchmark_grid(
  task = c(
    wordembeddings_arousal, wordembeddings_content, wordembeddings_sad,
    wordembeddings_masked_arousal, wordembeddings_masked_content, wordembeddings_masked_sad,
    speechembeddings_arousal, speechembeddings_content, speechembeddings_sad,
    liwc_arousal, liwc_content, liwc_sad,
    liwc_masked_arousal, liwc_masked_content, liwc_masked_sad,
    egemaps_arousal, egemaps_content, egemaps_sad,
    ensemble_arousal, ensemble_content, ensemble_sad
  ),
  learner = list(lrn_fl, lrn_rf, lrn_en),
  resampling = repeated_cv
)


design_pca = benchmark_grid(
  task =  c(
    wordembeddings_arousal, wordembeddings_content, wordembeddings_sad,
    speechembeddings_arousal, speechembeddings_content, speechembeddings_sad
  ),
  learner = list(lrn_rf_pca, lrn_en_pca),
  resampling = repeated_cv
)

# combine designs so PCA learners run only where intended
bmgrid = rbind(design_main, 
               design_pca
               )

future::plan("multisession", workers = 10) # enable parallelization

bmr_main = benchmark(bmgrid, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_main, "results/bmr_main.rds") # save results


## separate benchmark using en (to extract regression weights from later)

bmgrid_en = benchmark_grid(
  task = c(
    ensemble_arousal, # ensemble
    ensemble_content, 
    ensemble_sad,
    wordembeddings_arousal, # text embeddings
    wordembeddings_content,
    wordembeddings_sad,
    liwc_arousal, # liwc
    liwc_content,
    liwc_sad
  ),
  learner = list(lrn_en),
  resampling = repeated_cv
)

future::plan("multisession", workers = 20) # enable parallelization

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

# --- choose the legend/order once ---
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

perf_summary <- bmr_results_fig %>%
  group_by(task_id, feature_set) %>%
  summarise(
    ymin   = quantile(regr.srho, 0.025, na.rm = TRUE),  # 2.5th percentile
    lower  = quantile(regr.srho, 0.25,  na.rm = TRUE),  # 25th percentile
    middle = median(regr.srho,          na.rm = TRUE),  # median
    upper  = quantile(regr.srho, 0.75,  na.rm = TRUE),  # 75th percentile
    ymax   = quantile(regr.srho, 0.975, na.rm = TRUE),  # 97.5th percentile
    .groups = "drop"
  )

# make sure factor levels are preserved
perf_summary <- perf_summary %>%
  mutate(
    task_id     = factor(task_id,     levels = c("Contentment", "Sadness", "Arousal")),
    feature_set = factor(feature_set, levels = feat_levels)
  )

# boxplot with IQR box and 95% whiskers
perf_plot <- ggplot(
  perf_summary,
  aes(x = task_id, y = middle, fill = feature_set)
) +
  geom_boxplot(
    stat  = "identity",
    aes(ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax),
    width = 0.6,
    position = position_dodge2(width = 0.75, preserve = "single"),
    outlier.shape = NA   # no additional outlier points; whiskers already define 95% range
  ) +
  scale_fill_manual(
    name   = "Feature set",
    values = c(
      "All"                 = "#6a3d9a",
      "Text embeddings"     = "#33a02c",
      "Speech embeddings"   = "#e31a1c",
      "LIWC"                = "#ff7f00",
      "Prosodic descriptors"= "#1f78b4"
    ),
    breaks = feat_levels,
    labels = c("All", "Text\nembeddings", "Speech\nembeddings",
               "LIWC", "Prosodic\ndescriptors")
  ) +
  labs(
    x = NULL,
    y = bquote("Spearman correlation (" * italic(rho) * ")"),
    title = NULL
  ) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_custom() +
  theme(
    legend.position = "top",
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )


# Print
print(perf_plot)

# save figure

ggsave(
  filename = "figures/bmr_plot.png",
  plot     = perf_plot,   
  width    = 14,
  height   = 8,
  units    = "in",
  dpi      = 300        
)

### FINISH