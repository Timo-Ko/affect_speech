# Install and load required packages 

packages <- c( "dplyr", "tidyr", "ggplot2", "gridExtra") # also try patchwork?
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# read in benchmark results (for en)
bmr_en <- readRDS("results/bmr_en.rds")

# load features
audio_ema_features <- readRDS("data/audio_ema_features.rds")

####  DATA AMOUNT EFFECS ON PREDICTION PERFORMANCE ####

## create plot showing the prediction error on y axis and seconds of voiced speech score on x-axis with 3 curves (one for sad and one for content)

## get predictions 
aggr_content = bmr_egemaps_content$aggregate()
rr_content = aggr_content$resample_result[[3]]
predictions_content <- as.data.table(rr_content$prediction())

# rename columns
colnames(predictions_content)[colnames(predictions_content) == 'truth'] <- 'truth_content'
colnames(predictions_content)[colnames(predictions_content) == 'response'] <- 'response_content'

## get predictions from rf (best performing algo) for sadness
aggr_sad = bmr_egemaps_sad$aggregate()
rr_sad = aggr_sad$resample_result[[3]]
predictions_sad <- as.data.table(rr_sad$prediction())

# rename columns
colnames(predictions_sad)[colnames(predictions_sad) == 'truth'] <- 'truth_sad'
colnames(predictions_sad)[colnames(predictions_sad) == 'response'] <- 'response_sad'

## get predictions from rf (best performing algo) for arousal
aggr_arousal = bmr_egemaps_arousal$aggregate()
rr_arousal = aggr_arousal$resample_result[[3]]
predictions_arousal <- as.data.table(rr_arousal$prediction())

# rename columns
colnames(predictions_arousal)[colnames(predictions_arousal) == 'truth'] <- 'truth_arousal'
colnames(predictions_arousal)[colnames(predictions_arousal) == 'response'] <- 'response_arousal'

# create one df
predictions_voiceduration <- cbind(predictions_content, predictions_sad, predictions_arousal, affect_acoustics$Voice.only.duration.in.seconds)

# rename column for voice duration
colnames(predictions_voiceduration)[colnames(predictions_voiceduration) == 'V4'] <- 'voiceduration'

# compute prediction error 
predictions_voiceduration$error_content <- abs(predictions_voiceduration$truth_content - predictions_voiceduration$response_content)
predictions_voiceduration$error_sad <- abs(predictions_voiceduration$truth_sad - predictions_voiceduration$response_sad)
predictions_voiceduration$error_arousal <- abs(predictions_voiceduration$truth_arousal - predictions_voiceduration$response_arousal)

head(predictions_voiceduration)

# convert to long format
predictions_voiceduration_long <-  gather(predictions_voiceduration, condition, error, error_content, error_sad, error_arousal)

# create plot
voiceduration_error_plot <- ggplot(data = predictions_voiceduration_long[,c("voiceduration", "error", "condition")], 
                          aes_string(x= "voiceduration" , y= "error", color = "condition")) +
  #geom_point() +
  stat_smooth(method='loess', se = T, span = 1)+
  scale_x_continuous("Length of record") +
  scale_y_continuous("Absolute prediction error") + 
  theme_minimal(base_size = 20)

voiceduration_error_plot

# save plot

png(file="figures/performance_voiceamount.png",width=1000, height=800)


dev.off()

# FINISH

### new code

## =======================================================================
## Voice duration (s) vs. Elastic Net prediction error
## for Word Embeddings and LIWC (using bmr_en$aggregate() indexing)
## =======================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(stringr)
  library(ggplot2)
})

# --- inputs -----------------------------------------------------------------
bmr_en <- readRDS("results/bmr_en.rds")
audio_ema_features <- readRDS("data/audio_ema_features.rds")
stopifnot("voice_duration" %in% names(audio_ema_features))

# --- aggregate once ----------------------------------------------------------
aggr <- bmr_en$aggregate()  # has columns: task_id, learner_id, resample_result, ...
aggr_dt <- as.data.table(aggr)

# quick look (optional)
# print(aggr_dt[, .(row = .I, task_id, learner_id)])

# --- helper: map task name -> feature set / target --------------------------
label_feature_set_from_task <- function(task_id_str) {
  s <- tolower(task_id_str)
  case_when(
    str_detect(s, "^liwc") ~ "LIWC (text)",
    str_detect(s, "^wordembeddings|word[_-]?embeddings") ~ "Text Embeddings",
    TRUE ~ "Unknown"
  )
}

label_target_from_task <- function(task_id_str) {
  t <- tolower(task_id_str)
  case_when(
    str_detect(t, "sad") ~ "Sadness",
    str_detect(t, "content|valence") ~ "Content",
    str_detect(t, "energy|arousal") ~ "Energy",
    TRUE ~ "Unknown"
  )
}

# --- helper: extract a prediction dt for a given exact task_id --------------
get_pred_for_task <- function(task_name_exact, aggr_obj, features_df) {
  # find row index in aggr for this task (assumes single learner: regr.cv_glmnet)
  idx <- which(aggr_obj$task_id == task_name_exact)
  if (length(idx) != 1L) {
    stop(sprintf("Task '%s' not found uniquely in aggregate(). Found: %s",
                 task_name_exact, paste(idx, collapse = ",")))
  }
  rr <- aggr_obj$resample_result[[idx]]          # ResampleResult
  pred <- as.data.table(rr$prediction())         # out-of-fold predictions
  
  # normalize id column and attach voice_duration by row index
  if ("row_ids" %in% names(pred)) setnames(pred, "row_ids", "row_id")
  if (!"row_id" %in% names(pred)) pred[, row_id := .I]
  
  # attach voice_duration by indexing the features with row_id
  pred[, word_count := as.numeric(features_df$word_count[row_id])]
  
  # annotate
  pred[, `:=`(
    task_id = task_name_exact,
    feature_set = label_feature_set_from_task(task_name_exact),
    target = label_target_from_task(task_name_exact)
  )]
  
  # compute absolute error
  pred[, abs_error := abs(as.numeric(truth) - as.numeric(response))]
  
  pred[]
}

# --- list the tasks you want (current bmr_en has exactly these 6) ----------
task_list <- c(
  "wordembeddings_content",
  "wordembeddings_sad",
  "wordembeddings_arousal",
  "liwc_content",
  "liwc_sad",
  "liwc_arousal"
)

# --- extract all predictions -------------------------------------------------
pred_list <- lapply(task_list, get_pred_for_task, aggr_obj = aggr, features_df = audio_ema_features)
pred_all  <- rbindlist(pred_list, fill = TRUE)

# keep only rows with finite values
pred_all  <- pred_all[is.finite(abs_error) & is.finite(word_count)]

# factor order for nice facets/legend
pred_all[, feature_set := factor(feature_set, levels = c("Text Embeddings", "LIWC (text)", "Unknown"))]
pred_all[, target      := factor(target,      levels = c("Content","Sadness","Energy","Unknown"))]

# --- sanity checks -----------------------------------------------------------
cat("Rows with predictions:", nrow(pred_all), "\n")
print(table(pred_all$feature_set, useNA = "ifany"))
print(table(pred_all$target, useNA = "ifany"))

# --- plot: error vs. voice_duration -----------------------------------------
# install.packages("mgcv") # once
library(mgcv)
library(ggplot2)

p_len_err <- ggplot(pred_all, aes(x = word_count, y = abs_error, color = target)) +
  geom_point(alpha = 0.05, size = 0.6) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "cs"),  # cubic regression spline
    se = TRUE
  ) +
  facet_wrap(~ feature_set, scales = "free_x") +
  labs(
    x = "Voiced speech duration (seconds)",
    y = "Absolute prediction error",
    title = "Record Length vs. Elastic Net Prediction Error",
    subtitle = "GAM spline fits per feature set (out-of-fold predictions)"
  ) +
  scale_color_brewer(palette = "Dark2", name = "Target") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ggsave("figures/len_vs_error_en_textsets.png", p_len_err, width = 10, height = 7, dpi = 300)

# --- compact numeric summary (quintiles) ------------------------------------
summ_len <- pred_all %>%
  mutate(length_bin = cut(
    voice_duration,
    breaks = quantile(voice_duration, probs = seq(0, 1, 0.2), na.rm = TRUE),
    include.lowest = TRUE
  )) %>%
  group_by(feature_set, target, length_bin) %>%
  summarize(
    n = n(),
    mean_voice = mean(voice_duration, na.rm = TRUE),
    mean_abs_error = mean(abs_error, na.rm = TRUE),
    .groups = "drop"
  )

print(head(summ_len, 12))
# write.csv(summ_len, "results/len_vs_error_summary_en_textsets.csv", row.names = FALSE)

# --- how to extend later -----------------------------------------------------
# When you add eGeMAPS or Speech Embeddings:
# 1) ensure their tasks exist in the BenchmarkResult (e.g., "egemaps_content", "wav2vec_sad", etc.)
# 2) append those exact task names to `task_list` above
# 3) the labeler already recognizes them; plots will add new facets automatically


