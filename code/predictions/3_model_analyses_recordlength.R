## ================================================================
## Duration (s) vs. ensemble prediction error (Content, Sadness, Arousal)
## ================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(mlr3)
})

# --- inputs ---------------------------------------------------------------
bmr_en             <- readRDS("results/bmr_en.rds")
audio_ema_features <- readRDS("data/audio_ema_matched_cleaned.rds")
stopifnot("duration_s" %in% names(audio_ema_features))

# loads theme_custom()
source("code/functions/plot_theme.R")

# --- helper: map task_id -> target label ----------------------------------
label_target_from_task <- function(task_id_str) {
  t <- tolower(task_id_str)
  dplyr::case_when(
    str_detect(t, "sad")     ~ "Sadness",
    str_detect(t, "content") ~ "Contentment",
    str_detect(t, "arousal") ~ "Arousal",
    TRUE                     ~ NA_character_
  )
}

# --- aggregate once and select ensemble tasks -----------------------------
aggr_dt   <- as.data.table(bmr_en$aggregate())
task_list <- unique(aggr_dt[grepl("^ensemble_", task_id), task_id])

# --- helper: extract OOF predictions for one ensemble task ----------------
get_pred_for_task <- function(task_name) {
  idx <- which(aggr_dt$task_id == task_name)
  if (length(idx) != 1L) {
    stop(sprintf(
      "Task '%s' not found uniquely in aggregate(). Found rows: %s",
      task_name, paste(idx, collapse = ",")
    ))
  }
  
  rr   <- aggr_dt$resample_result[[idx]]
  pred <- as.data.table(rr$prediction())   # out-of-fold predictions
  
  # normalize id column and attach duration
  if ("row_ids" %in% names(pred)) setnames(pred, "row_ids", "row_id")
  if (!"row_id" %in% names(pred)) pred[, row_id := .I]
  
  pred[, duration_s := as.numeric(audio_ema_features$duration_s[row_id])]
  pred[, target     := label_target_from_task(task_name)]
  pred[, abs_error  := abs(as.numeric(truth) - as.numeric(response))]
  
  pred[is.finite(duration_s) & is.finite(abs_error) & !is.na(target),
       .(duration_s, abs_error, target)]
}

# --- collect predictions for all ensemble tasks ---------------------------
pred_all <- rbindlist(lapply(task_list, get_pred_for_task), use.names = TRUE)
pred_all[, target := factor(target, levels = c("Contentment", "Sadness", "Arousal"))]

# restrict to clips <= 90 s
pred_plot <- pred_all[duration_s <= 90]


## create the plot

# --- settings ---------------------------------------------------
x_min <- 5
x_max <- 60
y_min <- 0.50
y_max <- 0.75
bins  <- 40
hist_height_frac <- 0.35  # proportion of y-range used by histogram

# --- histogram data for background ------------------------------
hist_df <- pred_plot %>%
  filter(duration_s >= x_min, duration_s <= x_max)

hist_vals <- hist(hist_df$duration_s, breaks = bins, plot = FALSE)

hist_plot_df <- data.frame(
  xmin  = head(hist_vals$breaks, -1),
  xmax  = tail(hist_vals$breaks, -1),
  count = hist_vals$counts
) %>%
  mutate(
    ymin = y_min,
    ymax = y_min + (count / max(count)) * (y_max - y_min) * hist_height_frac
  )

# --- combined plot: SAME loess fit as before -------------------
p_len_err_loess <- ggplot(pred_plot, aes(x = duration_s, y = abs_error, color = target)) +
  geom_rect(
    data = hist_plot_df,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "grey80",
    color = "white",
    linewidth = 0.2
  ) +
  geom_smooth(
    method  = "loess",
    formula = y ~ x,
    se      = FALSE,
    span    = 1,
    linewidth = 1
  ) +
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
  labs(
    x     = "Voiced speech duration (seconds)",
    y     = "Absolute prediction error",
    color = "State"
  ) +
  theme_custom(base_size = 15)

p_len_err_loess


ggsave("figures/len_vs_error_ensemble_targets_loess_with_hist.png",
       p_len_err_loess, width = 10, height = 9, dpi = 300)

# finish