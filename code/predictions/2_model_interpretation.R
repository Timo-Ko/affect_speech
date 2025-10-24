library(data.table)
library(mlr3misc)

## --- read benchmark and prep --------------------------------------------------
bmr_en <- readRDS("results/bmr_en.rds")

`%||%` <- function(x, y) if (is.null(x)) y else x

dt <- as.data.table(bmr_en)
dt[, task_id    := vapply(task,    function(t) t$id, character(1))]
dt[, learner_id := vapply(learner, function(l) l$id, character(1))]
dt[, fold       := if (!is.null(iteration)) iteration else seq_len(.N)]

## --- helper: get cv.glmnet object from a learner, regardless of wrapping ----
get_cv_glmnet <- function(lrn) {
  if (!is.null(lrn$model) && inherits(lrn$model, "cv.glmnet")) return(lrn$model)
  if (!is.null(lrn$state$model) && inherits(lrn$state$model, "cv.glmnet")) return(lrn$state$model)
  if (!is.null(lrn$model$regr.cv_glmnet$model) &&
      inherits(lrn$model$regr.cv_glmnet$model, "cv.glmnet")) return(lrn$model$regr.cv_glmnet$model)
  if (inherits(lrn, "GraphLearner")) {
    for (po in lrn$graph$pipeops) {
      if (inherits(po, "PipeOpLearner")) {
        mdl <- po$learner$model
        if (!is.null(mdl) && inherits(mdl, "cv.glmnet")) return(mdl)
      }
    }
  }
  NULL
}

## --- extract betas for one learner (one fold) --------------------------------
extract_betas <- function(lrn) {
  cv <- get_cv_glmnet(lrn)
  if (is.null(cv)) return(NULL)
  fit <- cv$glmnet.fit
  
  pull <- function(s, tag) {
    cm <- tryCatch(coef(fit, s = s), error = function(e) NULL)
    if (is.null(cm)) return(NULL)
    mat <- as.matrix(cm)
    if (nrow(mat) < 2) return(NULL)  # only intercept
    data.frame(
      feature     = rownames(mat)[-1],
      beta        = as.numeric(mat[-1, 1]),
      lambda_used = tag,
      stringsAsFactors = FALSE
    )
  }
  
  out <- pull(cv$lambda.1se, "lambda.1se")
  if (is.null(out)) out <- pull(cv$lambda.min, "lambda.min")
  out
}

## --- collect betas for a given task_id ---------------------------------------
collect_task_betas <- function(task_name) {
  rows <- dt[task_id == task_name]
  if (nrow(rows) == 0) return(NULL)
  dfs <- lapply(seq_len(nrow(rows)), function(i) {
    bet <- extract_betas(rows$learner[[i]])
    if (is.null(bet)) return(NULL)
    bet$task_id <- task_name
    bet$fold    <- rows$fold[i]
    bet
  })
  data.table::rbindlist(dfs, use.names = TRUE, fill = TRUE)
}

## --- run for BOTH modalities: wordembeddings_* and liwc_* --------------------
# Find the six task IDs programmatically (keeps it robust)
task_ids <- unique(dt$task_id)

we_tasks <- c("wordembeddings_arousal", "wordembeddings_content", "wordembeddings_sad")
liwc_tasks <- c("liwc_arousal", "liwc_content", "liwc_sad")

we_tasks <- we_tasks[we_tasks %in% task_ids]
liwc_tasks <- liwc_tasks[liwc_tasks %in% task_ids]

betas_we   <- data.table::rbindlist(lapply(we_tasks, collect_task_betas),   use.names = TRUE, fill = TRUE)
betas_liwc <- data.table::rbindlist(lapply(liwc_tasks, collect_task_betas), use.names = TRUE, fill = TRUE)

all_betas <- data.table::rbindlist(list(betas_we, betas_liwc), use.names = TRUE, fill = TRUE)

## --- annotate target + modality ---------------------------------------------
if (nrow(all_betas)) {
  all_betas[, target := fifelse(grepl("_arousal$", task_id), "arousal",
                                fifelse(grepl("_content$", task_id), "content", "sad"))]
  all_betas[, modality := fifelse(grepl("^liwc_", task_id), "liwc", "wordembeddings")]
}

## quick sanity
dim(all_betas)
head(all_betas)

## --- per-target summaries, now also stratified by modality -------------------
all_betas_dt <- as.data.table(all_betas)

# Summary by (modality, target, feature)
beta_summary_all <- all_betas_dt[
  , .(
    mean_beta     = mean(beta),
    mean_abs_beta = mean(abs(beta)),
    nonzero_cnt   = sum(beta != 0),
    nonzero_prop  = sum(beta != 0) / .N
  ),
  by = .(modality, target, feature)
][order(modality, target, -nonzero_cnt, -abs(mean_beta))]

beta_summary_all

# (optional) write out combined table
# fwrite(beta_summary_all, "results/beta_summary_all_modalities.csv")






### code for table 1 ####


library(dplyr)
library(purrr)
library(tidyr)
library(knitr)

# --- data holders assumed ---
# df   = your merged frame with targets + LIWC cols
# liwc = your LIWC-only frame (for column names)

# Targets
targets <- c("content_num" = "Contentment",
             "sad_num"     = "Sadness",
             "energy_num"  = "Arousal")

# LIWC columns (numeric, excluding audio_id if present)
liwc_cols <- names(liwc)
liwc_cols <- setdiff(liwc_cols, "audio_id")
liwc_cols <- liwc_cols[vapply(df[liwc_cols], is.numeric, logical(1))]

# --- summarise betas for LIWC features per target (denominator = n_models) ---
liwc_betas <- all_betas_dt %>%
  dplyr::filter(grepl("^liwc_", task_id)) %>%
  dplyr::group_by(target, feature) %>%
  dplyr::summarise(
    mean_beta    = mean(beta),
    sd_beta      = sd(beta),        # dispersion across all resample fits
    nonzero_cnt  = sum(beta != 0),
    n_models     = dplyr::n(),      # <-- count how many fits contributed
    nonzero_prop = nonzero_cnt / n_models,
    .groups = "drop"
  ) %>%
  dplyr::arrange(target, dplyr::desc(abs(mean_beta)))

# --- Top-10 per target, beta string now uses the true denominator -------------
liwc_top10 <- liwc_betas %>%
  dplyr::group_by(target) %>%
  dplyr::slice_max(order_by = abs(mean_beta), n = 10, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    beta_str = sprintf("%+.2f (SD=%.2f, sel=%d/%d)",
                       mean_beta, sd_beta, nonzero_cnt, n_models)
  )
# --- reshape into wide table ---
liwc_table_df <- liwc_top10 %>%
  group_by(target) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = row,
    names_from  = target,
    values_from = c(feature, beta_str),
    names_glue = "{target}_{.value}"
  ) %>%
  select(-row) %>%
  select(
    arousal_feature,  arousal_beta_str,
    content_feature,  content_beta_str,
    sad_feature,      sad_beta_str
  )

print(liwc_table_df, n = 5)

# save table
write.csv(liwc_table_df, "results/en_liwc_betas.csv")


### code for figure 2 ## 

library(dplyr)
library(forcats)
library(tidyr)
library(ggplot2)
library(patchwork)

# 0) Features frame with targets, LIWC cols, and roberta_* dims
features_df <- readRDS("data/audio_ema_features.rds")

liwc_cols <- unique(betas_liwc$feature) # get liwc col names

# 2) betas_emb from your earlier summaries (all_betas_dt must exist)
#    (You created all_betas_dt above from bmr_en)
stopifnot(exists("all_betas_dt"))
betas_emb <- all_betas_dt[
  grepl("^wordembeddings_", task_id) & grepl("^roberta_\\d+$", feature)
][
  , .(beta = mean(beta, na.rm = TRUE)), by = .(target, feature)
][
  , target := factor(target,
                     levels = c("arousal","content","sad"),
                     labels = c("Arousal","Contentment","Sadness"))
][]
stopifnot(nrow(betas_emb) > 0)

targets_order <- c("Arousal","Contentment","Sadness")

# ==== Figure 2 build ==========================================================
TOP_N <- 10  # LIWC categories per target (ranked by |Δρ|)

# Spearman helper
rho_spearman <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 30) return(NA_real_)
  suppressWarnings(cor(x[ok], y[ok], method = "spearman"))
}

# Correlations (LIWC × one embedding)
corr_liwc_for_embedding <- function(emb_name) {
  e <- features_df[[emb_name]]
  out <- data.frame(liwc = liwc_cols, stringsAsFactors = FALSE)
  out$rho <- vapply(liwc_cols, function(v) rho_spearman(e, features_df[[v]]), numeric(1))
  out <- out[is.finite(out$rho), ]
  out
}

# Build top-Δρ LIWC table for ONE target
build_delta_tbl <- function(target_label) {
  sub <- betas_emb %>% dplyr::filter(target == target_label)
  if (nrow(sub) == 0) return(NULL)
  
  # top positive / negative embedding by beta
  pos_row <- sub %>% dplyr::arrange(dplyr::desc(beta)) %>% dplyr::slice(1)
  neg_row <- sub %>% dplyr::arrange(beta)               %>% dplyr::slice(1)
  
  pos_emb <- pos_row$feature[1]
  neg_emb <- neg_row$feature[1]
  
  # LIWC correlations for each embedding
  pos <- corr_liwc_for_embedding(pos_emb) %>% dplyr::rename(rho_pos = rho)
  neg <- corr_liwc_for_embedding(neg_emb) %>% dplyr::rename(rho_neg = rho)
  
  # rank LIWC by DELTA = |rho_pos - rho_neg| (this sets the y-axis order)
  both <- dplyr::inner_join(pos, neg, by = "liwc") %>%
    dplyr::mutate(delta = abs(rho_pos - rho_neg)) %>%
    dplyr::arrange(dplyr::desc(delta)) %>%
    dplyr::slice(1:TOP_N)
  
  # pretty labels with β
  pos_lab <- paste0(gsub("^roberta_", "", pos_emb),
                    " (β = ", sprintf("%+.02f", pos_row$beta[1]), ")")
  neg_lab <- paste0(gsub("^roberta_", "", neg_emb),
                    " (β = ", sprintf("%+.02f", neg_row$beta[1]), ")")
  
  # long format; keep `delta` so we can order by it (shared across + / −)
  both_long <- tidyr::pivot_longer(
    both,
    cols = c(rho_pos, rho_neg),
    names_to = "sign_code",
    values_to = "rho"
  ) %>%
    dplyr::mutate(
      sign            = ifelse(sign_code == "rho_pos", "+", "-"),
      embedding_label = ifelse(sign == "+", pos_lab, neg_lab),
      target          = target_label
    ) %>%
    dplyr::select(target, sign, liwc, rho, embedding_label, delta) %>%
    # order LIWC by DELTA (same order for + and − panels)
    dplyr::mutate(liwc = forcats::fct_reorder(liwc, delta, .desc = TRUE))
  
  both_long
}


# Build for all targets
delta_list <- lapply(targets_order, build_delta_tbl)
delta_list <- delta_list[!vapply(delta_list, is.null, logical(1))]
delta_df   <- bind_rows(delta_list)
stopifnot(nrow(delta_df) > 0)

# unified color limits for ONE legend
L <- max(abs(delta_df$rho), na.rm = TRUE)

# Small plotting helper
col_plot <- function(dat, show_y = TRUE, col_title = "+") {
  ggplot(dat, aes(x = embedding_label, y = liwc, color = rho, size = abs(rho))) +
    geom_point() +
    scale_y_discrete(drop = TRUE, limits = function(x) rev(x)) +  # <— flip order
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_gradient2(limits = c(-L, L),
                          low = "#d73027", mid = "white", high = "#1a9850",
                          midpoint = 0, name = "Spearman \u03C1") +
    scale_size(range = c(2.6, 6), guide = "none") +
    labs(title = col_title, x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x        = element_text(angle = 15, hjust = 0.5, vjust = 1),
      axis.text.y        = if (show_y) element_text() else element_blank(),
      plot.title         = element_text(face = "bold", hjust = 0.5, size = 11),
      legend.position    = "right"
    )
}

library(grid)

make_target_pair <- function(tgt) {
  dat <- delta_df %>% filter(target == tgt)
  left  <- col_plot(dat %>% filter(sign == "+"),  show_y = TRUE,  col_title = "+")
  right <- col_plot(dat %>% filter(sign == "-"), show_y = FALSE, col_title = "\u2212")
  pair <- left | right
  title_grob <- grid::textGrob(tgt, gp = grid::gpar(fontsize = 13, fontface = "bold"))
  wrap_elements(grid::grobTree(title_grob)) / pair + plot_layout(heights = c(0.06, 1))
}

# Build and combine with one legend
p_ar  <- make_target_pair("Arousal")
p_con <- make_target_pair("Contentment")
p_sad <- make_target_pair("Sadness")

final_plot <- (p_con | p_sad | p_ar) + plot_layout(guides = "collect") &
  theme(legend.position = "right")

print(final_plot)

ggsave("figures/embed_interpretability.png", final_plot,
       width = 12, height = 8, units = "in", dpi = 300)














### OLD CODE ####






library(dplyr)
library(stringr)
library(glmnet)
library(tidyr)
library(purrr)
library(ggplot2)
library(forcats)

df <- audio_ema_features  # your merged table (has targets, LIWC cols, roberta_*)

# Identify columns
roberta_cols <- grep("^roberta_\\d+$", names(df), value = TRUE)

# Safer way to get LIWC cols: intersect with the original LIWC names you showed
liwc_names_in_source <- setdiff(names(liwc), "audio_id")
liwc_cols <- intersect(names(df), liwc_names_in_source)

targets <- c("content_num","sad_num","energy_num")
target_labels <- c(content_num = "Contentment", sad_num = "Sadness", energy_num = "Arousal")


fit_en_betas <- function(X, y, alpha = 0.5) {
  ok <- is.finite(y) & apply(X, 1, function(r) all(is.finite(r)))
  Xz <- scale(as.matrix(X[ok, , drop = FALSE]))
  yz <- scale(as.numeric(y[ok]))
  
  cv <- cv.glmnet(Xz, yz, family = "gaussian", alpha = alpha, standardize = FALSE)
  lam <- if (!is.null(cv$lambda.1se)) cv$lambda.1se else cv$lambda.min
  cm  <- as.matrix(coef(cv, s = lam))
  
  if (nrow(cm) < 2) return(tibble(feature = character(), beta = numeric()))
  tibble(feature = rownames(cm)[-1], beta = as.numeric(cm[-1, 1]))
}

# LIWC betas per target
betas_liwc <- map_dfr(targets, function(tgt) {
  out <- fit_en_betas(df[, liwc_cols], df[[tgt]])
  out %>% mutate(target = target_labels[[tgt]], family = "LIWC")
})

# Text-embedding betas per target
betas_emb <- map_dfr(targets, function(tgt) {
  out <- fit_en_betas(df[, roberta_cols], df[[tgt]])
  out %>% mutate(target = target_labels[[tgt]], family = "Text embeddings")
})

top_k <- 12
panelA <- betas_liwc %>%
  group_by(target) %>%
  slice_max(order_by = abs(beta), n = top_k, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(feature = fct_reorder(feature, beta))

gg_liwc <- ggplot(panelA, aes(x = feature, y = beta, fill = beta > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#377eb8", "FALSE" = "#e41a1c"), guide = "none") +
  facet_wrap(~ target, ncol = 1, scales = "free_y") +
  labs(x = NULL, y = "Standardized Elastic-Net coefficient (β)",
       title = "LIWC predictors of momentary emotion (top |β|)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank())

# pick top embedding dims per target
top_dims_per_target <- betas_emb %>%
  group_by(target) %>%
  slice_max(order_by = abs(beta), n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(dim = feature) %>%
  select(target, dim, beta)

# compute Spearman correlations with LIWC
corr_map <- map_dfr(seq_len(nrow(top_dims_per_target)), function(i) {
  tgt  <- top_dims_per_target$target[i]
  dimc <- top_dims_per_target$dim[i]
  b    <- top_dims_per_target$beta[i]
  
  x <- df[[dimc]]
  map_dfr(liwc_cols, function(lc) {
    y <- df[[lc]]
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 20) return(NULL)
    r <- suppressWarnings(cor(x[ok], y[ok], method = "spearman"))
    tibble(target = tgt, dim = dimc, dim_beta = b, liwc = lc, rho = r)
  })
})

# Keep strongest LIWC correlates per dim (absolute correlation)
panelB <- corr_map %>%
  group_by(target, dim) %>%
  slice_max(order_by = abs(rho), n = 8, with_ties = FALSE) %>%
  ungroup()

gg_map <- ggplot(panelB, aes(x = dim, y = liwc, fill = rho)) +
  geom_tile() +
  scale_fill_gradient2(low = "#e41a1c", mid = "white", high = "#377eb8", midpoint = 0,
                       name = "Spearman ρ") +
  facet_wrap(~ target, ncol = 1, scales = "free_y") +
  labs(x = "Most predictive embedding dimensions",
       y = "LIWC category",
       title = "Interpreting embeddings via LIWC (correlations with top dimensions)") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

library(patchwork)
(gg_liwc | gg_map) +
  plot_annotation(theme = theme(plot.title = element_text(hjust = 0)))
