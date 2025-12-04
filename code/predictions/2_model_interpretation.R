# load packages

library(mlr3)
library(data.table)
library(mlr3misc)
library(dplyr)
library(purrr)
library(tidyr)
library(knitr)
library(ggplot2)
library(grid)
library(patchwork)
library(ggnewscale)

## --- read benchmark and prep --------------------------------------------------
bmr_en <- readRDS("results/bmr_en.rds")

`%||%` <- function(x, y) if (is.null(x)) y else x

dt <- as.data.table(bmr_en)

## --- identify tasks ---------------------------------------
dt[, task_id    := vapply(task,    function(t) t$id, character(1))]
dt[, learner_id := vapply(learner, function(l) l$id, character(1))]
dt[, fold       := if (!is.null(iteration)) iteration else seq_len(.N)]

## --- helper: recursively search for cv.glmnet -------------------------

get_cv_glmnet <- function(lrn) {
  
  find_cv <- function(x, depth = 0) {
    if (is.null(x) || depth > 5) return(NULL)
    
    # direct hit
    if (inherits(x, "cv.glmnet")) return(x)
    
    # lists
    if (is.list(x)) {
      for (el in x) {
        res <- find_cv(el, depth + 1)
        if (!is.null(res)) return(res)
      }
      return(NULL)
    }
    
    # environments
    if (is.environment(x)) {
      for (nm in ls(x, all.names = TRUE)) {
        res <- find_cv(get(nm, envir = x), depth + 1)
        if (!is.null(res)) return(res)
      }
      return(NULL)
    }
    
    # R6 objects: check fields + enclosing envs
    if ("R6" %in% class(x)) {
      envs <- list(
        x,
        x$.__enclos_env__$public,
        x$.__enclos_env__$private
      )
      for (e in envs) {
        if (is.environment(e)) {
          res <- find_cv(e, depth + 1)
          if (!is.null(res)) return(res)
        }
      }
      # also inspect common slots directly
      if (!is.null(x$model)) {
        res <- find_cv(x$model, depth + 1)
        if (!is.null(res)) return(res)
      }
      if (!is.null(x$state)) {
        res <- find_cv(x$state, depth + 1)
        if (!is.null(res)) return(res)
      }
      return(NULL)
    }
    
    NULL
  }
  
  find_cv(lrn)
}

### --- extract betas for one learner (one fold) -------------------------

extract_betas <- function(lrn) {
  cv <- get_cv_glmnet(lrn)
  if (is.null(cv)) return(NULL)
  
  fit <- cv$glmnet.fit  # glmnet "elnet" object (beta is a dgCMatrix)
  
  # column index of lambda.1se in beta/a0
  idx <- cv$index["1se", "Lambda"]
  if (is.na(idx) || idx < 1 || idx > length(fit$lambda)) return(NULL)
  
  beta_mat   <- as.matrix(fit$beta)          # now a standard numeric matrix
  beta_col   <- beta_mat[, idx, drop = TRUE] # numeric vector
  feat_names <- rownames(beta_mat)
  # -------------------------------------------------------------------
  
  if (length(beta_col) == 0) return(NULL)
  
  data.frame(
    feature   = feat_names,
    beta      = as.numeric(beta_col),
    lambda    = fit$lambda[idx],
    lambda_id = "lambda.1se",
    stringsAsFactors = FALSE
  )
}



## --- collect betas for a given task_id --------------------------------

collect_task_betas <- function(task_name) {
  rows <- dt[task_id == task_name & learner_id == "regr.cv_glmnet"]
  if (nrow(rows) == 0) return(NULL)
  
  dfs <- lapply(seq_len(nrow(rows)), function(i) {
    bet <- extract_betas(rows$learner[[i]])
    if (is.null(bet)) return(NULL)
    bet$task_id    <- task_name
    bet$fold       <- rows$fold[i]
    bet$learner_id <- rows$learner_id[i]
    bet
  })
  
  dfs <- Filter(Negate(is.null), dfs)
  if (!length(dfs)) return(NULL)
  
  data.table::rbindlist(dfs, use.names = TRUE, fill = TRUE)
}

we_tasks   <- c("wordembeddings_arousal", "wordembeddings_content", "wordembeddings_sad")
liwc_tasks <- c("liwc_arousal", "liwc_content", "liwc_sad")

we_tasks   <- intersect(we_tasks,   unique(dt$task_id))
liwc_tasks <- intersect(liwc_tasks, unique(dt$task_id))

we_list   <- lapply(we_tasks,   collect_task_betas)
liwc_list <- lapply(liwc_tasks, collect_task_betas)

we_list   <- Filter(Negate(is.null), we_list)
liwc_list <- Filter(Negate(is.null), liwc_list)

betas_we   <- if (length(we_list))   data.table::rbindlist(we_list,   use.names = TRUE, fill = TRUE) else data.table::data.table()
betas_liwc <- if (length(liwc_list)) data.table::rbindlist(liwc_list, use.names = TRUE, fill = TRUE) else data.table::data.table()

head(betas_we)
head(betas_liwc)


## --- annotate target + modality ---------------------------------------------
if (nrow(all_betas)) {
  all_betas[, target := fifelse(grepl("_arousal$", task_id), "arousal",
                                fifelse(grepl("_content$", task_id), "content", "sad"))]
  all_betas[, modality := fifelse(grepl("^liwc_", task_id), "liwc", "wordembeddings")]
}

## --- combine WE + LIWC betas into one table ---------------------------------

all_betas <- rbindlist(
  list(betas_we, betas_liwc),
  use.names = TRUE,
  fill      = TRUE
)

## --- annotate target + modality ---------------------------------------------
if (nrow(all_betas) > 0) {
  all_betas[, target := fifelse(
    grepl("_arousal$", task_id), "arousal",
    fifelse(grepl("_content$", task_id), "content", "sad")
  )]
  
  all_betas[, modality := fifelse(
    grepl("^liwc_", task_id), "liwc",
    "wordembeddings"
  )]
}

## quick sanity
dim(all_betas)
head(all_betas)

## --- per-target summaries, stratified by modality ---------------------------

# all_betas is already a data.table (from rbindlist), but this is harmless:
all_betas_dt <- as.data.table(all_betas)

beta_summary_all <- all_betas_dt[
  ,
  .(
    mean_beta     = mean(beta),
    mean_abs_beta = mean(abs(beta)),
    nonzero_cnt   = sum(beta != 0),
    nonzero_prop  = sum(beta != 0) / .N
  ),
  by = .(modality, target, feature)
][order(modality, target, -nonzero_cnt, -abs(mean_beta))]

beta_summary_all


### code for table 1 ####


# Targets
targets <- c("ema_content" = "Contentment",
             "ema_sad"     = "Sadness",
             "ema_energy"  = "Arousal")

# LIWC columns (numeric, excluding audio_id if present)
liwc_cols <- beta_summary_all %>%
  filter(modality == "liwc") %>%
  distinct(feature) %>%
  pull(feature)

# --- summarise betas for LIWC features per target (denominator = n_models) ---
liwc_betas <- all_betas_dt %>%
  dplyr::filter(grepl("^liwc_", task_id)) %>%
  dplyr::group_by(target, feature) %>%
  dplyr::summarise(
    mean_beta    = mean(beta),
    q_low        = quantile(beta, 0.025, na.rm = TRUE),
    q_high       = quantile(beta, 0.975, na.rm = TRUE),
    nonzero_cnt  = sum(beta != 0),
    n_models     = dplyr::n(),              # how many fits contributed
    nonzero_prop = nonzero_cnt / n_models,
    .groups = "drop"
  ) %>%
  dplyr::arrange(target, dplyr::desc(abs(mean_beta)))

# --- Top-10 per target, beta string now uses the 95% range -------------------
liwc_top5 <- liwc_betas %>%
  dplyr::group_by(target) %>%
  dplyr::slice_max(order_by = abs(mean_beta), n = 5, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    beta_str = sprintf(
      "%+.2f (95%% range=%.2f–%.2f, sel=%d/%d)",
      mean_beta, q_low, q_high, nonzero_cnt, n_models
    )
  )

# --- reshape into wide table ---
liwc_table_df <- liwc_top5 %>%
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

print(liwc_table_df)

# save table
write.csv(liwc_table_df, "results/en_liwc_betas.csv")


### code for figure 2 ## 

library(dplyr)
library(forcats)
library(tidyr)
library(ggplot2)
library(patchwork)

# 0) Features frame with targets, LIWC cols, and roberta_* dims
features_df <- readRDS("data/audio_ema_matched_cleaned.rds")

# 2) betas_emb 
betas_emb <- all_betas_dt[
  grepl("^wordembeddings_", task_id) & grepl("^roberta_\\d+$", feature)
][
  , .(beta = mean(beta, na.rm = TRUE)), by = .(target, feature)
][
  , target := factor(target,
                     levels = c("arousal","content","sad"),
                     labels = c("Arousal","Contentment","Sadness"))
][]

targets_order <- c("Arousal","Contentment","Sadness")

# 2) LIWC-22 lookup: abbreviation -> full label (subset of categories used here)

liwc_lookup <- tibble::tribble(
  ~liwc,         ~liwc_full,
  # Contentment panel
  "i",           "1st person singular",
  "Cognition",   "Cognition",
  "ppron",       "Personal pronouns",
  "negate",      "Negations",
  "pronoun",     "Total pronouns",
  
  # Sadness panel
  "tone_neg",    "Negative tone",
  "emo_neg",     "Negative emotion",
  "emo_anx",     "Anxiety",
  "verb",        "Common verbs",
  "leisure",     "Leisure",
  
  # Arousal panel
  "emo_pos",     "Positive emotion",
  "tone_pos",    "Positive tone",
  "home",        "Home",
  "focuspresent","Present focus"
)


# ==== Figure 2 build ==========================================================
TOP_N <- 5  # LIWC categories per target (ranked by |Δρ|)

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
  pos_lab <- paste0(
    gsub("^roberta_", "", pos_emb),
    "\n(β = ", sprintf("%+.02f", pos_row$beta[1]), ")"
  )
  
  neg_lab <- paste0(
    gsub("^roberta_", "", neg_emb),
    "\n(β = ", sprintf("%+.02f", neg_row$beta[1]), ")"
  )
  
  
  # long format; keep `delta` so we can order by it (shared across + / −)
  both_long <- tidyr::pivot_longer(
    both,
    cols = c(rho_pos, rho_neg),
    names_to = "sign_code",
    values_to = "rho"
  ) %>%
    mutate(
      sign            = ifelse(sign_code == "rho_pos", "+", "-"),
      embedding_label = ifelse(sign == "+", pos_lab, neg_lab),
      target          = target_label
    ) %>%
    left_join(liwc_lookup, by = c("liwc" = "liwc")) %>%
    mutate(
      liwc_full = if_else(is.na(liwc_full), liwc, liwc_full),
      # order by delta using full labels
      liwc_full = forcats::fct_reorder(liwc_full, delta, .desc = TRUE)
    ) %>%
    select(target, sign, liwc = liwc_full, rho, embedding_label, delta)
  
  
  both_long
}


# Build for all targets
delta_list <- lapply(targets_order, build_delta_tbl)
delta_list <- delta_list[!vapply(delta_list, is.null, logical(1))]
delta_df   <- bind_rows(delta_list)
stopifnot(nrow(delta_df) > 0)

# unified color limits for ONE legend
L <- max(abs(delta_df$rho), na.rm = TRUE)


## ---------- plotting helper --------------------------------------------------

col_plot <- function(dat,
                     show_y = TRUE,
                     col_title = "+",
                     show_size_legend = FALSE) {
  
  # remove any NA rows to avoid stray labels
  dat <- dat %>%
    dplyr::filter(!is.na(embedding_label),
                  !is.na(liwc),
                  is.finite(rho)) %>%
    droplevels()
  
  if (nrow(dat) == 0L) {
    return(
      ggplot() + theme_void() + labs(title = col_title)
    )
  }
  
  # base plot: correct sizes, only color legend (order = 1)
  p <- ggplot(dat,
              aes(x = embedding_label,
                  y = liwc,
                  color = rho,
                  size  = abs(rho))) +
    geom_point() +
    scale_y_discrete(drop = TRUE, limits = function(x) rev(x)) +
    scale_x_discrete(
      guide       = guide_axis(n.dodge = 2),
      na.translate = FALSE      # <- remove NA tick
    ) +
    scale_color_gradient2(
      limits   = c(-L, L),
      low      = "#d73027",
      mid      = "white",
      high     = "#1a9850",
      midpoint = 0,
      name     = "Spearman \u03C1"
    ) +
    scale_size_continuous(
      range = c(2.6, 6),
      guide = "none",
      name  = NULL
    ) +
    guides(color = guide_colorbar(order = 1)) +
    labs(title = col_title, x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x        = element_text(angle = 15, hjust = 0.5, vjust = 1),
      axis.text.y        = if (show_y) element_text() else element_blank(),
      plot.title         = element_text(face = "bold", hjust = 0.5, size = 11)
    )
  
  if (!show_size_legend) return(p)
  
  # dummy points only for size legend (not visible in panel)
  legend_df <- data.frame(
    embedding_label = factor(rep(dat$embedding_label[1], 3),
                             levels = levels(dat$embedding_label)),
    liwc            = factor(rep(dat$liwc[1], 3),
                             levels = levels(dat$liwc)),
    size_level      = factor(c(".05", ".15", ".25"),
                             levels = c(".05", ".15", ".25"))
  )
  
  p +
    ggnewscale::new_scale("size") +
    geom_point(
      data        = legend_df,
      aes(x = embedding_label, y = liwc, size = size_level),
      inherit.aes = FALSE,
      alpha       = 0
    ) +
    scale_size_manual(
      name   = NULL,
      values = c(".05" = 2.6, ".15" = 4, ".25" = 6),
      breaks = c(".05", ".15", ".25"),
      labels = c(".05", ".15", ".25")
    ) +
    guides(
      size = guide_legend(
        order        = 2,   # BELOW the colorbar
        title        = NULL,
        override.aes = list(
          shape  = 21,
          fill   = NA,
          color  = "black",
          stroke = 0.8,
          alpha  = 1
        )
      )
    )
}

## ---------- wrap +/− panels per target --------------------------------------

make_target_pair <- function(tgt, show_size_legend = FALSE) {
  dat <- delta_df %>% dplyr::filter(target == tgt)
  
  left  <- col_plot(dat %>% dplyr::filter(sign == "+"),
                    show_y = TRUE,
                    col_title = "+",
                    show_size_legend = show_size_legend)
  
  right <- col_plot(dat %>% dplyr::filter(sign == "-"),
                    show_y = FALSE,
                    col_title = "\u2212",
                    show_size_legend = FALSE)
  
  pair <- left | right
  title_grob <- grid::textGrob(
    tgt,
    gp = grid::gpar(fontsize = 13, fontface = "bold")
  )
  wrap_elements(grid::grobTree(title_grob)) /
    pair + plot_layout(heights = c(0.06, 1))
}

## ---------- build final figure ----------------------------------------------

p_con <- make_target_pair("Contentment", show_size_legend = TRUE)
p_sad <- make_target_pair("Sadness",    show_size_legend = FALSE)
p_ar  <- make_target_pair("Arousal",    show_size_legend = FALSE)

final_plot <- (p_con | p_sad | p_ar) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "right",
    legend.box      = "vertical",   # colorbar on top, circles below
    legend.title    = element_text(hjust = 0.5)
  )

print(final_plot)

ggsave("figures/top_embed_interpretability.png", final_plot,
       width = 10, height = 5, units = "in", dpi = 300)


# finish