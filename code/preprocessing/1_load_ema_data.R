library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(purrr)
`%||%` <- rlang::`%||%`

ema_dir   <- "data/archive/study2/ema"
ema_files <- list.files(ema_dir, pattern = "\\.RDS$", full.names = TRUE)

# --- recoder aligned to your instrument ---
recode_ema_answer <- function(question_id, answer_chr) {
  a    <- stringr::str_trim(answer_chr %||% "")
  a_lc <- stringr::str_to_lower(a)
  
  # explicit missing tokens -> NA
  if (a == "" || a_lc %in% c("not_presented","not presented","no_answer_selected",
                             "skipped","skip question","na","n/a","null")) {
    return(NA_real_)
  }
  
  # already numeric? keep AS-IS, including 0
  num <- suppressWarnings(readr::parse_number(a))
  if (!is.na(num)) return(as.numeric(num))
  
  # map labels -> numeric with 0-based scales
  if (question_id %in% c("content","sad")) {
    # 0..3 scale
    map4 <- c(
      "not at all"   = 0,
      "a little bit" = 1,
      "quite a bit"  = 2,
      "very much"    = 3
    )
    return(unname(map4[a_lc] %||% NA_real_))
  }
  
  if (question_id == "energy") {
    # assume 0..4 scale in your exports; supports both “low energy” and short forms
    map5_0based <- c(
      "low energy"           = 0, "low"            = 0,
      "somewhat low energy"  = 1, "somewhat low"   = 1,
      "neutral"              = 2,
      "somewhat high energy" = 3, "somewhat high"  = 3,
      "high energy"          = 4, "high"           = 4
    )
    return(unname(map5_0based[a_lc] %||% NA_real_))
  }
  
  NA_real_
}


process_ema <- function(path) {
  obj <- readRDS(path)
  ans <- obj$survey_answers
  if (is.null(ans)) ans <- obj$survey_anwers  # some exports misspelled
  
  if (is.null(ans) || nrow(ans) == 0) {
    return(tibble(
      participant_id = tools::file_path_sans_ext(basename(path)),
      survey_time    = as.POSIXct(NA),
      content        = NA_character_,
      sad            = NA_character_,
      energy         = NA_character_,
      content_num    = NA_real_,
      sad_num        = NA_real_,
      energy_num     = NA_real_
    ))
  }
  
  ans <- ans %>%
    mutate(
      answer  = as.character(answer),
      q_clean = question.text %>% str_squish() %>% str_replace(":$", "")
    ) %>%
    mutate(
      question_id = case_when(
        str_detect(q_clean, regex("\\bcontent\\b", ignore_case = TRUE)) ~ "content",
        str_detect(q_clean, regex("\\bsad\\b",     ignore_case = TRUE)) ~ "sad",
        str_detect(q_clean, regex("\\benergy\\b",  ignore_case = TRUE)) ~ "energy",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(question_id)) %>%
    mutate(answer_num = mapply(recode_ema_answer, question_id, answer) %>% as.numeric())
  
  # pivot to text + numeric columns
  wide <- ans %>%
    select(filename, question_id, answer, answer_num) %>%
    distinct() %>%
    pivot_wider(
      id_cols     = filename,
      names_from  = question_id,
      values_from = c(answer, answer_num),
      values_fn   = list(answer = ~ first(.x), answer_num = ~ first(.x)),
      values_fill = list(answer = NA_character_, answer_num = NA_real_),
      names_glue  = "{question_id}{ifelse(.value=='answer_num','_num','')}"
    )
  
  for (nm in c("content","sad","energy","content_num","sad_num","energy_num")) {
    if (!nm %in% names(wide)) wide[[nm]] <- if (grepl("_num$", nm)) NA_real_ else NA_character_
  }
  
  wide %>%
    mutate(
      participant_id = tools::file_path_sans_ext(basename(path)),
      survey_time    = as.POSIXct(sub("\\.csv$", "", filename),
                                  format = "%Y-%m-%d_%H-%M-%S", tz = "UTC")
    ) %>%
    select(participant_id, survey_time, content, sad, energy, content_num, sad_num, energy_num)
}

ema_all <- ema_files %>%
  set_names(ema_files) %>%
  map_dfr(process_ema, .id = "file_path")

# quick audit: show labels that didn't map (non-empty text but NA numeric)
audit_unmapped <- function(df, q) {
  df %>%
    filter(!is.na(.data[[q]]), str_trim(.data[[q]]) != "", is.na(.data[[paste0(q, "_num")]])) %>%
    count(.data[[q]], sort = TRUE, name = "n")
}
list(
  unmapped_content = audit_unmapped(ema_all, "content"),
  unmapped_sad     = audit_unmapped(ema_all, "sad"),
  unmapped_energy  = audit_unmapped(ema_all, "energy")
)


# drop columns without responses

ema_clean <- ema_all %>%
  # drop helper col
  select(-file_path) %>%
  
  # keep only rows where at least one target emotion was answered
  filter(!(is.na(content_num) & is.na(sad_num) & is.na(energy_num))) %>%
  
  # create ema_id
  mutate(
    ema_id = paste0(
      participant_id, "_",
      format(survey_time, "%Y%m%d%H%M%S")
    )
  )


# save data
saveRDS(ema_clean, "data/ema_data.rds")

# finish