# ----------------------------- packages -------------------------------------
suppressPackageStartupMessages({
  library(stringr)
  library(dplyr)
})

# read transcripts 
transcripts <- read.csv("data/transcripts_combined.csv")

# ------------------------ load PANAS-X word list -----------------------------
panasx_df <- read.csv("data/panasx_terms.csv", stringsAsFactors = FALSE)

# emotion_terms
emotion_terms <- panasx_df$panasx_term

# add extra explicit emotional state descriptors manually
extra_terms <- c(
  "stressed", "overwhelmed", "burned out", "exhausted",
  "anxious", "worried", 
  "depressed", 
  "chill"
)

emotion_terms <- unique(c(emotion_terms, extra_terms))

# split into multi-word phrases vs single words
phrase_terms <- grep("\\s", emotion_terms, value = TRUE)
single_terms <- setdiff(emotion_terms, phrase_terms)

# helper to escape regex metacharacters in literal phrases/words
re_escape <- function(x) gsub("([.\\^$|()\\[\\]{}*+?\\\\])", "\\\\\\1", x, perl = TRUE)

# build a single regex that matches ANY phrase term with word boundaries, case-insensitive
phrase_pattern <- if (length(phrase_terms)) {
  paste0("\\b(", paste(re_escape(phrase_terms), collapse = "|"), ")\\b")
} else {
  NA_character_
}

MASK <- "[EMO]"

# ------------------ tokenizer (preserve punctuation/spacing) -----------------
tokenize_keep_punct <- function(x) {
  if (is.na(x) || !nzchar(x)) return(character())
  # \p{L} letters, \p{N} numbers; keep internal apostrophes/hyphens/dashes
  str_extract_all(x, "\\p{L}[\\p{L}\\p{N}'’-]*|[^\\p{L}\\p{N}]+", simplify = FALSE)[[1]]
}

# ------------------------------ masking -------------------------------------
mask_text_emotions <- function(txt) {
  if (is.na(txt) || !nzchar(txt)) return(txt)
  
  # 1) mask multi-word phrases first (e.g., "angry at self", "disgusted with self", "at ease")
  if (!is.na(phrase_pattern)) {
    txt <- str_replace_all(txt, regex(phrase_pattern, ignore_case = TRUE), MASK)
  }
  
  # 2) mask single-word terms while preserving punctuation/spacing
  chunks <- tokenize_keep_punct(txt)
  if (!length(chunks)) return(txt)
  
  masked <- vapply(
    chunks,
    FUN.VALUE = character(1),
    FUN = function(tok) {
      # word token? (starts with a letter)
      if (grepl("^\\p{L}", tok, perl = TRUE)) {
        low <- tolower(tok)
        if (low %in% single_terms) MASK else tok
      } else {
        tok
      }
    }
  )
  
  paste0(masked, collapse = "")
}

# ------------------------------ apply ---------------------------------------

transcripts_masked <- transcripts %>%
  mutate(
    text_masked = unname(vapply(text, mask_text_emotions, character(1)))
  )

# ---------------------- save data  -------------------------
write.csv(
  transcripts_masked %>%
    dplyr::select(-text) %>%
    dplyr::relocate(text_masked, .after = timestamp),
  file = "data/transcripts_masked.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------ additional: QA summary -----------------------------------
# helper: count literal [EMO] markers quickly and robustly
count_mask_tokens <- function(txt) {
  if (is.na(txt) || !nzchar(txt)) return(0L)
  m <- gregexpr("\\[EMO\\]", txt, perl = TRUE)[[1]]
  if (length(m) == 1L && m[1] == -1L) 0L else length(m)
}

# word counter that EXCLUDES [EMO] tokens
count_words_no_EMO <- function(txt) {
  if (is.na(txt) || !nzchar(txt)) return(0L)
  chunks <- tokenize_keep_punct(txt)
  sum(grepl("^\\p{L}", chunks, perl = TRUE) & !grepl("^EMO\\]?$", chunks))
}

qa_summary <- audio_ema_masked %>%
  mutate(
    words_pre   = vapply(transcript,        count_words_no_EMO, integer(1)),
    mask_count  = vapply(transcript_masked, count_mask_tokens,  integer(1)),
    words_post  = pmax(words_pre - mask_count, 0L),
    masked_pct  = if_else(words_pre > 0, mask_count / words_pre, 0)
  ) %>%
  summarize(
    n                   = n(),
    total_words_pre     = sum(words_pre,   na.rm = TRUE),
    total_masked        = sum(mask_count,  na.rm = TRUE),
    pct_masked_overall  = total_masked / total_words_pre,
    median_masked_pct   = median(masked_pct, na.rm = TRUE),
    iqr_l               = quantile(masked_pct, 0.25, na.rm = TRUE),
    iqr_h               = quantile(masked_pct, 0.75, na.rm = TRUE),
    median_words_pre    = median(words_pre,  na.rm = TRUE),
    median_words_post   = median(words_post, na.rm = TRUE)
  )

print(qa_summary)

## finish