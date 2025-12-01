# load packages
library(dplyr)
library(text)   # textEmbed()

# read in data
transcripts <- read.csv("data/transcripts_masked.csv")

texts           <- transcripts$text_masked
participant_id  <- transcripts$participant_id
timestamp       <- transcripts$timestamp

stopifnot(
  length(texts) == nrow(transcripts),
  length(texts) == length(participant_id),
  length(texts) == length(timestamp)
)

# ---------------- params ----------------
batch_size <- 500
n          <- length(texts)
n_batches  <- ceiling(n / batch_size)

batch_dir <- "data/text_embeddings/batches_masked"
dir.create(batch_dir, recursive = TRUE, showWarnings = FALSE)

# ------------- batching loop ------------
for (b in seq_len(n_batches)) {
  i1 <- (b - 1) * batch_size + 1
  i2 <- min(b * batch_size, n)
  
  batch_file <- file.path(batch_dir, sprintf("batch_%05d.rds", b))
  if (file.exists(batch_file)) {
    message(sprintf("Batch %d/%d exists → skip  [%d → %d]", b, n_batches, i1, i2))
    next
  }
  
  batch_texts <- texts[i1:i2]
  batch_pid   <- participant_id[i1:i2]
  batch_ts    <- timestamp[i1:i2]
  
  out <- textEmbed(
    batch_texts,
    model                 = "roberta-large",
    layers                = 23,
    tokenizer_parallelism = TRUE,
    device                = "cpu"
  )
  
  mat <- as.matrix(out$texts$texts)
  colnames(mat) <- paste0("roberta_", seq_len(ncol(mat)))
  
  batch_tbl <- tibble(
    participant_id = batch_pid,
    timestamp      = batch_ts
  ) %>% bind_cols(as.data.frame(mat))
  
  saveRDS(batch_tbl, batch_file, compress = FALSE)
  
  message(sprintf("Batch %d/%d done  [%d → %d]  → %s",
                  b, n_batches, i1, i2, basename(batch_file)))
  rm(out, mat, batch_tbl); gc()
}

### 3) COMBINE BATCHES & SAVE FINAL #######################################

# read all batch files in order and bind
batch_files <- list.files(batch_dir, pattern = "^batch_\\d+\\.rds$", full.names = TRUE)
if (length(batch_files) == 0) stop("No batch files found. Did the loop run?")

# ensure deterministic order
batch_idx <- as.integer(sub("^batch_(\\d+)\\.rds$", "\\1", basename(batch_files)))
batch_files <- batch_files[order(batch_idx)]

batch_list <- lapply(batch_files, readRDS)
textembeddings_robertalarge <- dplyr::bind_rows(batch_list)

# sanity check
stopifnot(nrow(textembeddings_robertalarge) == n)

saveRDS(
  textembeddings_robertalarge,
  "data/textembeddings_robertalarge_masked.rds"
)

message("All done!  ", nrow(textembeddings_robertalarge),
        " embeddings saved with audio_id. Final file: data/textembeddings_robertalarge_masked.rds")

# finish