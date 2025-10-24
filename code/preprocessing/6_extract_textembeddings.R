### 0) SETUP ###############################################################

library(dplyr)
library(text)   # for textEmbed()

# read in data
audio_ema <- readRDS("data/audio_ema.rds")

# pull out transcripts & audio_ids
texts   <- audio_ema$transcript_masked # either the regular transcripts or the masked ones
audioid <- audio_ema$audio_id

stopifnot(length(texts) == length(audioid))

### 1) PARAMETERS #########################################################

batch_size <- 500
n          <- length(texts)
n_batches  <- ceiling(n / batch_size)

# where to store per-batch results
batch_dir <- "data/text_embeddings/batches_masked"
dir.create(batch_dir, recursive = TRUE, showWarnings = FALSE)

### 2) BATCH EMBEDDING LOOP (writes RDS per batch) ########################

for (b in seq_len(n_batches)) {
  i1 <- (b - 1) * batch_size + 1
  i2 <- min(b * batch_size, n)
  
  batch_file <- file.path(batch_dir, sprintf("batch_%05d.rds", b))
  
  # resume-friendly: skip if already computed
  if (file.exists(batch_file)) {
    message(sprintf("Batch %d/%d exists → skip  [%d → %d]", b, n_batches, i1, i2))
    next
  }
  
  # your slice of transcripts + ids
  batch_texts <- texts[i1:i2]
  batch_ids   <- audioid[i1:i2]
  
  # extract embeddings
  out <- textEmbed(
    batch_texts,
    model                 = "roberta-large",
    layers                = 23,          # penultimate
    tokenizer_parallelism = TRUE,
    device                = "cpu"
  )
  
  # text 1.2.0 returns embeddings in out$texts$texts (tibble n × 1024)
  mat <- as.matrix(out$texts$texts)
  colnames(mat) <- paste0("roberta_", seq_len(ncol(mat)))
  
  # build a tibble with audio_id + the embeddings
  batch_tbl <- tibble(audio_id = batch_ids) %>%
    bind_cols(as.data.frame(mat))
  
  # write this batch; uncompressed = fastest (final file will be compressed)
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