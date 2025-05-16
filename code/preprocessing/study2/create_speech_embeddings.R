## SET UP PYTHON ENVIRONMENT ###

# load required packages
library(reticulate)

# set up conda environment (on apple machine)

# # install a minimal conda just for reticulate
# reticulate::install_miniconda()
# 
# # check that it’s there
# reticulate::miniconda_available()   # should return TRUE
# 
# # now create (or install) your env
# reticulate::conda_create("speech-env", packages = c("python=3.9","pip"))
# reticulate::conda_install("speech-env", 
#                           packages = c("transformers","torch","torchaudio"),
#                           pip = TRUE)
# 
# # finally tell reticulate to use it
# reticulate::use_condaenv("speech-env", required = TRUE)

# set up python environment on arm server

# # Create a virtualenv named "speech-env" using your system python3
# if (!"speech-env" %in% reticulate::virtualenv_list()) {
#   reticulate::virtualenv_create(
#     envname = "speech-env",
#     python  = "python3"
#   )
#   # 2) Install the required packages via pip
#   reticulate::virtualenv_install(
#     envname  = "speech-env",
#     packages = c("transformers", "torch", "torchaudio")
#   )
# }

# # install huggingface hub
# reticulate::conda_install(
#   envname  = "speech-env",
#   packages = c("huggingface_hub"),
#   pip      = TRUE,
#   channel  = "conda-forge"
# )

# Tell reticulate to use that virtualenv
#reticulate::use_condaenv("speech-env", required = TRUE) # on apple machine
reticulate::use_virtualenv("speech-env", required = TRUE) # on server

# Check it picked up the right Python
reticulate::py_config()

# connect to huggingface hub
hub <- import("huggingface_hub")

repo      <- "audeering/wav2vec2-large-robust-12-ft-emotion-msp-dim"
model_dir <- normalizePath(
  "~/affect_speech/data/study2/speech_model/wav2vec2-msp-dim",
  mustWork   = FALSE
)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

# 1) Download the minimal set of files
for (fname in c("config.json", "pytorch_model.bin", "preprocessor_config.json")) {
  hub$hf_hub_download(
    repo_id   = repo,
    filename  = fname,
    cache_dir = model_dir  )
}


# # test dowload hubert model: Where we’ll store the model
# local_model_path <- normalizePath(
#   "~/affect_speech/data/study2/speech_model/hubert-base-ls960",
#   mustWork = FALSE
# )
# dir.create(local_model_path, recursive = TRUE, showWarnings = FALSE)
# 
# # Download only the speech foundation model
# files_to_download <- c(
#   "config.json",
#   "pytorch_model.bin",
#   "preprocessor_config.json"   # Wav2Vec2FeatureExtractor needs this
# )
# 
# for (fname in files_to_download) {
#   message("Downloading ", fname, " …")
#   hub$hf_hub_download(
#     repo_id    = "facebook/hubert-base-ls960",
#     filename   = fname,
#     cache_dir  = local_model_path
#   )
# }

# import the HF modules
transformers <- import("transformers")
torch        <- import("torch")
torchaudio   <- import("torchaudio")

# load from your local clone, offline only
local_model_path <- normalizePath(
  "~/affect_speech/data/study2/speech_model/wav2vec2-msp-dim/models--audeering--wav2vec2-large-robust-12-ft-emotion-msp-dim/snapshots/6eba34a2485ea31cb03600241787c3a5edab8626",
  mustWork = TRUE
)

# load feature extractor and model locally
feature_extractor <- transformers$Wav2Vec2FeatureExtractor$from_pretrained(
  local_model_path,
  local_files_only = TRUE
)

model <- transformers$Wav2Vec2Model$from_pretrained(
  local_model_path,
  local_files_only  = TRUE)

# 3) Define a function that trims to the first/last word timestamps, then extracts embeddings
get_speech_embedding_trimmed <- function(rec) {
  wav        <- rec$audio_wave
  transcript <- rec$speech.to.text
  
  sig <- as.numeric(wav$sig[,1])
  fs  <- as.integer(wav$fs)
  
  # find only the parts that carry start_time/end_time
  parts <- transcript[
    sapply(transcript, function(x) is.list(x) && !is.null(x$start_time))
  ]
  
  if (length(parts) > 0) {
    start_times <- unlist(lapply(parts, `[[`, "start_time"))
    end_times   <- unlist(lapply(parts, `[[`, "end_time"))
    begin_sec   <- min(start_times)
    end_sec     <- max(end_times)
    i1 <- max(1, floor(begin_sec * fs) + 1)
    i2 <- min(length(sig),   ceiling(end_sec * fs))
    sig <- sig[i1:i2]
  }
  # else: leave sig as-is (no trimming)
  
  # now the usual embedding pipeline
  tensor <- torch$tensor(sig, dtype = torch$float32)$unsqueeze(0L)
  if (fs != 16000L) {
    tensor <- torchaudio$transforms$Resample(
      orig_freq = fs, new_freq = 16000L
    )(tensor)
  }
  
  inputs_py <- feature_extractor(
    tensor$squeeze()$numpy(),
    sampling_rate  = 16000L,
    return_tensors = "pt"
  )
  
  with(torch$no_grad(), {
    out <- model$forward(input_values = inputs_py$input_values)
  })
  
  pooled   <- torch$mean(out$last_hidden_state, dim = as.integer(1))
  squeezed <- torch$squeeze(pooled, dim = as.integer(0))
  as.numeric(squeezed$numpy())
}


# 4) Discover all user RDS files & compute total number of recordings
library(tools)
rds_dir    <- "data/study2/files_raw"
rds_files  <- list.files(rds_dir, "\\.RDS$", full.names = TRUE)
n_users    <- length(rds_files)
rec_counts <- vapply(rds_files, function(f) length(readRDS(f)), integer(1))
total_recs <- sum(rec_counts)

# 5) Determine embedding dimensionality from one trimmed example
one_rec <- readRDS(rds_files[1])[[1]]
D       <- length(get_speech_embedding_trimmed(one_rec))

# 6) Pre‐allocate the embeddings matrix and row‐names vector
big_mat   <- matrix(NA_real_, nrow = total_recs, ncol = D)
colnames(big_mat) <- paste0("wav2vec2_", seq_len(D))
row_names <- character(total_recs)

# 7) Loop over users and recordings, extract trimmed embeddings & build row names
row <- 1L
for (ui in seq_along(rds_files)) {
  user_path  <- rds_files[ui]
  user_id    <- file_path_sans_ext(basename(user_path))
  audio_list <- readRDS(user_path)
  n_rec      <- length(audio_list)
  
  # timestamps come from the list names (e.g. "2018-10-10_17-38-16")
  timestamps <- file_path_sans_ext(names(audio_list))
  
  for (j in seq_len(n_rec)) {
    big_mat[row, ] <- get_speech_embedding_trimmed(audio_list[[j]])
    row_names[row] <- paste0(user_id, "_", timestamps[j])
    row <- row + 1L
  }
  
  # free per-user memory immediately
  rm(audio_list); gc()
  
  # status message
  pct_done <- ui / n_users * 100
  message(sprintf(
    "[%3d/%3d] Processed user '%s' (%d recordings) — %.1f%% done",
    ui, n_users, user_id, n_rec, pct_done
  ))
}

# 8) Assign the row names and save
rownames(big_mat) <- row_names
message("Done! Extracted ", total_recs, " embeddings of dimension ", D, ".")
saveRDS(big_mat, "data/study2/speech_embeddings.rds")





# # load file and inspect it 
# 
audio <- readRDS("data/study2/files_raw/1ft8rnux.RDS")
# 
# # explore the file
# dim(audio)
# length(audio)
# 
# # each file contains all speech records from each single participant
audio$`2018-10-10_17-38-16.mp4`$speech.to.text
# 
# sample1 <- audio[[1]]
# 
# str(sample1$audio_wave)
# 
# # 5) test on one sample
# embedding_1 <- get_speech_embedding(sample1$audio_wave)
# length(embedding_1)  # e.g. 1024
# 
# 
# # 6) batch over all your Sound objects
# emb_list   <- lapply(all_samples, function(x) get_speech_embedding(x$audio_wave))
# emb_matrix <- do.call(rbind, emb_list)
# colnames(emb_matrix) <- paste0("hubert_large_", seq_len(ncol(emb_matrix)))

# Now cbind `emb_matrix` to your low-level voice features, RoBERTa text embeddings,
# and feed into your randomForest regression on the 4-point scales.
