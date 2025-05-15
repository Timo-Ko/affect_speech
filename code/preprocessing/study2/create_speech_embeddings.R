# load file and inspect it 

audio <- readRDS("data/study2/files_raw/1ft8rnux.RDS")

# explore the file
dim(audio)
length(audio)

# each file contains all speech records from each single participant
audio[[1]]

sample1 <- audio[[1]]

str(sample1$audio_wave)

# set up conda environemtn

# install a minimal conda just for reticulate
reticulate::install_miniconda()

# check that it’s there
reticulate::miniconda_available()   # should return TRUE

# now create (or install) your env
reticulate::conda_create("speech-env", packages = c("python=3.9","pip"))
reticulate::conda_install("speech-env", 
                          packages = c("transformers","torch","torchaudio"),
                          pip = TRUE)

# finally tell reticulate to use it
reticulate::use_condaenv("speech-env", required = TRUE)



# 1) load reticulate and point at your Python env
#library(reticulate)
#use_condaenv("speech-env", required = TRUE)

# 2) import the HF modules
transformers <- import("transformers")
torch        <- import("torch")
torchaudio   <- import("torchaudio")

# 3) load the official feature-extractor & backbone
feature_extractor <- transformers$Wav2Vec2FeatureExtractor$from_pretrained(
  "facebook/hubert-base-ls960"
)
model <- transformers$HubertModel$from_pretrained(
  "facebook/hubert-base-ls960"
)

# 4) embedding function (HuBERT backbone + mean-pool)
get_speech_embedding <- function(audio_wave) {
  # 1) pull signal & sample rate
  sig <- as.numeric(audio_wave$sig[,1])
  sr  <- as.integer(audio_wave$fs)
  
  # 2) tensor [1, N] and resample if needed
  tensor <- torch$tensor(sig, dtype = torch$float32)$unsqueeze(0L)
  if (sr != 16000L) {
    resampler <- torchaudio$transforms$Resample(
      orig_freq = sr, new_freq = 16000L
    )
    tensor <- resampler(tensor)
  }
  
  # 3) feature‐extractor
  inputs_py <- feature_extractor(
    tensor$squeeze()$numpy(),
    sampling_rate  = 16000L,
    return_tensors = "pt"
  )
  
  # 4) forward‐pass (no_grad)
  with(torch$no_grad(), {
    outputs_py <- model$forward(input_values = inputs_py$input_values)
  })
  
  # 5) mean‐pool over time dim (dim=1), then squeeze batch dim (dim=0)
  pooled <- torch$mean(outputs_py$last_hidden_state, dim = as.integer(1))
  squeezed <- torch$squeeze(pooled, dim = as.integer(0))
  
  # 6) to R numeric
  emb <- squeezed$numpy()
  as.numeric(emb)
}



# 5) test on one sample
embedding_1 <- get_speech_embedding(sample1$audio_wave)
length(embedding_1)  # e.g. 1024

# 6) batch over all your Sound objects
emb_list   <- lapply(all_samples, function(x) get_speech_embedding(x$audio_wave))
emb_matrix <- do.call(rbind, emb_list)
colnames(emb_matrix) <- paste0("hubert_large_", seq_len(ncol(emb_matrix)))

# Now cbind `emb_matrix` to your low-level voice features, RoBERTa text embeddings,
# and feed into your randomForest regression on the 4-point scales.
