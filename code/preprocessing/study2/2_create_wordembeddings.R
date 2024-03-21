### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "text")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# read in data frame
affect_voice <- readRDS("data/study2/affect_voice.rds")

## set up text environment - this needs to be done only once!

# Install text required python packages in a conda environment (with defaults).
#textrpp_install()

# Initialize the installed conda environment.
# save_profile = TRUE saves the settings so that you don't have to run textrpp_initialize() after restarting R. 
#textrpp_initialize(save_profile = TRUE)

### CREATE WORDEMBEDDINGS FROM TRANSCRIPTS ####

# extract embeddings in batches

texts <- affect_voice$Text

batch_size <- 100
n <- length(affect_voice$Text)
n_batches <- ceiling(n / batch_size)

embeddings <- list()

for(i in 1:n_batches) {
  start_index <- ((i - 1) * batch_size) + 1
  end_index <- min(i * batch_size, n)
  batch_texts <- texts[start_index:end_index]
  
  # Extract embeddings for this batch
  batch_embeddings <- textEmbed(batch_texts, 
                                model = 'roberta-large', # we are using the large roberta model
                                layers = 23, # second to last (layer 23) as standard approach
                                tokenizer_parallelism = T) 
  
  # save batch embeddings
  saveRDS(batch_embeddings$texts$texts, paste0("data/study2/word_embeddings/batches/batch", i, ".rds"))
  
  # write status
  print(paste(Sys.time(),":Embedding batch",i , "of", n_batches, "completed"))
  
  # remove embeddings from this batch to free up memory before next iteration
  rm(batch_embeddings)
}

# load all batches
folder_path <- "data/study2/word_embeddings/batches"

# List all .rds files in the folder
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)

# Read each .rds file and store the data frames in a list
list_of_dfs <- lapply(rds_files, readRDS)

# Combine all data frames into one large data frame
wordembeddings_robertalarge <- do.call(rbind, list_of_dfs)

# save results
saveRDS(wordembeddings_robertalarge, "data/study2/wordembeddings_robertalarge.rds")

# # get time
# T1 <- Sys.time()
# T1
# 
# # transform the text data to word embeddings
# wordembeddings_robertalarge <- textEmbed(affect_voice$Text, 
#                             model = 'roberta-large', # we are using the large roberta model
#                             layers = 23) # second to last (layer 23) as standard approach
# 
# # save results
# saveRDS(wordembeddings_robertalarge, "data/study2/wordembeddings_robertalarge.RData")
# 
# # Save stopping time
# T2 <- Sys.time()
# T2
# 
# T2-T1 # compute time difference
# # takes approx 16 hrs for roberta base, layer 11, 18k rows
# # takes 17 hrs for roberta large layer 23, 13k rows

### APPEND EMBEDDING FEATURES ####

# read in word embeddings
wordembeddings_robertalarge <- readRDS("data/study2/wordembeddings_robertalarge.rds")

# create df that contains voice AND wordembedding features
affect_voice_wordembeddings <- cbind(affect_voice, wordembeddings_robertalarge)

# remove text column
affect_voice_wordembeddings$Text <- NULL

# save data
saveRDS(affect_voice_wordembeddings, "data/study2/affect_voice_wordembeddings.rds")

# FINISH