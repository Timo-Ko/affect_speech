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

# get time
T1 <- Sys.time()
T1

# transform the text data to word embeddings
wordembeddings_robertalarge <- textEmbed(affect_voice$Text, 
                            model = 'roberta-large', # we are using the large roberta model
                            layers = 23, # second to last (layer 23) as standard approach
                            tokenizer_parallelism = T) 

# save results
saveRDS(wordembeddings_robertalarge, "data/study2/wordembeddings_robertalarge.RData")

# Save stopping time
T2 <- Sys.time()
T2

T2-T1 # compute time difference
# takes approx 16 hrs for roberta base, layer 11, 18k rows
# takes 17 hrs for roberta large layer 23, 13k rows

### APPEND EMBEDDING FEATURES ####

# read in word embeddings
wordembeddings_robertalarge <- readRDS("data/study2/wordembeddings_robertalarge.RData")

# create df that contains voice AND wordembedding features
affect_voice_wordembeddings <- cbind(affect_voice, wordembeddings_robertalarge$texts$texts)

# save data
saveRDS(affect_egemaps_wordembeddings, "data/study2/affect_voice_wordembeddings.rds")

# FINISH