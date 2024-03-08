### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "text")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# read in data frame
affect_acoustics <- readRDS("data/study2/affect_acoustics.RData")

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
wordembeddings_robertalarge <- textEmbed(affect_acoustics$Text, 
                            model = 'roberta-large', # we are using the large roberta model
                            layers = 23) # layer 23 as standard approach

# save results
saveRDS(wordembeddings_robertalarge, "data/study2/wordembeddings_robertalarge.RData")

# Save stopping time
T2 <- Sys.time()
T2

T2-T1 # compute time difference
# takes approx 16 hrs for roberta base, layer 11, 18k rows
# takes 17 hrs for roberta large layer 23, 13k rows

# create df with wordembedding features and merge with other data (ema, user id etc)

affect_wordembeddings <- cbind(affect_acoustics[,c(which(colnames(affect_acoustics)=="user_id"), 
                                                   which(colnames(affect_acoustics)=="timestamp"), 
                                                   which(colnames(affect_acoustics)=="Age"), 
                                                   which(colnames(affect_acoustics)=="Gender"), 
                                                   which(colnames(affect_acoustics)=="Total.words"), 
                                                   which(colnames(affect_acoustics)=="File.duration.in.seconds"), 
                                                   which(colnames(affect_acoustics)=="Voice.only.duration.in.seconds"), 
                                                   which(colnames(affect_acoustics)=="Sentiment.magnitude"), 
                                                   which(colnames(affect_acoustics)=="Sentiment.score"), 
                                                   which(colnames(affect_acoustics)=="content"),
                                                   which(colnames(affect_acoustics)=="md_content"), 
                                                   which(colnames(affect_acoustics)=="diff_content"), 
                                                   which(colnames(affect_acoustics)=="sad"),
                                                   which(colnames(affect_acoustics)=="md_sad"), 
                                                   which(colnames(affect_acoustics)=="diff_sad"), 
                                                   which(colnames(affect_acoustics)=="arousal"),
                                                   which(colnames(affect_acoustics)=="md_arousal"), 
                                                   which(colnames(affect_acoustics)=="diff_arousal"))], 
                               wordembeddings_robertalarge$texts$texts)

saveRDS(affect_wordembeddings, "data/study2/affect_wordembeddings.RData")

# create df that contains acoustic (eGeMAPS) AND wordembedding features

affect_egemaps_wordembeddings <- cbind(affect_acoustics[,c(which(colnames(affect_acoustics)=="user_id"), 
                                                             which(colnames(affect_acoustics)=="timestamp"), 
                                                             which(colnames(affect_acoustics)=="Age"), 
                                                             which(colnames(affect_acoustics)=="Gender"), 
                                                             which(colnames(affect_acoustics)=="Total.words"), 
                                                             which(colnames(affect_acoustics)=="File.duration.in.seconds"), 
                                                             which(colnames(affect_acoustics)=="Voice.only.duration.in.seconds"), 
                                                             which(colnames(affect_acoustics)=="Sentiment.magnitude"), 
                                                             which(colnames(affect_acoustics)=="Sentiment.score"), 
                                                             which(colnames(affect_acoustics)=="content"),
                                                             which(colnames(affect_acoustics)=="md_content"), 
                                                             which(colnames(affect_acoustics)=="diff_content"), 
                                                             which(colnames(affect_acoustics)=="sad"),
                                                             which(colnames(affect_acoustics)=="md_sad"), 
                                                             which(colnames(affect_acoustics)=="diff_sad"), 
                                                             which(colnames(affect_acoustics)=="arousal"),
                                                             which(colnames(affect_acoustics)=="md_arousal"), 
                                                             which(colnames(affect_acoustics)=="diff_arousal"), 
                                                             which(colnames(affect_acoustics)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics)=="equivalentSoundLevel_dBp"))], 
                                       wordembeddings_robertalarge$texts$texts)

saveRDS(affect_egemaps_wordembeddings, "data/study2/affect_egemaps_wordembeddings.RData")

# FINISH