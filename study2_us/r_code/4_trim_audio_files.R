## load required packages

require(tuneR)
require(googleCloudStorageR)

# load data frame containing timestamps
audio_transcript_df_metaonly_duration <- readRDS("data/audio_transcript_df_metaonly_duration.RData")

# set bucket path
bucket_path = 'gs://sable_data_2018' # this needs to be adapted 

# Get names of audio files in the GCP folder
listoffiles <- gcs_list_objects(bucket = "xxx")
audionames <- listoffiles$name

#### TRIM AUDIO RECORDS ####

for (i in audionames) {
  
# get the audio file name 
audiorecord <- paste0(bucket_path,i,".wav")

# get the respective timestamps
user_id <- substr(i, 1, 15) # get the user id, this needs to be adapted depending on what the file names looks like
audio_timestamp <- substr(i, 1, 15) # get the audio timestamp,  this needs to be adapted depending on what the file names looks like

# extract respective timestamp for the end of the last word from the data frame 
last_word_end_timestamp <- audio_transcript_df_metaonly_duration[c(audio_transcript_df_metaonly_duration$user_id == user_id & audio_transcript_df_metaonly_duration$audio_timestamp == audio_timestamp), "last_word_end_timestamp"]

# Read in the respective audio file from GCP folder, but only the part that contains speech!
audiofile <- readWave(paste0("../xxx/", i, ".wav"), to = last_word_end_timestamp, units = "seconds") # read in audio file

# save timmed file in new GCP folder
writeWave(paste0("../trimmed_files/", i, ".wav")) # save trimmed audio file

}

## FINISH