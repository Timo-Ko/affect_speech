## load required packages

require(tuneR)
require(googleCloudStorageR)

# load data frame containing meta data on audio records (this does not contain the raw trancripts for privacy reasons)
audio_transcript_df_metaonly <- readRDS("data/audio_transcript_df_metaonly.RData")

# create new column for raw audio file duration
audio_transcript_df_metaonly$raw_duration <- NA

# set bucket path
bucket_path = 'gs://sable_data_2018' # this needs to be adapted 

# get names of audio files in the GCP folder
listoffiles <- gcs_list_objects(bucket = "xxx") # this needs to be adapted 
audionames <- listoffiles$name

#### GET DURATION OF AUDIO RECORDS ####

for (i in audionames) {
  
audio <- readWave(paste0(bucket_path,i,".wav"), header=TRUE) # read meta data of the audio file

user_id <- substr(i, 1, 15) # get the user id, this needs to be adapted depending on what the file names looks like
audio_timestamp <- substr(i, 1, 15) # get the audio timestamp,  this needs to be adapted depending on what the file names looks like

duration <- round(audio$samples / audio$sample.rate, 2) # get duration

# insert duration of that specific record into the data frame
audio_transcript_df_metaonly[c(audio_transcript_df_metaonly$user_id == user_id & audio_transcript_df_metaonly$audio_timestamp == audio_timestamp), "raw_duration"] <- duration

}

# compute the difference between the last spoken word and the length of the audio file
audio_transcript_df_metaonly$nospeech_duration <- audio_transcript_df_metaonly$raw_duration - audio_transcript_df_metaonly$last_word_end_timestamp

# save data 
saveRDS(audio_transcript_df_metaonly, "data/audio_transcript_df_metaonly_duration.RData")

## FINISH