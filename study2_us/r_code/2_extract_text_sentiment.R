## load packages 

require(data.table)
require(stringr)

## extract from raw data

# load one file first to test this 

# iterate through user files 

# list all files containing language data of users (one df per user, one row per typing session)
data_files <- list.files("data/FA18_All_participants.Audio_Text_only", pattern="*.RDS") 

# create empty df

col_transcript <- c("user_id", 
                    "audio_timestamp", #"first_word_start_timestamp", 
                    "last_word_end_timestamp",
                    "transcript", "word_count", "sent_magnitude", "sent_score")

audio_transcript_df <- data.frame(matrix(NA, 
                                            nrow = 0, 
                                              ncol = length(col_transcript )))

colnames(audio_transcript_df) <- col_transcript # set colnames

# load data from respective columna and paste into new df

for(i in 1:length(data_files)) { 
  
  print(paste("Starting with transcripts from participant",data_files[i]))
  
  user_data <- readRDS(paste0("data/FA18_All_participants.Audio_Text_only/", data_files[i])) # read in data from that respective user
  # these are listed dfs (one list element = one audio record, each audio record is nested into sentences)
  
  user_id <- substr(data_files[i], 1, nchar(data_files[i])-4) # get user_id
  
  # iterate through the list of features and extract data
  
  for(k in 1:length(user_data )) { 
  
  # handle cases when no speech had been detected
    
    if (user_data[[k]]$speech.to.text[1] == "No speech was detected in this file"){ # skip if no speech was detected
      print(paste("No speech detected in file number", k , "from participant", user_id ))
      next
    }
  
  # get the total number of detected sentences is each single record  
  sentences <- length(user_data[[k]][[1]])
    
  # fill data in df
  
  audio_transcript_df[nrow(audio_transcript_df)+1,] <- NA # add empty row to meta_features_df that is to be filled
  numberofrows <- nrow(audio_transcript_df) #number of rows present in the df
  
  # fill df
  audio_transcript_df[numberofrows,"user_id"] = user_id 
  audio_transcript_df[numberofrows,"audio_timestamp"] = substr(names(user_data)[k], 1, nchar(names(user_data)[k])-4) #get the timestamp when that audio had been recorded
  #audio_transcript_df[numberofrows,"first_word_start_timestamp"] = first(user_data[[k]]$speech.to.text[[2]]$start_time) #the numbers here look weird!
  audio_transcript_df[numberofrows,"last_word_end_timestamp"] = last(user_data[[k]]$speech.to.text[[sentences]]$end_time)
  audio_transcript_df[numberofrows,"transcript"] = user_data[[k]]$google_NLP_annotateText$text
  audio_transcript_df[numberofrows,"word_count"] = str_count(user_data[[k]]$google_NLP_annotateText$text, '\\w+')
  audio_transcript_df[numberofrows,"sent_magnitude"] = user_data[[k]]$google_NLP_annotateText$documentSentiment$magnitude
  audio_transcript_df[numberofrows,"sent_score"] = user_data[[k]]$google_NLP_annotateText$documentSentiment$score
  
  }
}

# only keep records with min 10 spoken words
audio_transcript_df_filtered <- audio_transcript_df[audio_transcript_df$word_count >= 10,]

# save data

saveRDS(audio_transcript_df_filtered, "data/audio_transcript_df.RData")
saveRDS(audio_transcript_df_filtered[,c(-4)], "data/audio_transcript_df_metaonly.RData") # save file that does not contain the raw transcripts (potentially sensitive)

### desriptives 

dim(audio_transcript_df)

summary(audio_transcript_df$word_count)

hist(audio_transcript_df$word_count)
hist(audio_transcript_df$sent_magnitude)
hist(audio_transcript_df$sent_score)

# idea word embeddings on text? this is only an add-on!!




## FINISH



### OLD CODE







# open transcript file from one user

transcript <- readRDS("audio_transcripts/121f7uao.RDS")

ab <- transcript[[1]]$google_NLP_annotateText$documentSentiment

test


#str(transcript)

# get the sentiment scores
#onetrans <- transcript[2]
#str(onetrans)

# this transcript has sentiment
#sentiment <- transcript[2]$`2018-10-11_00-04-26.mp4`$google_NLP_annotateText$documentSentiment
#test <- transcript[2][1]$google_NLP_annotateText$documentSentiment

str(test)

# this one does not have sentiment
#transcript[1]$`2018-10-11_00-04-26.mp4`$google_NLP_annotateText$documentSentiment


# get the sentiment scores for the whole document
transcript[1]$google_NLP_annotateText$documentSentiment

# the files alread containt a Google API sentiment score
# magnitude - that indicates how emotional the text is (the higher, the more emotional)
# score - that is the actual sentiment score between -1 and 1