### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "data.table", "stringr", "ggplot2", "lubridate")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

### DOWNLOAD FILES FROM BUCKET ####

## explore bucket with data

# # this is the gs util path of the bucket
# bucket_path = "'gs://sable_data_2018/Opensmile Audio Features'"
# 
# # create a list of all files in the bucket
# list_cmd = paste("gsutil ls -r ", bucket_path, sep = "")
# list_of_files = system(list_cmd, intern = TRUE)
# list_of_files

## download files, only do this once!

# system("gsutil cp 'gs://sable_data_2018/Opensmile Audio Features/All_EMA_opensmile_features_FA18_Joined.csv' ./data") #open smile (compare2016) and EMA data
# system("gsutil cp 'gs://sable_data_2018/Opensmile Audio Features/All_EMA_opensmile_eGeMaps0v2_features_FA18_Joined.csv' ./data") #open smile (egemaps) and EMA data
# system("gsutil cp 'gs://sable_data_2018/Opensmile Audio Features/Age_Gender_Fa18.csv' ./data") # participant demographics

# also download the auxiliary r scripts that had been used to process the data

#system("gsutil cp 'gs://sable_data_2018/Opensmile Audio Features/2 Find Within person AudioEMA pairs.R' ./r_code")
#system("gsutil cp 'gs://sable_data_2018/Opensmile Audio Features/Opensmile.featureextraction_with_ActiveVoiceHeuristic (uptodate on July 18th 2022).R' ./r_code")

### READ IN AND RESTRUCTURE DFS ####

## read in data (this takes a while!)
affect_egemaps_raw <- read.csv("data/study2/All_EMA_opensmile_eGeMaps0v2_features_FA18_Joined.csv")
affect_compare_raw <- read.csv("data/study2/All_EMA_opensmile_features_FA18_Joined.csv")
participant_demographics <- read.csv("data/study2/Age_Gender_Fa18.csv")

## egemaps features

affect_egemaps <- affect_egemaps_raw[,c(which(colnames(affect_egemaps_raw)=="names.x"),
                                        which(colnames(affect_egemaps_raw)=="UTC.time.x"),
                                        which(colnames(affect_egemaps_raw)=="CONTENT_EMA_Numeric"),
                                        which(colnames(affect_egemaps_raw)=="SAD_EMA_Numeric"),
                                        which(colnames(affect_egemaps_raw)=="ENERGY_EMA_Numeric"),
                                        which(colnames(affect_egemaps_raw)=="Total.words"),
                                        which(colnames(affect_egemaps_raw)=="File.duration.in.seconds"),
                                        which(colnames(affect_egemaps_raw)=="Voice.only.duration.in.seconds"),
                                        which(colnames(affect_egemaps_raw)=="Text"),                                 
                                        which(colnames(affect_egemaps_raw)=="Sentiment.magnitude"),
                                        which(colnames(affect_egemaps_raw)=="Sentiment.score"),
                                        which(colnames(affect_egemaps_raw)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_egemaps_raw)=="equivalentSoundLevel_dBp"))]

# rename columns
affect_egemaps <- affect_egemaps %>%
  rename(user_id = names.x) %>%
  rename(timestamp = UTC.time.x) %>%
  rename(content = CONTENT_EMA_Numeric) %>% 
  rename(sad = SAD_EMA_Numeric) %>%         
  rename(arousal = ENERGY_EMA_Numeric)   

# trim user id column (remove ".RDS" appendix)
affect_egemaps$user_id <- str_sub(affect_egemaps$user_id, end=-5)

# append demographic data to affect data
affect_egemaps <- merge(affect_egemaps, participant_demographics, by.x = "user_id", by.y = "beiwe_id", all.x = T)

# reorder columns
affect_egemaps <- affect_egemaps %>% relocate("user_id", "Age", "Gender", "timestamp", everything())

# compare2016 features
compare_features <- affect_compare_raw[,c(which(colnames(affect_compare_raw)=="names.x"),
                                        which(colnames(affect_compare_raw)=="UTC.time.x"),
                                        which(colnames(affect_compare_raw)=="audspec_lengthL1norm_sma_range"):which(colnames(affect_compare_raw)=="mfcc_sma_de.14._stddevFallingSlope"))]

# rename columns
compare_features <- compare_features %>%
  rename(user_id = names.x) %>%
  rename(timestamp = UTC.time.x) 

# trim user id column (remove ".RDS" appendix)
compare_features$user_id <- str_sub(compare_features$user_id, end=-5)

# append compare features
affect_voice <- merge(affect_egemaps, compare_features, by = c("user_id", "timestamp"))

### FILTER INSTANCES BASED ON VOICE DATA ####

# inclusion criteria:
# min 4 secs of file duration and min 15 words spoken (comparable to GER study) and must contain human voice

affect_voice_enoughvoice <- affect_voice %>% 
  filter(File.duration.in.seconds >= 4) %>% # min 4 seconds of file length
  filter(Total.words >= 15) %>% # min 15 word spoken
  filter(voicingFinalUnclipped_sma_amean >= 0.5) %>% # min 50% chance that human voice had been recorded
  filter(VoicedSegmentsPerSec > 0) %>% # more than 0 voiced segments per second
  filter(MeanVoicedSegmentLengthSec > 0) # mean length of voice segments greater than zero

### FILTER INSTANCES BASED ON AFFECT DATA ####

## remove participants with less than 10 EMA instances 

# count how many es instances with ema ratings are available per participant
count_es_user <- affect_voice_enoughvoice %>% 
  dplyr::group_by(user_id) %>% 
  dplyr::count(sort =T)

hist(count_es_user$n) #plot histogram

# find participants with less than 10 emas
length(which(count_es_user$n < 10)) 

# find participants with at least 10 emas
enoughes_user <- count_es_user[ count_es_user$n >= 10, "user_id"]

enoughes_user <- pull(enoughes_user) # format

affect_voice_filtered <- affect_voice_enoughvoice[ affect_voice_enoughvoice$user_id %in% enoughes_user ,] #remove participants with less than 5 es days

# compute variance in ema responses per participant
var_es_user <- affect_voice_filtered %>%
  dplyr::group_by(user_id) %>%
  dplyr::mutate(var_content = var(content, na.rm = T), var_sad = var(sad, na.rm = T), var_arousal = var(arousal, na.rm = T)) %>%
  dplyr::slice(1) #keep one row per user 

# find participants with zero variance in their ema responses across all their es (they were probably straight lining)
length(which(var_es_user$var_content == 0 & var_es_user$var_sad == 0 & var_es_user$var_arousal == 0)) # 1 user

# find participants with variance in their responses
variancees_user <- var_es_user[ var_es_user$var_content > 0  | var_es_user$var_sad > 0 | var_es_user$var_arousal > 0, "user_id"]

variancees_user <- pull(variancees_user) # format

affect_voice_filtered <- affect_voice_filtered[affect_voice_filtered$user_id %in% variancees_user ,] #remove straight liners

### COMPUTE BASELINE AFFECT PER PARTICIPANT ####

# compute median contentedness, sadness and arousal per participant as baseline ("trait") score

median_affect_user <- affect_voice_filtered %>% 
  dplyr::group_by(user_id) %>%
  dplyr::mutate(md_content = median(content, na.rm =T), md_sad = median(sad, na.rm =T), md_arousal = median(arousal, na.rm =T)) %>%
  dplyr::slice(1) #keep one row per user 

# append median affect column to df
affect_voice <- merge(affect_voice_filtered , median_affect_user[,c("user_id", "md_content", "md_sad", "md_arousal")], by = "user_id")

# compute deviation of current affect from baseline for each participant
affect_voice$diff_content <- affect_voice$content - affect_voice$md_content
affect_voice$diff_sad <- affect_voice$sad - affect_voice$md_sad
affect_voice$diff_arousal <- affect_voice$arousal - affect_voice$md_arousal

# reorder columns 
affect_voice <- affect_voice  %>% 
  dplyr::select(c("user_id" , "content", "md_content", "diff_content", "sad", "md_sad", "diff_sad", "arousal", "md_arousal", "diff_arousal"),everything())

# save final df
saveRDS(affect_voice, "data/study2/affect_voice.rds")

### DESCRIPTIVES OF FINAL AFFECT DATA ####

# distribution of raw content and sad ratings across es instances
hist(affect_voice$content)
hist(affect_voice$sad)
hist(affect_voice$arousal)

table(affect_voice$content)
table(affect_voice$sad)
table(affect_voice$arousal)

# distribution of differences from participants' baseline across es instances
hist(affect_voice$diff_content)
hist(affect_voice$diff_sad)
hist(affect_voice$diff_arousal)

table(affect_voice$diff_content)
table(affect_voice$diff_sad)
table(affect_voice$diff_arousal)

# demographics

affect_voice %>% group_by(user_id) %>% mutate (m_age = mean(Age, na.rm = T))


# finish