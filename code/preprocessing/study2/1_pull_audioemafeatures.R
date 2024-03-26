### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "data.table", "stringr", "ggplot2", "lubridate", "stringr")
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

## download files from gcp bucket, only do this once!

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

# save data before data cleaning
saveRDS(affect_voice, "data/study2/affect_voice_study2.rds")
