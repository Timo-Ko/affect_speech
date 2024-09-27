### FILTER INSTANCES BASED ON AFFECT DATA (USER LEVEL) ####

library(dplyr)

# load data 
affect_voice <- readRDS("data/study2/affect_voice_study2.rds")

## remove participants with less than 10 voice logs with ema data

# count how many voice logs are available per participant
count_voice_user <- affect_voice %>% 
  dplyr::group_by(user_id) %>% 
  dplyr::summarize(count = n()) %>% 
  dplyr::arrange(desc(count))

# count participants with less than 10 voice samples
length(which(count_voice_user$count < 10)) 

# find participants with at least 10 voice samples
affect_voice_filtered <- affect_voice %>%
  dplyr::semi_join(count_voice_user %>% dplyr::filter(count >= 10), by = "user_id")

# compute variance in ema responses per participant
var_es_user <- affect_voice_filtered %>%
  dplyr::group_by(user_id) %>%
  dplyr::select(user_id, content, sad, arousal) %>%
  dplyr::mutate(var_content = var(content, na.rm = T), var_sad = var(sad, na.rm = T), var_arousal = var(arousal, na.rm = T)) %>%
  dplyr::slice(1) #keep one row per user 

# find participants with zero variance in their ema responses across all their es (one participant, was probably straight lining)
length(which(var_es_user$var_content == 0 & var_es_user$var_sad == 0 & var_es_user$var_arousal == 0)) # 1 user

# remove straightliner
affect_voice_filtered <- affect_voice_filtered %>%
  group_by(user_id) %>%
  filter(var(arousal, na.rm = TRUE) != 0 | var(content, na.rm = TRUE) != 0 | var(sad, na.rm = TRUE) != 0) %>%
  ungroup()

### COMPUTE BASELINE AFFECT PER PARTICIPANT ####

# compute median contentedness, sadness and arousal per participant as baseline ("trait") score

median_affect_user <- affect_voice_filtered %>% 
  dplyr::group_by(user_id) %>%
  dplyr::mutate(md_content = median(content, na.rm =T), md_sad = median(sad, na.rm =T), md_arousal = median(arousal, na.rm =T)) %>%
  dplyr::slice(1) #keep one row per user 

# append median affect column to df
affect_voice_filtered <- merge(affect_voice_filtered , median_affect_user[,c("user_id", "md_content", "md_sad", "md_arousal")], by = "user_id")

# compute deviation of current affect from baseline for each participant
affect_voice_filtered$diff_content <- affect_voice_filtered$content - affect_voice_filtered$md_content
affect_voice_filtered$diff_sad <- affect_voice_filtered$sad - affect_voice_filtered$md_sad
affect_voice_filtered$diff_arousal <- affect_voice_filtered$arousal - affect_voice_filtered$md_arousal

# reorder columns 
affect_voice_filtered <- affect_voice_filtered  %>% 
  dplyr::select(c("user_id" , "content", "md_content", "diff_content", "sad", "md_sad", "diff_sad", "arousal", "md_arousal", "diff_arousal"),everything())

### FILTER INSTANCES BASED ON VOICE DATA (INSTANCE LEVEL) ####

# filter for length of voice sample 
# min 4 secs of file duration and min 15 words spoken (comparable to GER study) and must contain human voice
affect_voice_cleaned <- affect_voice_filtered %>% 
  filter(
    File.duration.in.seconds >= 4 &          # min 4 seconds of file length
    Total.words >= 15 )                     # min 15 words spoken

# investigate excluded samples
removed_cases <- anti_join(affect_voice_filtered, affect_voice_cleaned)

nrow(removed_cases) # number of removed cases 
length(unique(removed_cases$user_id)) # number of user_ids of removed cases

# filter for voice indicators

hist(affect_voice_cleaned$voicingFinalUnclipped_sma_amean, breaks = 1000) #plot distribution 
hist(affect_voice_cleaned$VoicedSegmentsPerSec, breaks = 1000) #plot (normal distribution)
hist(affect_voice_cleaned$MeanVoicedSegmentLengthSec, breaks = 1000) #plot (normal distribution)

# remove all instances based on those features
affect_voice_cleaned <- affect_voice_cleaned %>%
  dplyr::filter(
    voicingFinalUnclipped_sma_amean >= 0.5 |
      VoicedSegmentsPerSec > 0 |
      MeanVoicedSegmentLengthSec > 0
  )

# all samples contained human voice (pretty likely given that at least 15 words had been spoken)

# save final df
saveRDS(affect_voice_cleaned, "data/study2/affect_voice_study2_cleaned.rds")

### DESCRIPTIVES OF FINAL DATA ####

# number of participants total 
length(unique(affect_voice_cleaned$user_id))

# number of emas + voice samples 
nrow(affect_voice_cleaned)

# get some summary statistics
summary_stats <- affect_voice_cleaned %>%
  # Create a summary for each user
  group_by(user_id) %>%
  # Summarize the number of audio logs and file details for each user
  summarise(
    num_audio_logs = n(),
    avg_File.duration.in.seconds = mean(File.duration.in.seconds, na.rm = TRUE),
    sd_File.duration.in.seconds = sd(File.duration.in.seconds, na.rm = TRUE),
    avg_Total.words = mean(Total.words, na.rm = TRUE),
    sd_Total.words = sd(Total.words, na.rm = TRUE)
  ) %>%
  # Ungroup to perform overall summary calculations
  ungroup() %>%
  # Calculate the overall summaries, including means and standard deviations
  summarise(
    num_participants = n(),  # Total number of participants
    num_audio_logs_total = sum(num_audio_logs),  # Total number of audio logs
    avg_audio_logs_per_participant = mean(num_audio_logs),  # Average number of audio logs per participant
    sd_audio_logs_per_participant = sd(num_audio_logs),  # SD of audio logs per participant
    avg_File.duration.in.seconds = mean(avg_File.duration.in.seconds, na.rm = TRUE),  
    sd_File.duration.in.seconds = mean(sd_File.duration.in.seconds, na.rm = TRUE),
    avg_Total.words = mean(avg_Total.words, na.rm = TRUE),  
    sd_Total.words = mean(sd_Total.words, na.rm = TRUE)
  )


# distribution of raw content and sad ratings across es instances
hist(affect_voice_cleaned$content)
hist(affect_voice_cleaned$sad)
hist(affect_voice_cleaned$arousal)

table(affect_voice_cleaned$content)
table(affect_voice_cleaned$sad)
table(affect_voice_cleaned$arousal)

# distribution of differences from participants' baseline across es instances
hist(affect_voice_cleaned$diff_content)
hist(affect_voice_cleaned$diff_sad)
hist(affect_voice_cleaned$diff_arousal)

table(affect_voice_cleaned$diff_content)
table(affect_voice_cleaned$diff_sad)
table(affect_voice_cleaned$diff_arousal)

# finish