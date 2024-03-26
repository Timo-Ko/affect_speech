### FILTER INSTANCES BASED ON AFFECT DATA (USER LEVEL) ####

library(dplyr)

# load data 
affect_voice <- readRDS("data/study2/affect_voice_study2.rds")

## remove participants with less than 10 EMA instances 

# count how many es instances with ema ratings are available per participant
count_es_user <- affect_voice %>% 
  dplyr::group_by(user_id) %>% 
  dplyr::count(sort =T)

hist(count_es_user$n) #plot histogram

# find participants with less than 10 emas
length(which(count_es_user$n < 10)) 

# find participants with at least 10 emas
enoughes_user <- count_es_user[ count_es_user$n >= 10, "user_id"]

enoughes_user <- pull(enoughes_user) # format

affect_voice_cleaned <- affect_voice[ affect_voice$user_id %in% enoughes_user ,] #remove participants with less than 10 emas

# compute variance in ema responses per participant
var_es_user <- affect_voice_cleaned %>%
  dplyr::group_by(user_id) %>%
  dplyr::mutate(var_content = var(content, na.rm = T), var_sad = var(sad, na.rm = T), var_arousal = var(arousal, na.rm = T)) %>%
  dplyr::slice(1) #keep one row per user 

# find participants with zero variance in their ema responses across all their es (they were probably straight lining)
length(which(var_es_user$var_content == 0 & var_es_user$var_sad == 0 & var_es_user$var_arousal == 0)) # 1 user

# find participants with variance in their responses
variancees_user <- var_es_user[ var_es_user$var_content > 0  | var_es_user$var_sad > 0 | var_es_user$var_arousal > 0, "user_id"]

variancees_user <- pull(variancees_user) # format

affect_voice_cleaned <- affect_voice_cleaned[affect_voice_cleaned$user_id %in% variancees_user ,] #remove straight liners

### COMPUTE BASELINE AFFECT PER PARTICIPANT ####

# compute median contentedness, sadness and arousal per participant as baseline ("trait") score

median_affect_user <- affect_voice_cleaned %>% 
  dplyr::group_by(user_id) %>%
  dplyr::mutate(md_content = median(content, na.rm =T), md_sad = median(sad, na.rm =T), md_arousal = median(arousal, na.rm =T)) %>%
  dplyr::slice(1) #keep one row per user 

# append median affect column to df
affect_voice_cleaned <- merge(affect_voice_cleaned , median_affect_user[,c("user_id", "md_content", "md_sad", "md_arousal")], by = "user_id")

# compute deviation of current affect from baseline for each participant
affect_voice_cleaned$diff_content <- affect_voice_cleaned$content - affect_voice_cleaned$md_content
affect_voice_cleaned$diff_sad <- affect_voice_cleaned$sad - affect_voice_cleaned$md_sad
affect_voice_cleaned$diff_arousal <- affect_voice_cleaned$arousal - affect_voice_cleaned$md_arousal

# reorder columns 
affect_voice_cleaned <- affect_voice_cleaned  %>% 
  dplyr::select(c("user_id" , "content", "md_content", "diff_content", "sad", "md_sad", "diff_sad", "arousal", "md_arousal", "diff_arousal"),everything())

### FILTER INSTANCES BASED ON VOICE DATA (INSTANCE LEVEL) ####

# inclusion criteria:
# min 4 secs of file duration and min 15 words spoken (comparable to GER study) and must contain human voice

affect_voice_cleaned_final <- affect_voice_cleaned %>% 
  filter(File.duration.in.seconds >= 4) %>% # min 4 seconds of file length
  filter(Total.words >= 15) %>% # min 15 word spoken
  filter(voicingFinalUnclipped_sma_amean >= 0.5) %>% # min 50% chance that human voice had been recorded
  filter(VoicedSegmentsPerSec > 0) %>% # more than 0 voiced segments per second
  filter(MeanVoicedSegmentLengthSec > 0) # mean length of voice segments greater than zero

# save final df
saveRDS(affect_voice_cleaned_final, "data/study2/affect_voice_study2_cleaned.rds")

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