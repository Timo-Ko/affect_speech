### FILTED DATA BASED ON AFFECT DATA ####

## read in data

affect_voice <- readRDS("data/study1/affect_voice_study1.rds")

## remove participants with less than 10 experience sampling instances

# count how many es instances with valence and arousal ratings are available per participant
count_es_user <- affect_voice %>% 
  dplyr::group_by(user_id) %>% 
  dplyr::count(sort =T)

# find participants with less than 10 es instances
length(which(count_es_user$n < 10))

# find participants with at least 5 es days
enoughes_user <- count_es_user[ count_es_user$n >=10, "user_id"]

enoughes_user <- pull(enoughes_user) # format

affect_voice <- affect_voice[ affect_voice$user_id %in% enoughes_user ,] #remove participants with less than 10 es instances

# compute variance in valence and arousal responses per participant
var_es_user <- affect_voice %>%
  dplyr::group_by(user_id) %>%
  dplyr::mutate(var_valence = var(valence, na.rm = T), var_arousal = var(arousal, na.rm = T)) %>%
  dplyr::slice(1) #keep one row per user 

# find participants with zero variance in their valence AND arousal responses across all their es (they were probably straightlining)
length(which(var_es_user$var_valence == 0 & var_es_user$var_arousal == 0)) # 4 participants fall into the straightliner category

# find participants with variance in their responses
variancees_user <- var_es_user[ var_es_user$var_valence != 0  | var_es_user$var_arousal != 0, "user_id"]

variancees_user <- pull(variancees_user) # format

affect_voice <- affect_voice[ affect_voice$user_id %in% variancees_user ,] #remove straightliners

## supplementary analysis: compute affect baseline per participant

# compute median valence and arousal per participant as baseline ("trait") score

median_affect_user <- affect_voice  %>% 
  dplyr::group_by(user_id) %>%
  dplyr::mutate(md_valence = median(valence, na.rm =T), md_arousal = median(arousal, na.rm =T)) %>%
  dplyr::slice(1) #keep one row per user 

# append median affect column to affect df
affect_voice <- merge(affect_voice, median_affect_user[,c("user_id", "md_valence", "md_arousal")], by = "user_id")

# compute deviation of current affect from baseline for each participant
affect_voice$diff_valence <- as.numeric(affect_voice$valence - affect_voice$md_valence)
affect_voice$diff_arousal <- as.numeric(affect_voice$arousal - affect_voice$md_arousal)

# arrange cols
affect_voice <- affect_voice  %>% 
  dplyr::select(c("e_s_questionnaire_id", "questionnaireStartedTimestamp", "id", "user_id" , "Demo_A1", "Demo_GE1", "condition", "valence", "md_valence", "diff_valence", "arousal", "md_arousal", "diff_arousal"), everything())

### CLEAN DATA BASED ON VOICE INDICATORS ####

## find cases where participants did not record voice in their audio samples

# there is a feature in the compare feature set that indicates the probability that human voice was recorded at all (values between 0 and 1)
hist(affect_voice$voicingFinalUnclipped_sma_amean, breaks = 1000) #plot distribution 
hist(affect_voice$VoicedSegmentsPerSec, breaks = 1000) #plot (normal distribution)
hist(affect_voice$MeanVoicedSegmentLengthSec, breaks = 1000) #plot (normal distribution)

# remove all instances based on those features

affect_voice_cleaned  <- affect_voice %>% 
  dplyr::filter(voicingFinalUnclipped_sma_amean >= 0.5) %>%
  dplyr::filter(VoicedSegmentsPerSec >0) %>%
  dplyr::filter(MeanVoicedSegmentLengthSec >0)

# # investigate descriptives of instances without voice
# 
# length(unique(compare_feature_df_novoice$e_s_questionnaire_id)) # the no voice records come from 1908 ES instances
# length(unique(compare_feature_df_novoice$user_id)) # the no voice records come from 514 participants
# 
# # count no voice records per participant
# novoice_user <- compare_feature_df_novoice %>% 
#   group_by(user_id) %>% 
#   count(sort =T, name = "n_novoice")
# 
# # count voice records per participant
# voice_user <- compare_feature_df_voice %>% 
#   group_by(user_id) %>% 
#   count(sort =T, name = "n_voice")
# 
# # merge and compute share of voice containing records in all records
# voiceshare_user = merge(voice_user, novoice_user)
# 
# voiceshare_user$n_total = voiceshare_user$n_voice + voiceshare_user$n_novoice
# voiceshare_user$voice_share = voiceshare_user$n_voice / voiceshare_user$n_total
# 
# hist(voiceshare_user$voice_share, breaks = 10)
# 
# table(no_voice$condition) # no meaningful differences across sentence conditions
# 
# no_voice_id <- compare_feature_df_novoice$id # get ids of no voice records
# 
# # removes no voice instances from voice and compare feature sets
# 
# `%!in%` <- Negate(`%in%`)
# 
# voice_feature_df_cleaned <- voice_feature_df_rec %>% 
#   filter(id %!in% no_voice_id )

# save cleaned dfs
saveRDS(affect_voice_cleaned, "study1/data/affect_voice_cleaned.rds")

### DESCRIPTIVES OF FINAL DATA ####

# number of participants
length(unique(affect_voice_cleaned$user_id))

# number of es
length(unique(affect_voice_cleaned$e_s_questionnaire_id))

# number of audio logs
dim(affect_voice_cleaned)

# distribution of raw valence and arousal ratings across es instances
hist(affect_voice_cleaned$valence)
hist(affect_voice_cleaned$arousal)

table(affect_voice_cleaned$valence)
table(affect_voice_cleaned$arousal)

# distribution of valence and arousal differences from participants' baseline across es instances
hist(affect_voice_cleaned$diff_valence)
hist(affect_voice_cleaned$diff_arousal)

table(affect_voice_cleaned$diff_valence)
table(affect_voice_cleaned$diff_arousal)

## FINISH