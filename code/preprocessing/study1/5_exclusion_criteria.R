### FILTED DATA BASED ON AFFECT DATA ####

## read in data

affect_df_raw <- readRDS("data/study1/affect_df_raw.RData")

## remove participants with less than 10 experience sampling instances

# count how many es instances with valence and arousal ratings are available per participant
count_es_user <- affect_df_raw %>% 
  dplyr::group_by(user_id) %>% 
  dplyr::count(sort =T)

# find participants with less than 10 es instances
length(which(count_es_user$n < 10))

# find participants with at least 5 es days
enoughes_user <- count_es_user[ count_es_user$n >=10, "user_id"]

enoughes_user <- pull(enoughes_user) # format

affect_df <- affect_df_raw[ affect_df_raw$user_id %in% enoughes_user ,] #remove participants with less than 10 es instances

# compute variance in valence and arousal responses per participant
var_es_user <- affect_df %>%
  dplyr::group_by(user_id) %>%
  dplyr::mutate(var_valence = var(valence, na.rm = T), var_arousal = var(arousal, na.rm = T)) %>%
  dplyr::slice(1) #keep one row per user 

# find participants with zero variance in their valence AND arousal responses across all their es (they were probably straightlining)
length(which(var_es_user$var_valence == 0 & var_es_user$var_arousal == 0)) # 11 participants fall into the straightliner category

# find participants with variance in their responses
variancees_user <- var_es_user[ var_es_user$var_valence != 0  | var_es_user$var_arousal != 0, "user_id"]

variancees_user <- pull(variancees_user) # format

affect_df <- affect_df[ affect_df$user_id %in% variancees_user ,] #remove straightliners

## compute affect baseline per participant

# compute median valence and arousal per participant as baseline ("trait") score

median_affect_user <- affect_df %>% 
  dplyr::group_by(user_id) %>%
  dplyr::mutate(md_valence = median(valence, na.rm =T), md_arousal = median(arousal, na.rm =T)) %>%
  dplyr::slice(1) #keep one row per user 

# append median affect column to affect df
affect_df <- merge(affect_df, median_affect_user[,c("user_id", "md_valence", "md_arousal")], by = "user_id")

# compute deviation of current affect from baseline for each participant
affect_df$diff_valence <- affect_df$valence - affect_df$md_valence
affect_df$diff_arousal <- affect_df$arousal - affect_df$md_arousal

# save final df
saveRDS(affect_df, "data/study1/affect_df.RData")

### DESCRIPTIVES OF FINAL AFFECT DATA ####

# distribution of raw valence and arousal ratings across es instances
hist(affect_df$valence)
hist(affect_df$arousal)

table(affect_df$valence)
table(affect_df$arousal)

# distribution of valence and arousal differences from participants' baseline across es instances
hist(affect_df$diff_valence)
hist(affect_df$diff_arousal)

table(affect_df$diff_valence)
table(affect_df$diff_arousal)


### CLEAN DATA BASED ON VOICE INDICATORS ####

# load data
egemaps_feature_df_all <- readRDS("data/egemaps_features_all.RData")
compare_feature_df_all <- readRDS("data/compare_features_all.RData")

## filter out instances where a voice record has been made

egemaps_feature_df_rec  <- egemaps_feature_df_all %>% 
  filter(!is.na(frameTime))

compare_feature_df_rec  <- compare_feature_df_all %>% 
  filter(!is.na(frameTime))

## find cases where participants did not record voice in their audio samples

# there is a feature in the compare feature set that indicates the probability that human voice was recorded at all (values between 0 and 1)
hist(compare_feature_df_rec$voicingFinalUnclipped_sma_amean, breaks = 1000) #plot distribution (two clear peaks)
summary(compare_feature_df_rec$voicingFinalUnclipped_sma_amean)

# find all instances with less than 50% percent probability that voice was recorded

compare_feature_df_novoice <- compare_feature_df_rec %>% 
  filter(voicingFinalUnclipped_sma_amean < 0.5)

compare_feature_df_voice  <- compare_feature_df_rec %>% 
  filter(voicingFinalUnclipped_sma_amean>= 0.5)


# there is a feature set in the egemaps feature set that indicates how much voice was recorded (voiced segments per second)
hist(egemaps_feature_df_rec$VoicedSegmentsPerSec, breaks = 1000) #plot (normal distribution)
summary(egemaps_feature_df_rec$VoicedSegmentsPerSec)

# remove cases where this is zero! TO DO with raw data!
#VoicedSegmentsPerSec - Anzahl der Sprachsegmente pro Sekunde. Wenn Null, dürfte keine Sprache vorhanden sein.
#MeanVoicedSegmentLengthSec - Mittlere Länge der Sprachsegmente in Sekunden. Wenn Null, dürfte keine Sprache vorhanden sein.
hist(egemaps_feature_df_rec$MeanVoicedSegmentLengthSec, breaks = 1000) #plot (normal distribution)

# investigate descriptives of instances without voice

length(unique(compare_feature_df_novoice$e_s_questionnaire_id)) # the no voice records come from 1908 ES instances
length(unique(compare_feature_df_novoice$user_id)) # the no voice records come from 514 participants

# count no voice records per participant
novoice_user <- compare_feature_df_novoice %>% 
  group_by(user_id) %>% 
  count(sort =T, name = "n_novoice")

# count voice records per participant
voice_user <- compare_feature_df_voice %>% 
  group_by(user_id) %>% 
  count(sort =T, name = "n_voice")

# merge and compute share of voice containing records in all records
voiceshare_user = merge(voice_user, novoice_user)

voiceshare_user$n_total = voiceshare_user$n_voice + voiceshare_user$n_novoice
voiceshare_user$voice_share = voiceshare_user$n_voice / voiceshare_user$n_total

hist(voiceshare_user$voice_share, breaks = 10)

table(no_voice$condition) # no meaningful differences across sentence conditions

no_voice_id <- compare_feature_df_novoice$id # get ids of no voice records

# removes no voice instances from egemaps and compare feature sets

`%!in%` <- Negate(`%in%`)

egemaps_feature_df_cleaned <- egemaps_feature_df_rec %>% 
  filter(id %!in% no_voice_id )

compare_feature_df_cleaned <- compare_feature_df_rec %>% 
  filter(id %!in% no_voice_id )

# save cleaned dfs
saveRDS(egemaps_feature_df_cleaned, "study1_ger/data/egemaps_features.RData")
saveRDS(compare_feature_df_cleaned, "study1_ger/data/compare_features.RData")

## FINISH