### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "data.table", "stringr", "ggplot2", "lubridate")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# load data

affect_es <- readRDS("study1_ger/data/affect_es.RData")

### CONVERT AFFECT ANSWERS TO NUMERICS ####

# create new empty columns for numeric responses
affect_es$valence <- NA
affect_es$arousal <- NA

# convert affect responses to numerics
affect_es = affect_es  %>% 
  dplyr::mutate(answer_text = gsub("[^[:alnum:]]", "", answer_text)) %>% 
  dplyr::mutate(valence = ifelse(answer_text == "sehrangenehm", 6, valence),
         valence = ifelse(answer_text == "angenehm", 5, valence),
         valence = ifelse(answer_text == "eherangenehm", 4, valence), 
         valence = ifelse(answer_text == "eherunangenehm", 3, valence), 
         valence = ifelse(answer_text == "unangenehm", 2, valence), 
         valence = ifelse(answer_text == "sehrunangenehm", 1, valence)) %>% 
  dplyr::mutate(arousal = ifelse(answer_text == "sehraktiviert", 6, arousal),
         arousal = ifelse(answer_text == "aktiviert", 5, arousal),
         arousal = ifelse(answer_text == "eheraktiviert", 4, arousal), 
         arousal = ifelse(answer_text == "eherinaktiv", 3, arousal), 
         arousal = ifelse(answer_text == "inaktiv", 2, arousal), 
         arousal = ifelse(answer_text == "sehrinaktiv", 1, arousal)) 

affect_df_raw <- as.data.frame(matrix(0, ncol = 0, nrow = length(unique(affect_es$e_s_questionnaire_id)) )) # create new empty df
affect_df_raw$e_s_questionnaire_id <-  unique(affect_es$e_s_questionnaire_id) # add column with questionnaire id   

affect_df_raw <- merge(affect_df_raw, na.omit(affect_es[, c("e_s_questionnaire_id", "valence")]), by="e_s_questionnaire_id", all.x = T) # add valence column
affect_df_raw <- merge(affect_df_raw, na.omit(affect_es[, c("e_s_questionnaire_id", "arousal")]), by="e_s_questionnaire_id", all.x = T) # add arousal column

# beware: in some es instances only the valence or the arousal item has been answered!

## match w corresponding user ids

affect_df_raw_merged <- merge(affect_df_raw, affect_es[, c("e_s_questionnaire_id", "user_id", "questionnaireStartedTimestamp")], by ="e_s_questionnaire_id", all.x = T)

affect_df_raw_merged <- affect_df_raw_merged[!duplicated(affect_df_raw_merged$e_s_questionnaire_id),] # remove duplicates created by merge
                                        
# save 
saveRDS(affect_df_raw_merged, "study1_ger/data/affect_df_raw.RData")

### CLEAN AFFECT DATA ####

# read data
affect_df_raw <- readRDS("study1_ger/data/affect_df_raw.RData")

## remove participants with too few affect experience sampling instances (at least 5 es days )

# count how many es instances with valence and arousal ratings are available per participant
count_es_user <- affect_df_raw %>% 
  dplyr::group_by(user_id) %>% 
  dplyr::count(sort =T)

# count how many es days are available per participant
count_es_perday <- affect_df_raw %>% 
  dplyr::group_by(user_id) %>% 
  dplyr::count(as.Date(questionnaireStartedTimestamp), sort =T)

count_es_days <- count_es_perday %>% 
  dplyr::group_by(user_id) %>% 
  dplyr::count(sort =T)

hist(count_es_days$n, breaks = 28) #plot histogram

# find participants with less than 5 days of es
length(which(count_es_days$n < 5)) # 62 participants

# find participants with at least 5 es days
enoughes_user <- count_es_days[ count_es_days$n >=5, "user_id"]

enoughes_user <- pull(enoughes_user) # format

affect_df_cleaned <- affect_df_raw[ affect_df_raw$user_id %in% enoughes_user ,] #remove participants with less than 5 es days

# compute variance in valence and arousal responses per participant
var_es_user <- affect_df_cleaned %>%
  dplyr::group_by(user_id) %>%
  dplyr::mutate(var_valence = var(valence, na.rm = T), var_arousal = var(arousal, na.rm = T)) %>%
  dplyr::slice(1) #keep one row per user 

# find participants with zero variance in their valence AND arousal responses across all their es (they were probably straightlining)
length(which(var_es_user$var_valence == 0 & var_es_user$var_arousal == 0)) # 12 participants fall into the straightliner category

# find participants with variance in their responses
variancees_user <- var_es_user[ var_es_user$var_valence != 0  | var_es_user$var_arousal != 0, "user_id"]

variancees_user <- pull(variancees_user) # format

affect_df_cleaned <- affect_df_cleaned[ affect_df_cleaned$user_id %in% variancees_user ,] #remove straightliners

# save 
saveRDS(affect_df_cleaned, "study1_ger/data/affect_df_cleaned.RData")

### COMPUTE BASELINE AFFECT PER PARTICIPANT ####

# read data
affect_df_cleaned <- readRDS("study1_ger/data/affect_df_cleaned.RData")

# compute median valence and arousal per participant as baseline ("trait") score

median_affect_user <- affect_df_cleaned %>% 
  dplyr::group_by(user_id) %>%
  dplyr::mutate(md_valence = median(valence, na.rm =T), md_arousal = median(arousal, na.rm =T)) %>%
  dplyr::slice(1) #keep one row per user 

# append median affect column to affect df
affect_df <- merge(affect_df_cleaned, median_affect_user[,c("user_id", "md_valence", "md_arousal")], by = "user_id")

# compute deviation of current affect from baseline for each participant
affect_df$diff_valence <- affect_df$valence - affect_df$md_valence
affect_df$diff_arousal <- affect_df$arousal - affect_df$md_arousal

# save final df
saveRDS(affect_df, "study1_ger/data/affect_df.RData")

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

# finish