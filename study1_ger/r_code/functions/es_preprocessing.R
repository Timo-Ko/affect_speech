#' Preprocessing function
#'
#' @family preprocessing function
#' @param data Activity table
#' @param dbCon_ps Database connection to the phonestudy database, for access to ps_activity
#' @return activity_df Preprocessed activity table
#' @return session_df Session table
#' @import lubridate
#' @import dplyr
#' @examples 
#' preprocessing(ps_activity, ps_communication, ps_participant)
#' @export

# subset ps_activity only to es days?

preprocessing_general_es <- function(es_df, dbCon_ps = NULL, studymanagement){
  
  user_uuid = es_df$user_id[1] #the function is applied to each user separately to prevent memory crash
  
  # Parse timestamps
  if (es_df$timestamp_type_start[[1]] > 1000000000000){
    es_df$timestamp_type_start = es_df$timestamp_type_start/1000
  }
  es_df$timestamp_type_start <- as.integer(es_df$timestamp_type_start)
  es_df$timestamp_type_start <- lubridate::as_datetime(es_df$timestamp_type_start)
  
  if (es_df$timestamp_type_end[[1]] > 1000000000000){
    es_df$timestamp_type_end = es_df$timestamp_type_end/1000
  }
  es_df$timestamp_type_end <- as.integer(es_df$timestamp_type_end)
  es_df$timestamp_type_end <- lubridate::as_datetime(es_df$timestamp_type_end)
  
  # Correct timestamps
  if (!is.null(dbCon_ps)){
    data_activity = dplyr::tbl(dbCon_ps, "ps_activity") %>% dplyr::filter(user_id == user_uuid) %>% dplyr::select(timestamp,timezoneOffset) %>% data.frame()
    es_df <- sensing_prepare_format_es(es_df, data_activity, studymanagement)
  }
  else {
    print("dbCon_ps is NULL - You are using es data without correcting timestamps!")
  }
  
  
  # Get sessions
  #  activity_session <- continuousSessions(activity_df)
  # activity_df <- activity_session[[1]]
  # session_df <- activity_session[[2]]
  
  # Correct session length if needed
  # session_df <- callsCorrection(activity_df, session_df, comm_df)
  
  # Study was made entirely in 2020
  #  es_df <- es_df %>% filter(year(timestamp.corrected) == 2020)
  #session_df <- session_df %>% filter(year(first_timestamp) == 2020) TODO do I need sessions?
  
  return(es_df)
}


# this function is used inside the above "preprocessing_general_es" function

sensing_prepare_format_es = function(data_es, data_activity, studymanagement){
  
  if (nrow(data_activity) == 0){
    print("Could not find sensing data to get timezoneOffset from. Will assume german time")
    data_es$timestamp_type_start = with_tz(data_es$timestamp_type_start, tzone = "Europe/Berlin")
    data_es$timestamp_type_end = with_tz(data_es$timestamp_type_end, tzone = "Europe/Berlin")
    return(data_es)
  }
  
  
  data_activity = data_activity %>% rename(timestamp_mergecol = timestamp)
  
  # Step 1: impute missing values with k-nearest neigbours in timezoneOffset
  # 1.1: Merge ps_activity + the es data, because only ps_activity has timezoneOffset
  data_es_with_id = data_es %>% bind_cols(tibble(rowid = 1:count(data_es)$n))  
  data_merged = data_es_with_id %>% 
    dplyr::rename(timestamp_mergecol = timestamp_type_start) %>% 
    dplyr::select(timestamp_mergecol,rowid) %>% 
    mutate(timezoneOffset = NA) %>% 
    mutate(table="es") %>% 
    bind_rows(data_activity %>% 
                dplyr::select(timestamp_mergecol,timezoneOffset) %>% 
                mutate(table="activity") %>%
                mutate(rowid=-1)) %>% 
    arrange(timestamp_mergecol)
  # 1.2: Impute the empty values -> es data becomes imputed by the ps_activity data
  data_merged$timezoneOffset[which(data_merged$timezoneOffset == 0)] = NA
  data_merged$timezoneOffset = impute.knn(data_merged$timezoneOffset, k = 10)
  
  # 1.3: Re-separate both, and apply the timezoneOffsets to the timestamps in the es data
  data = data_es_with_id %>% left_join(data_merged %>% filter(table=="es") %>% dplyr::select(-table), by=c("rowid"="rowid")) %>% dplyr::select(-rowid)
  
  # Step 2: consider timezoneOffset in timestamp and get correct format with miliseconds! 
  options(digits.secs=6)
  data$timestamp_type_start = lubridate::ymd_hms(data$timestamp_type_start) + lubridate::hours(data$timezoneOffset/(60*60*1000))
  data$timestamp_type_end = lubridate::ymd_hms(data$timestamp_type_end) + lubridate::hours(data$timezoneOffset/(60*60*1000))
  if ("timestamp" %in% colnames(data)){
    data$timestamp = lubridate::ymd_hms(data$timestamp) + lubridate::hours(data$timezoneOffset/(60*60*1000))
  }
  if ("date" %in% colnames(data)){
    data$date = lubridate::ymd_hms(data$date) + lubridate::hours(data$timezoneOffset/(60*60*1000))
  }
  
  # Step 3: order rows according to logging timestamps
  data = dplyr::arrange(data, timestamp_type_start) %>% select(-timestamp_mergecol)
  
  return(data)
}
