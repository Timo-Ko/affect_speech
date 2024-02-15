#' Assigns users that changed their smartphone during the study a new user id
#' => all (physical) users are then encompassed by that new user (ids >= 2000)
#' => the old user id is kept in a new column suffixed with _phys
#' 
#' ATTENTION
#' This approach can confus and interfer with many common preprocessing + feature extraction steps - I do not recommend to use it yet!
#' However it is fine to detect + remove smartphone changers, that works reliably
#' 
#' By merging users under a new user_id, former user-unique columns (e.g. client_event_id, client_db_id) are not unique anymore. That
#' results in undesired behavior in algorithms that rely on their uniqueness
#'
#' @family preprocessing function
#' @param df Some data frame
#' @param user_id_columnname The columnname of the user id in this df. For phonestudy data usually user_id, for researchime data usually user_uuid.
#' @return The dataframe df, with user ids updated for smartphone changers and a new column suffixed by _phys
#' @examples 
#'   apply_smartphonechanges(some_sensing_dataframe"user_uuid") %>%
#'   filter(user_uuid < 2000) # remove smartphone changers as long as the above function's approach is not fixed
#' @export
apply_smartphonechanges <- function(df, user_id_columnname){
  smartphone_changes = read.csv("data/Smartphonewechsel_20210219.csv", sep=",")
  
  logical_id_base = 2000 # merged users get assigned a new user id. Such Ids start from 2000
  physical_user_id_columnname = paste(user_id_columnname,"_phys",sep="")
  df = df %>% dplyr::mutate(!!physical_user_id_columnname := !!sym(user_id_columnname)) # keep "old" user id in physical column
  for(row_id in 1:nrow(smartphone_changes)){
    a_change <- smartphone_changes[row_id,]
    new_logical_id = logical_id_base + row_id
    user_ids = c(a_change["NewId"][[1]], a_change["p_0001_2"][[1]],a_change["p_0001_3"][[1]], a_change["p_0001_new"][[1]])
    
    physical_user_id_columnname = paste(user_id_columnname,"_phys",sep="")
    df = df %>% dplyr::mutate(!!user_id_columnname := ifelse(!!sym(user_id_columnname) %in% user_ids, new_logical_id, !!sym(user_id_columnname)))
  }
  
  return(df)
}
