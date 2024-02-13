# collection of diverse helper functions 

# calculate mode
mode.knn =  function(x){
  uniq.x = unique(x)
  uniq.x = uniq.x[which(!is.na(uniq.x))]
  knn = uniq.x[which.max(tabulate(match(x, uniq.x)))]
  return(knn)
}


# find [k] neigherst neigbors and impute by their mode 
impute.knn = function(y, k){
  t = which(is.na(y))
  if(length(t) == 0){
    return(y)
  }else{
    is = 1:length(t)
    for(i in is){
      if(i > k){
        look.at = y[(t[i]-k):(t[i]+k)]
        y[t[i]] = mode.knn(look.at)
      }
      if(i <= k){
        look.at = y[1:(t[i]+k)]
        y[t[i]] = mode.knn(look.at)
      }
    }
    return(y)
  }
}


# find pattern in data
occur <- function(patrn, x) {
  patrn.rev <- rev(patrn)
  w <- embed(x,length(patrn))
  which(apply(w, 1, function(r) all(r == patrn.rev)))
}

# unfold json data into one column per key-value pair
parseJsonColumnSensing <- function(df, column_name){
  parseJsonColumn <- function(x)  {
    str_c("[ ", str_c(x, collapse = ",", sep=" "), " ]")  %>% 
      fromJSON(flatten = T) %>% 
      as_tibble()
  }
  
  df2 <- df %>% 
    select(user_id,client_db_id,!!column_name) %>%
    filter(!is.na(!!rlang::sym(column_name))) %>%
    map_dfc(.f = parseJsonColumn) %>%
    distinct()
  
  df3 <- df %>%
    left_join(df2, by=c("user_id"="value...1","client_db_id"="value...2")) %>%
    dplyr::select(-!!column_name)
  
  return(df3)
  
}