#' @param data df containing features and targets
#' @param no_feature_columns a vector containing the colnames that are not targets
#' @return dataframe with preprocessed features

target_independent_preproc <- function(data, no_feature_columns){
  
  ## transform all features to numerics (if they are not already)
  data[, which(!colnames(data) %in% no_feature_columns)] = apply(data[, which(!colnames(data) %in% no_feature_columns)], 2, function(x) as.numeric(x))
  
  # ## replace extreme outliers (M+-4SD) with NA (will be imputed in target-dependent preprocessing)
  # data[, which(!colnames(data) %in% no_feature_columns)] = apply(data[, which(!colnames(data) %in% no_feature_columns)], 2, 
  #                                                                function(x) ifelse(x > (mean(x, na.rm = TRUE)+4*sd(x, na.rm = TRUE)) | x < (mean(x, na.rm = TRUE)-4*sd(x, na.rm = TRUE)), NA, x))
  # 
  return(data)
}
