#' Gets the emoticon scores for all words that were typed in the given typing sessions.
#'
#' @param cor_object as returned from psych::corr.test
#' @param targets a vector containing the target variable names
#' @return dataframe of correlations of features with targets with 95% r CI not containing zero

filter_corrs <- function(cor_object, targets){
  
  # create upper and lower ci bound matrices
  upper_ci <- cor_object$r + 1.96*cor_object$se
  lower_ci <- cor_object$r - 1.96*cor_object$se
  
  # filter out only relevant columns from matrices
  
  '%ni%' <- Negate('%in%')
  
  features <- colnames(cor_object$r)[colnames(cor_object$r) %ni% targets]
  
  sub_matrix <- cor_object$r[targets, features] # subset matrix with all correlations 
  
  upper_ci <- upper_ci[targets, features] 
  lower_ci <- lower_ci[targets, features] 
  
  # create another matrix checking if both cis bounds are pos or neg
  zero_in_ci <- lower_ci > 0 & upper_ci > 0  | lower_ci < 0 & upper_ci < 0 
  
  zero_in_ci_filtered <- zero_in_ci[, which(colSums(zero_in_ci) >0)]
  
  # filter r matrix
  sub_matrix_filtered <- sub_matrix[, c(colnames(zero_in_ci_filtered))]
  
  return(sub_matrix_filtered)
}