#  Function 1: helper function to extract coefficients from trained full model

extract_beta_coefficients = function(coef.target){
  #prepare data 
  beta = as.data.frame(as.matrix(coef.target))
  beta = cbind(rownames(beta), beta)
  colnames(beta) = c("Variable", "Stand_Beta")
  beta = beta %>% dplyr::filter(!Variable == "(Intercept)")
  beta = beta[order(abs(beta$Stand_Beta), decreasing = TRUE), ]
  num.predictors.exceed0 = beta %>% dplyr::filter(abs(Stand_Beta) > 0) %>% nrow()
  beta = beta %>% dplyr::filter(abs(Stand_Beta) > 0)
  # beta = beta[1:5,]
  # beta$Group1 = NA
  # beta$Group2 = NA
  # 
  # for(i in 1:nrow(beta)){
  #   beta$Group1[i] = helper$Cue_Group1[helper$Feature == beta$Variable[i]]
  #   beta$Group2[i] = helper$Cue_Group2[helper$Feature == beta$Variable[i]]
  # }
  # 
  # beta$Group = factor(beta$Group, levels = c("Persons_Interactions", "Objects", "Events_Activities", "Locations" , "Time"))
  # beta$Variable = factor(beta$Variable, levels=rev(beta$Variable))
  
  return(list(beta, num.predictors.exceed0)) 
}
