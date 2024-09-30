### PREPARATION ####

# Install and load required packages 

packages <- c("dplyr", "data.table", "psych","ggplot2", "ggcorrplot", "ggrepel", "tidyr", "tibble", "patchwork")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# read in data 

# study 1
affect_voice_study1 <- readRDS(file="data/study1/affect_voice_study1_cleaned.rds") 

# study 2
affect_voice_study2_cleaned <- readRDS(file="data/study2/affect_voice_study2_cleaned.rds") 

### COMPUTE VOICE X AFFECT CORRS ####

# compute corrs of targets w egemaps voice features

cor_voice_affect_study1_all <- psych::corr.test(affect_voice_study1[,c(which(colnames(affect_voice_study1)=="valence"), 
                                                                   which(colnames(affect_voice_study1)=="arousal"), 
                                                                   which(colnames(affect_voice_study1)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_study1)=="equivalentSoundLevel_dBp"))],  
                                            use="pairwise.complete.obs", 
                                            method = "pearson")


cor_voice_affect_study2_all <- psych::corr.test(affect_voice_study2_cleaned[,c(which(colnames(affect_voice_study2_cleaned)=="content"), 
                                                         which(colnames(affect_voice_study2_cleaned)=="sad"),
                                                         which(colnames(affect_voice_study2_cleaned)=="arousal"), 
                                                         which(colnames(affect_voice_study2_cleaned)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_study2_cleaned)=="equivalentSoundLevel_dBp"))],  
                                       use="pairwise.complete.obs", 
                                       method = "pearson")

# filter correlation table for affect targets
filter_corrs <- function(cor_object, targets){
  
  '%ni%' <- Negate('%in%')
  
  features <- colnames(cor_object$r)[colnames(cor_object$r) %ni% targets]
  
  sub_matrix <- cor_object$r[targets, features] # subset matrix with all correlations 
  
  return(sub_matrix)
}

cor_voice_affect_study1 <- filter_corrs(cor_voice_affect_study1_all, c("valence", "arousal"))
cor_voice_affect_study2 <- filter_corrs(cor_voice_affect_study2_all, c("content", "sad", "arousal"))

# save results 
write.csv(round(t(cor_voice_affect_study1),2), "results/cor_voice_affect_study1.csv")
write.csv(round(t(cor_voice_affect_study2),2), "results/cor_voice_affect_study2.csv")

## FINISH