### PREPARATION ####

# Install and load required packages 

packages <- c("dplyr", "data.table", "psych","ggplot2", "ggcorrplot", "ggrepel", "tidyr", "tibble", "patchwork")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# read in data 

# study 1
affect_voice_study1  <- readRDS("data/study1/affect_egemaps.RData")

# study 2
affect_voice_study2  <- readRDS("data/study2/affect_acoustics.RData")

### COMPUTE CORRS ####

# compute corrs of targets w egemaps voice features

cor_voice_affect_study1 <- psych::corr.test(affect_voice_study1[,c(which(colnames(affect_voice_study1)=="valence"), 
                                                                   which(colnames(affect_voice_study1)=="arousal"), 
                                                                   which(colnames(affect_voice_study1)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_study1)=="equivalentSoundLevel_dBp"))],  
                                            use="pairwise.complete.obs", 
                                            method = "pearson")


cor_voice_affect_study2 <- psych::corr.test(affect_voice_study2[,c(which(colnames(affect_voice_study2)=="content"), 
                                                         which(colnames(affect_voice_study2)=="sad"),
                                                         which(colnames(affect_voice_study2)=="arousal"), 
                                                         which(colnames(affect_voice_study2)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_study2)=="equivalentSoundLevel_dBp"))],  
                                       use="pairwise.complete.obs", 
                                       method = "pearson")


# save corrs


### CREATE PLOTS

# study 1


# study 2

cor_voice_affect_study2_plot = ggcorrplot(cor_egemaps_affect_filtered[,c(1:(round(ncol(cor_egemaps_affect_filtered)/2)))], 
                                       method = "square", 
                                       legend.title = "Pearson\ncorrelation", 
                                       lab = TRUE, 
                                       lab_size = 3, 
                                       ggtheme = theme_minimal, 
                                       outline.color = "lightgray",
                                       colors = c("#6D9EC1", "white", "#E46726")) +  
  theme(axis.text.x = ggplot2::element_text(size = 12),
        axis.text.y = ggplot2::element_text(size = 12)) +
  scale_x_discrete(labels=c('Contentedness', 'Sadness', 'Arousal')) 

# save figure

png(file="figures/cor_voice_affect_study2_plot.png",width=750, height=1500)

cor_voice_affect_study2_plot

dev.off()


## FINISH