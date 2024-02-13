### PREPARATION ####

## Install and load required packages 

# install and load required packages
packages <- c("dplyr", "data.table", "psych","ggplot2", "ggcorrplot", "ggrepel", "patchwork")

install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

source("study1_ger/r_code/functions/filter_corrs.R")

# read in data frame
affect_egemaps  <- readRDS("study1_ger/data/affect_egemaps.RData")

## subset df
colnames(affect_egemaps)

## compute corrs

cor_egemaps_affect <- psych::corr.test(affect_egemaps[,c(which(colnames(affect_egemaps)=="valence"), 
                                                         which(colnames(affect_egemaps)=="diff_valence"),
                                                         which(colnames(affect_egemaps)=="arousal"), 
                                                         which(colnames(affect_egemaps)=="diff_arousal"), 
                                                         which(colnames(affect_egemaps)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_egemaps)=="equivalentSoundLevel_dBp"))],  
                                       use="pairwise.complete.obs", 
                                       method = "pearson")


targets <- c("valence", "diff_valence", "arousal", "diff_arousal") # get specific col names

cor_egemaps_affect_filtered <- filter_corrs(cor_egemaps_affect, targets) # filter correlations

# split and create two plots bc too many corrs

dim(cor_egemaps_affect_filtered)

# change the legend colors
cor_egemaps_affect_plot_1 = ggcorrplot(cor_egemaps_affect_filtered[,c(1:(ncol(cor_egemaps_affect_filtered)/2))], 
                                     method = "square", 
                                     legend.title = "Pearson\ncorrelation", 
                                     lab = TRUE, 
                                     lab_size = 4, 
                                     ggtheme = theme_minimal, 
                                     outline.color = "lightgray",
                                     colors = c("#6D9EC1", "white", "#E46726")) +  
  theme(axis.text.x = ggplot2::element_text(size = 15),
           axis.text.y = ggplot2::element_text(size = 15)) +
          scale_x_discrete(labels=c('Valence', 'Valence Fluct.', 'Arousal', 'Arousal Fluct.')) 

cor_egemaps_affect_plot_2 = ggcorrplot(cor_egemaps_affect_filtered[,c(((ncol(cor_egemaps_affect_filtered)/2)+1):ncol(cor_egemaps_affect_filtered))], 
                                       method = "square", 
                                       legend.title = "Pearson\ncorrelation", 
                                       lab = TRUE, 
                                       lab_size = 4, 
                                       ggtheme = theme_minimal, 
                                       outline.color = "lightgray",
                                       colors = c("#6D9EC1", "white", "#E46726")) +  
  theme(axis.text.x = ggplot2::element_text(size = 15),
        axis.text.y = ggplot2::element_text(size = 15)) +
  scale_x_discrete(labels=c('Valence', 'Valence Fluct.', 'Arousal', 'Arousal Fluct.')) 


# save figures

png(file="figures/cor_egemaps_affect_ger_plot_1.png",width=750, height=1500)

cor_egemaps_affect_plot_1

dev.off()

png(file="figures/cor_egemaps_affect_ger_plot_2.png",width=750, height=1500)

cor_egemaps_affect_plot_2

dev.off()

### DISTRIBUTION OF AFFECT OUTCOMES ACROSS DATA SETS ####

hist_valence <- ggplot(affect_egemaps, aes(x=valence)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,6000))+ 
  scale_x_continuous(name = "Valence", breaks = c(1:6)) + 
  theme_minimal(base_size = 20)

hist_diff_valence <- ggplot(affect_egemaps, aes(x=diff_valence)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,6000))+ 
  scale_x_continuous(name = "Valence Fluctuation", seq(-5, 5, 1)) + 
  theme_minimal(base_size = 20)

hist_arousal <- ggplot(affect_egemaps, aes(x=arousal)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,4000))+ 
  scale_x_continuous(name = "Arousal", breaks = c(1:6)) + 
  theme_minimal(base_size = 20)

hist_diff_arousal <- ggplot(affect_egemaps, aes(x=diff_arousal)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,4000))+ 
  scale_x_continuous(name = "Arousal Fluctuation", seq(-5, 5, 1)) + 
  theme_minimal(base_size = 20)

# arrange histograms (2x2 matrix)

affect_dist_voice <- 
  (hist_valence + hist_diff_valence) /
  (hist_arousal + hist_diff_arousal)

# save figure
png(file="figures/ger_affect_dist_overview.png",width=1000, height=1000)
affect_dist_voice
dev.off()

## FINISH