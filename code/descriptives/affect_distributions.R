### DISTRIBUTION OF AFFECT OUTCOMES ACROSS DATA SETS (FINAL DATA) ####

library(ggplot2)
library(patchwork)

## Study 1

# load data
affect_voice_study1 <- readRDS(file="data/study1/affect_voice_study1_cleaned.rds") 

# create histograms

hist_valence_study1 <- ggplot(affect_voice_study1, aes(x=valence)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,10000))+ 
  scale_x_continuous(name = "Valence (Study 1)", breaks = c(1:6)) + 
  theme_minimal(base_size = 20)

hist_arousal_study1 <- ggplot(affect_voice_study1, aes(x=arousal)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,10000))+ 
  scale_x_continuous(name = "Arousal (Study 1)", breaks = c(1:6)) + 
  theme_minimal(base_size = 20)

## Study 2

# load data
affect_voice_study2_cleaned <- readRDS(file="data/study2/affect_voice_study2_cleaned.rds") 

# create histograms 

hist_content_study2 <- ggplot(affect_voice_study2_cleaned , aes(x=content)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,10000))+ 
  scale_x_continuous(name = "Contentment (Study 2)", breaks = c(0:3)) + 
  theme_minimal(base_size = 20)

hist_sad_study2 <- ggplot(affect_voice_study2_cleaned , aes(x=sad)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,10000))+ 
  scale_x_continuous(name = "Sadness (Study 2)", breaks = c(0:3)) + 
  theme_minimal(base_size = 20)

hist_arousal_study2 <- ggplot(affect_voice_study2_cleaned , aes(x=arousal)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,10000))+ 
  scale_x_continuous(name = "Arousal (Study 2)", breaks = c(0:4)) + 
  theme_minimal(base_size = 20)

# arrange histograms (2x3 matrix)

affect_dist <- 
  (hist_valence_study1 + hist_arousal_study1) /
  (hist_content_study2 + hist_sad_study2 + hist_arousal_study2 )

# save figure
png(file="figures/affect_dist_overview.png",width=1000, height=1000)
affect_dist
dev.off()

## FINISH