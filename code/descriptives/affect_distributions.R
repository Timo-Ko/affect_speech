### DISTRIBUTION OF AFFECT OUTCOMES ACROSS DATA SETS ####

## Study 1

# load data

# create histograms

hist_valence <- ggplot(affect_acoustics, aes(x=sad)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,10000))+ 
  scale_x_continuous(name = "Sadness", breaks = c(0:3)) + 
  theme_minimal(base_size = 20)

hist_arousal_study1 <- ggplot(affect_acoustics, aes(x=arousal)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,10000))+ 
  scale_x_continuous(name = "Arousal", breaks = c(0:4)) + 
  theme_minimal(base_size = 20)


## Study 2

# load data

# create histograms 

hist_content <- ggplot(affect_acoustics, aes(x=content)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,10000))+ 
  scale_x_continuous(name = "Contentedness", breaks = c(0:3)) + 
  theme_minimal(base_size = 20)

hist_sad <- ggplot(affect_acoustics, aes(x=sad)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,10000))+ 
  scale_x_continuous(name = "Sadness", breaks = c(0:3)) + 
  theme_minimal(base_size = 20)

hist_arousal_study2 <- ggplot(affect_acoustics, aes(x=arousal)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,10000))+ 
  scale_x_continuous(name = "Arousal", breaks = c(0:4)) + 
  theme_minimal(base_size = 20)

# arrange histograms (2x2 matrix)

affect_dist <- 
  (hist_content + hist_diff_content) /
  (hist_sad + hist_diff_sad) /
  (hist_arousal + hist_diff_arousal)

# save figure
png(file="figures/us_affect_dist_overview.png",width=1000, height=1500)
affect_dist_voice
dev.off()

## FINISH