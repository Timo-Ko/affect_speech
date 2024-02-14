# preparation

# explore the data set
ema_audio_data[1, 1:100]

# plot some statistics 
length(unique(ema_audio_data$names.x)) #980 unique participants with EMA + voice records

ema_per_participant <- ema_audio_data %>%
  group_by(names.x)%>%
  count()%>%
  filter(n > 5)

hist(ema_per_participant$n, breaks = 100)

# if we want at least 5 samples per participant, we keep 774 participants 
# we could also do the between person predictions on ALL instances and the within-person ones on a sub sample 
length(unique(ema_per_participant$names.x)) 

# next steps:
# figure out participant groups
