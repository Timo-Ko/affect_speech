# helper script to remove more cases

affect_egemaps <- readRDS("study1_ger/data/affect_egemaps.RData")
affect_compare <- readRDS("study1_ger/data/affect_compare.RData")

hist(affect_egemaps$VoicedSegmentsPerSec)
hist(affect_egemaps$MeanVoicedSegmentLengthSec)

affect_egemaps <- affect_egemaps %>%
  filter(VoicedSegmentsPerSec >0) %>%
  filter(MeanVoicedSegmentLengthSec >0)

table(duplicated(affect_egemaps$id))

# this only removes 8 cases!