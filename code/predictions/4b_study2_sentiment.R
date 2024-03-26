# Install and load required packages 

packages <- c( "dplyr", "tidyr", "ggplot2", "gridExtra", "mlr3") 
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# read in benchmark results from acoustics predictions

bmr_egemaps <- readRDS("results/study2/bmr_egemaps_study2.rds")

# load df
affect_voice_wordembeddings <- readRDS("data/study2/affect_voice_wordembeddings.rds")

#### SENTIMENT DESCRIPTIVES ####

hist(affect_voice_wordembeddings$Sentiment.score)
summary(affect_voice_wordembeddings$Sentiment.score)

####  CONTENT SENTIMENT EFFECTS ON PREDICTION PERFORMANCE FROM VOICE ####

## create plot showing the prediction error on y axis and seconds of voiced speech score on x-axis with 3 curves (one for sad and one for content)

## get predictions from lasso (best performing algo) for contentedness
aggr = bmr_egemaps$aggregate(msrs("regr.mae"))


rf_content = aggr$resample_result[[2]]
predictions_content <- as.data.table(rf_content$prediction())

# rename columns
colnames(predictions_content)[colnames(predictions_content) == 'truth'] <- 'truth_content'
colnames(predictions_content)[colnames(predictions_content) == 'response'] <- 'response_content'

## get predictions from lasso (best performing algo) for sad
rf_sad = aggr$resample_result[[8]]
predictions_sad <- as.data.table(rf_sad$prediction())

# rename columns
colnames(predictions_sad)[colnames(predictions_sad) == 'truth'] <- 'truth_sad'
colnames(predictions_sad)[colnames(predictions_sad) == 'response'] <- 'response_sad'

## get predictions from lasso (best performing algo) for arousal
rf_arousal = aggr$resample_result[[14]]
predictions_arousal <- as.data.table(rf_arousal$prediction())

# rename columns
colnames(predictions_arousal)[colnames(predictions_arousal) == 'truth'] <- 'truth_arousal'
colnames(predictions_arousal)[colnames(predictions_arousal) == 'response'] <- 'response_arousal'

# create one df
predictions_sentiment <- cbind(predictions_content, predictions_sad, predictions_arousal, affect_voice_wordembeddings$Sentiment.score)

# rename column for sentiment
colnames(predictions_sentiment)[colnames(predictions_sentiment) == 'V4'] <- 'sentiment'

# compute prediction error 
predictions_sentiment$error_content <- abs(predictions_sentiment$truth_content - predictions_sentiment$response_content)
predictions_sentiment$error_sad <- abs(predictions_sentiment$truth_sad - predictions_sentiment$response_sad)
predictions_sentiment$error_arousal <- abs(predictions_sentiment$truth_arousal - predictions_sentiment$response_arousal)

head(predictions_sentiment)

# save data 

saveRDS(predictions_sentiment, "results/study2/predictions_sentiment.rds")

# compute correlations of sentiment score and prediction error for each target
cor(predictions_sentiment$sentiment, predictions_sentiment$error_content, use = "complete.obs")
cor(predictions_sentiment$sentiment, predictions_sentiment$error_arousal, use = "complete.obs")
cor(predictions_sentiment$sentiment, predictions_sentiment$error_sad, use = "complete.obs")

# convert to long format
predictions_sentiment_long <-  gather(predictions_sentiment, target, error, error_content, error_sad, error_arousal)

# remove unneeded columns
predictions_sentiment_long$row_ids <- NULL
predictions_sentiment_long$row_ids <- NULL
predictions_sentiment_long$row_ids <- NULL

# rename 
predictions_sentiment_long <- predictions_sentiment_long %>% 
  mutate(target = case_when(
    target == "error_content" ~    "Contentedness",
    target == "error_sad" ~ "Sadness",
    target == "error_arousal" ~ "Arousal"))

# create plot
sentiment_error_plot <- ggplot(data = predictions_sentiment_long[,c("sentiment", "error", "target")], 
                                   aes_string(x= "sentiment" , y= "error", color = "target")) +
  #geom_point() +
  stat_smooth(method='loess', se = T, span = 1)+
  scale_x_continuous("Sentiment score") +
  scale_y_continuous("Absolute prediction error") + 
  theme_minimal(base_size = 20 ) + labs(colour = "Prediction Target") +
  theme(legend.position="top")

sentiment_error_plot

# save figure

png(file="figures/sentiment_error_plot.png",width=1000, height=750)

sentiment_error_plot

dev.off()

# FINISH