# Install and load required packages 

packages <- c( "dplyr", "tidyr", "mlr3", "data.table", "ggplot2", "gridExtra")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# read in benchmark results
bmr_egemaps <- readRDS("results/study1/bmr_study1.rds")

# load data
affect_voice_study1 <- readRDS("data/study1/affect_voice_study1_cleaned.rds")

# remove illegal characters from colnames 
colnames(affect_voice_study1) <- make.names(colnames(affect_voice_study1), unique = TRUE)

####  DESCRIPTIVE DIFFERENCES IN VOICE FEATURES AMONG THREE SENTENCE CONDITIONS ####

egemaps_features <- colnames(affect_voice_study1)[which(colnames(affect_voice_study1) == "F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_study1) == "equivalentSoundLevel_dBp")]

# Calculate the average values for each feature across the three conditions
voice_by_condition <- affect_voice_study1 %>%
  select(condition, all_of(egemaps_features)) %>%
  group_by(condition) %>%
  summarize(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -condition, names_to = "feature", values_to = "value") %>%
  pivot_wider(names_from = condition, values_from = value)

# save results 
write.csv2(voice_by_condition, "results/study1/voice_by_condition.csv")

####  CONTENT SENTIMENT EFFECTS ON VOICE PREDICTIONS ####

## create plot showing the prediction error on y axis and valence/ arousal score on x-axis with 3 curves (sentence conditions separate and all together)

aggr = bmr_egemaps$aggregate(msrs("regr.mae")) # get aggr performance

## get valence en predictions 
rr_valence = aggr$resample_result[[3]]
predictions_valence <- as.data.table(rr_valence$prediction())
predictions_valence$target_1 <- as.factor("valence")

# rename columns
colnames(predictions_valence)[colnames(predictions_valence) == 'truth'] <- 'truth_valence'
colnames(predictions_valence)[colnames(predictions_valence) == 'response'] <- 'response_valence'

## get arousal en predictions 
rr_arousal = aggr$resample_result[[6]]
predictions_arousal <- as.data.table(rr_arousal$prediction())
predictions_arousal$target_2 <- as.factor("arousal")

# rename columns
colnames(predictions_arousal)[colnames(predictions_arousal) == 'truth'] <- 'truth_arousal'
colnames(predictions_arousal)[colnames(predictions_arousal) == 'response'] <- 'response_arousal'

# append sentence conditions 
predictions_condition <- cbind(predictions_valence, predictions_arousal, affect_voice_study1$condition)

# rename column
colnames(predictions_condition)[colnames(predictions_condition) == 'V3'] <- 'condition'

# compute error 
predictions_condition$error_valence <- abs(predictions_condition$truth_valence - predictions_condition$response_valence)
predictions_condition$error_arousal <- abs(predictions_condition$truth_arousal - predictions_condition$response_arousal)

predictions_condition$condition <- as.factor(predictions_condition$condition)

head(predictions_condition)

# no significant differences in mean error across conditions
cond_error <- predictions_condition[,c("condition", "error_valence", "error_arousal")] %>% 
  group_by(condition) %>% 
  mutate(mean_error_valence = mean(error_valence)) %>% 
  mutate(mean_error_arousal = mean(error_arousal)) 

# save data 
saveRDS(predictions_condition, file = "results/study1/predictions_condition.rds")

# also i need to create a "target" column, reshape to long format
predictions_condition_long <- predictions_condition  %>% 
  pivot_longer(cols = c(target_1, target_2), names_to = "prediction", values_to = "target")  %>% 
  pivot_longer(cols = c(error_valence, error_arousal), names_to = "prediction_error", values_to = "error") 

# rename 
predictions_condition_long <- predictions_condition_long %>% 
  mutate(target = case_when(
    target == "valence" ~    "Valence",
    target == "arousal" ~ "Arousal"))

# create bar plot
predictions_condition_plot <- ggplot(predictions_condition_long, aes(x= condition , y= error, color = target)) + 
  geom_boxplot() +
  scale_x_discrete("Sentence sentiment") +
  scale_y_continuous("Absolute prediction error") + 
  theme_minimal(base_size = 20) + labs(x ="Sentence sentiment", color="Prediction Target") +
  theme(legend.position="top")

# save plot

png(file="figures/prediction_error_en_condition_study1_plot.png",width=1000, height=700)

predictions_condition_plot

dev.off()

# run F Test to compare prediction errors 

library(car)

# run anova comparing means for valence 
aov_valence <- car::Anova(aov(error_valence ~ condition, data = predictions_condition))

# run anova comparing means for arousal
aov_arousal <- car::Anova(aov(error_arousal ~ condition, data = predictions_condition))


## FINISH