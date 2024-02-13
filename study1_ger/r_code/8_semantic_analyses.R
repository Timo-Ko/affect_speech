# Install and load required packages 

packages <- c( "dplyr", "tidyr","data.table", "ggplot2", "gridExtra")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# read in benchmark results
bmr_egemaps <- readRDS("study1_ger/results/bmr_egemaps.RData")

affect_egemaps <- readRDS("study1_ger/data/affect_egemaps.RData")

####  SEMANTIC EFFECTS: VALENCE ####

## create plot showing the prediction error on y axis and valence/ arousal score on x-axis with 3 curves (sentence conditions separate and all together)

## get predictions from rf
aggr = bmr_egemaps$aggregate(msrs("regr.mae"))
rf_valence = aggr$resample_result[[2]]
predictions_valence <- as.data.table(rf_valence$prediction())
predictions_valence$target_1 <- as.factor("valence")

# rename columns
colnames(predictions_valence)[colnames(predictions_valence) == 'truth'] <- 'truth_valence'
colnames(predictions_valence)[colnames(predictions_valence) == 'response'] <- 'response_valence'

## get predictions from rf
rf_arousal = aggr$resample_result[[8]]
predictions_arousal <- as.data.table(rf_arousal$prediction())
predictions_arousal$target_2 <- as.factor("arousal")

# rename columns
colnames(predictions_arousal)[colnames(predictions_arousal) == 'truth'] <- 'truth_arousal'
colnames(predictions_arousal)[colnames(predictions_arousal) == 'response'] <- 'response_arousal'

# append sentence conditions 
predictions_condition <- cbind(predictions_valence, predictions_arousal, affect_egemaps$condition)

# rename column
colnames(predictions_condition)[colnames(predictions_condition) == 'V3'] <- 'condition'

# compute error 
predictions_condition$error_valence <- abs(predictions_condition$truth_valence - predictions_condition$response_valence)
predictions_condition$error_arousal <- abs(predictions_condition$truth_arousal - predictions_condition$response_arousal)

predictions_condition$condition <- as.factor(predictions_condition$condition)

head(predictions_condition)

# check overall error per semantic condition
# no significant differences in mean error across conditions
cond_error <- predictions_condition[,c("condition", "error_valence", "error_arousal")] %>% 
  group_by(condition) %>% 
  mutate(mean_error_valence = mean(error_valence)) %>% 
  mutate(mean_error_arousal = mean(error_arousal)) 


# save data 
saveRDS(predictions_condition, file = "study1_ger/results/predictions_condition.RData")

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
  theme_minimal(base_size = 20) + labs(x ="Sentence sentiment", color="Prediction Target")

# save plot

png(file="figures/predictions_condition_plot.png",width=1000, height=700)

predictions_condition_plot

dev.off()

## FINISH


## OLD CODE
# create plot
# condition_error_plot <- ggplot(data = predictions_condition_long[,c("target", "condition", "error")], 
#                           aes_string(x= "condition" , y= "error", color = "target")) +
#   #geom_point() +
#   #stat_smooth(method='loess', se = T, span = 1)+
#   scale_x_discrete("Sentence sentiment") +
#   scale_y_continuous("Absolute prediction error") + 
#   theme_minimal(base_size = 20)
# 
# # error is much higher for low valence and no apparent differences across sentence conditions

