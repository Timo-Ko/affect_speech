# Install and load required packages 

packages <- c( "dplyr", "tidyr","data.table", "ggplot2", "gridExtra")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# read in benchmark results
bmr_egemaps_valence <- readRDS("study1_ger/results/bmr_egemaps_valence.RData")
bmr_egemaps_arousal <- readRDS("study1_ger/results/bmr_egemaps_arousal.RData")

affect_egemaps <- readRDS("study1_ger/data/affect_egemaps.RData")

####  SEMANTIC EFFECTS: VALENCE ####

## create plot showing the prediction error on y axis and valence/ arousal score on x-axis with 3 curves (sentence conditions separate and all together)

## get predictions from rf
aggr_valence = bmr_egemaps_valence$aggregate()
rr_valence = aggr_valence$resample_result[[3]]
predictions_valence <- as.data.table(rr_valence$prediction())
predictions_valence$target_1 <- as.factor("valence")

# rename columns
colnames(predictions_valence)[colnames(predictions_valence) == 'truth'] <- 'truth_valence'
colnames(predictions_valence)[colnames(predictions_valence) == 'response'] <- 'response_valence'

## get predictions from rf
aggr_arousal = bmr_egemaps_arousal$aggregate()
rr_arousal = aggr_arousal$resample_result[[3]]
predictions_arousal <- as.data.table(rr_arousal$prediction())
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

# check error per truth value
cond_error_truth <- predictions_valence_condition[,c("truth", "condition", "error")] %>% 
  group_by(condition, truth) %>% 
  mutate(mean_error = mean(error))

# also i need to create a "target" column 
predictions_condition_long <- predictions_condition  %>% 
  pivot_longer(cols = c(target_1, target_2), names_to = "prediction", values_to = "target")  %>% 
  pivot_longer(cols = c(error_valence, error_arousal), names_to = "prediction_error", values_to = "error") 

# transform into long format
predictions_condition_long <-  gather(predictions_condition, condition, error, error_valence, error_arousal)
predictions_condition_long <-  pivot_longer(predictions_condition, condition, error, error_valence, error_arousal)


?pivot_longer
# to do: finish this to long format

# create bar plot
p <- ggplot(predictions_condition_long, aes(x= condition , y= error, color = target)) + 
  geom_boxplot()
p


# create plot
condition_error_plot <- ggplot(data = predictions_condition_long[,c("target", "condition", "error")], 
                          aes_string(x= "condition" , y= "error", color = "target")) +
  #geom_point() +
  #stat_smooth(method='loess', se = T, span = 1)+
  scale_x_discrete("Sentence sentiment") +
  scale_y_continuous("Absolute prediction error") + 
  theme_minimal(base_size = 20)

# error is much higher for low valence and no apparent differences across sentence conditions

####  SEMANTIC EFFECTS: AROUSAL ####

## get predictions from rf
aggr = bmr_egemaps_arousal$aggregate()
rr = aggr$resample_result[[3]]
predictions_arousal <- as.data.table(rr$prediction())

# append conditions 
predictions_arousal_condition <- cbind(predictions_arousal, affect_egemaps$condition)

# rename column
colnames(predictions_arousal_condition)[colnames(predictions_arousal_condition) == 'V2'] <- 'condition'

# compute error 
predictions_arousal_condition$error <- abs(predictions_arousal_condition$truth - predictions_arousal_condition$response)

# no significant differences in mean error across conditions
cond_error <- predictions_arousal_condition %>% group_by(condition) %>% mutate(mean_error = mean(error))

# create plot
cond_arousal_plot <- ggplot(data = predictions_arousal_condition[,c("truth", "condition", "error")], 
                            aes_string(x= "truth" , y= "error", color = "condition")) +
  #geom_point() +
  stat_smooth(method='loess', se = T, span = 1)+
  scale_x_continuous("arousal") +
  scale_y_continuous("Absolute error") + 
  theme_minimal(base_size = 20)


# save plots

png(file="figures/performance_condition.png",width=1000, height=800)

performance_condtion = grid.arrange(cond_valence_plot, cond_arousal_plot ,ncol = 2)

dev.off()
