# Install and load required packages 

packages <- c( "dplyr", "tidyr", "ggplot2", "gridExtra")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# read in benchmark results

bmr_egemaps_valence <- readRDS("results/bmr_egemaps_valence.RData")
bmr_egemaps_arousal <- readRDS("results/bmr_egemaps_arousal.RData")

####  SEMANTIC EFFECTS: VALENCE ####

## create plot showing the prediction error on y axis and valence/ arousal score on x-axis with 3 curves (sentence conditions separate and all together)

## get predictions from rf
aggr = bmr_egemaps_valence$aggregate()
rr = aggr$resample_result[[3]]
predictions_valence <- as.data.table(rr$prediction())

# append conditions 
predictions_valence_condition <- cbind(predictions_valence, affect_egemaps$condition)

# rename column
colnames(predictions_valence_condition)[colnames(predictions_valence_condition) == 'V2'] <- 'condition'

# compute error 
predictions_valence_condition$error <- abs(predictions_valence_condition$truth - predictions_valence_condition$response)

predictions_valence_condition$condition <- as.factor(predictions_valence_condition$condition)

predictions_valence_condition$error 

# check overall error per semantic condition
# no significant differences in mean error across conditions
cond_error <- predictions_valence_condition[,c("condition", "error")] %>% 
  group_by(condition) %>% 
  mutate(mean_error = mean(error))

# check error per truth value
cond_error_truth <- predictions_valence_condition[,c("truth", "condition", "error")] %>% 
  group_by(condition, truth) %>% 
  mutate(mean_error = mean(error))


# create plot
cond_valence_plot <- ggplot(data = predictions_valence_condition[,c("truth", "condition", "error")], 
                          aes_string(x= "truth" , y= "error", color = "condition")) +
  #geom_point() +
  stat_smooth(method='loess', se = T, span = 1)+
  scale_x_continuous("Valence") +
  scale_y_continuous("Absolute error") + 
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
