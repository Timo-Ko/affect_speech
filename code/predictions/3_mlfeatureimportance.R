### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "data.table", "ranger", "ggplot2", "mlr3verse", "mlr3learners", "parallel", "mlr3tuning", "stringr", "DALEX", "DALEXtra", "iml", "patchwork", "forcats")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

set.seed(123, kind = "L'Ecuyer") # set seed to make sure all results are reproducible

source("code/functions/extract_betas.R")

# study 1
affect_voice_study1  <- readRDS("data/study1/affect_voice_study1_ml.rds")

# remove illegal characters from colnames 
colnames(affect_voice_study1) <- make.names(colnames(affect_voice_study1), unique = TRUE)

# study 2
affect_voice_wordembeddings_study2 <- readRDS("data/study2/affect_voice_wordembeddings_study2_ml.rds")

# remove illegal characters from colnames 
colnames(affect_voice_wordembeddings_study2) <- make.names(colnames(affect_voice_wordembeddings_study2), unique = TRUE)

#### MISSING DATA IMPUTATION ####

## create tasks for missing value imputation

# study 1
egemaps_arousal_study1 = TaskRegr$new(id = "egemaps_arousal", 
                                      backend = affect_voice_study1[,c(which(colnames(affect_voice_study1)=="arousal"),  
                                                                       which(colnames(affect_voice_study1)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_study1)=="equivalentSoundLevel_dBp"))], 
                                      target = "arousal")

# study 2
egemaps_content_study2 = TaskRegr$new(id = "egemaps_content", 
                                      backend = affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)== "content"), 
                                                                                      which(colnames(affect_voice_wordembeddings_study2)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_wordembeddings_study2)=="equivalentSoundLevel_dBp"))], 
                                      target = "content")

egemaps_arousal_study2 = TaskRegr$new(id = "egemaps_arousal", 
                                      backend = affect_voice_wordembeddings_study2[,c(which(colnames(affect_voice_wordembeddings_study2)== "arousal"), 
                                                                                      which(colnames(affect_voice_wordembeddings_study2)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_wordembeddings_study2)=="equivalentSoundLevel_dBp"))], 
                                      target = "arousal")

# create pipeline to impute missings
po_impute = po("imputehist") 

# Apply the pipeline to the tasks
egemaps_arousal_study1_imp <- po_impute$train(list(egemaps_arousal_study1))[[1]]
egemaps_content_study2_imp <- po_impute$train(list(egemaps_content_study2))[[1]]
egemaps_arousal_study2_imp <- po_impute$train(list(egemaps_arousal_study2))[[1]]

# get imputed data
affect_voice_study1_arousal_imputed = as.data.frame(egemaps_arousal_study1_imp$data())

affect_voice_study2_content_imputed = as.data.frame(egemaps_content_study2_imp$data())
affect_voice_study2_arousal_imputed = as.data.frame(egemaps_arousal_study2_imp$data())

#### TRAIN MODELS ####

# standardize outcome variables bc they are scaled differently!
summary(affect_voice_study1_arousal_imputed$arousal)
summary(affect_voice_study2_arousal_imputed$arousal)
summary(affect_voice_study2_content_imputed$content)

affect_voice_study1_arousal_imputed$arousal_stand <- scale(affect_voice_study1_arousal_imputed$arousal, center = TRUE, scale = TRUE)
affect_voice_study2_arousal_imputed$arousal_stand <- scale(affect_voice_study2_arousal_imputed$arousal, center = TRUE, scale = TRUE)
affect_voice_study2_content_imputed$content_stand <- scale(affect_voice_study2_content_imputed$content, center = TRUE, scale = TRUE)

## create new prediction tasks w imputed data

# study 1
egemaps_arousal_study1_imp = TaskRegr$new(id = "egemaps_arousal_study1_imp", 
                                      backend = subset(affect_voice_study1_arousal_imputed, select = -arousal),
                                      target = "arousal_stand")

# study 2
egemaps_content_study2_imp = TaskRegr$new(id = "egemaps_content_study2_imp", 
                                      backend = subset(affect_voice_study2_content_imputed, select = -content),
                                      target = "content_stand")

egemaps_arousal_study2_imp = TaskRegr$new(id = "egemaps_arousal_study2_imp", 
                                      backend = subset(affect_voice_study2_arousal_imputed, select = -arousal),
                                      target = "arousal_stand")

# create lasso learners
lrn_rr_arousal_study1 = lrn("regr.cv_glmnet")
lrn_rr_content_study2 = lrn("regr.cv_glmnet")
lrn_rr_arousal_study2 = lrn("regr.cv_glmnet")

# train models

model_rr_egemaps_arousal_study1 <- lrn_rr_arousal_study1$train(egemaps_arousal_study1_imp) # train model
model_rr_egemaps_content_study2 <- lrn_rr_content_study2$train(egemaps_content_study2_imp) # train model
model_rr_egemaps_arousal_study2 <- lrn_rr_arousal_study2$train(egemaps_arousal_study2_imp) # train model

# save trained models
saveRDS(model_rr_egemaps_arousal_study1, "results/study1/model_rr_egemaps_arousal_study1.rds") 
saveRDS(model_rr_egemaps_content_study2, "results/study2/model_rr_egemaps_content_study2.rds") 
saveRDS(model_rr_egemaps_arousal_study2, "results/study2/model_rr_egemaps_arousal_study2.rds") 

#### SINGLE FEATURE IMPORTANCE: VOICE FEATURES ####

# load models 

model_rr_egemaps_arousal_study1 <- readRDS( "results/study1/model_rr_egemaps_arousal_study1.rds") 
model_rr_egemaps_content_study2 <- readRDS("results/study2/model_rr_egemaps_content_study2.rds") 
model_rr_egemaps_arousal_study2 <- readRDS( "results/study2/model_rr_egemaps_arousal_study2.rds") 

## get selected features from lasso model 

model_rr_egemaps_arousal_study1$selected_features()
model_rr_egemaps_content_study2$selected_features()
model_rr_egemaps_arousal_study2$selected_features()

## get beta weights from lasso model 
coef_arousal_study1 = coef(model_rr_egemaps_arousal_study1$model, s = "lambda.1se") #s because this was the value used for training/evaluation
coef_content_study2 = coef(model_rr_egemaps_content_study2$model, s = "lambda.1se") #s because this was the value used for training/evaluation
coef_arousal_study2 = coef(model_rr_egemaps_arousal_study2$model, s = "lambda.1se") #s because this was the value used for training/evaluation

coefficients_arousal_study1 = extract_beta_coefficients(coef_arousal_study1)[[1]]
coefficients_content_study2 = extract_beta_coefficients(coef_content_study2)[[1]]
coefficients_arousal_study2 = extract_beta_coefficients(coef_arousal_study2)[[1]]

rownames(coefficients_arousal_study1) <- NULL
rownames(coefficients_content_study2) <- NULL
rownames(coefficients_arousal_study2) <- NULL

# save betas
write.csv2(coefficients_arousal_study1, "results/coefficients_arousal_study1.csv")
write.csv2(coefficients_content_study2, "results/coefficients_content_study2.csv")
write.csv2(coefficients_arousal_study2, "results/coefficients_arousal_study2.csv")

# compute abs betas or number of features from each subgroup in the final model?
# dots und dann profil verbinden?

# find features that are in study 2 models and also in arousal study 1 model 

# features_study2 <- unique(coefficients_content_study2$Variable, coefficients_arousal_study2$Variable)
# 
# shared_features <- coefficients_arousal_study1 %>% filter(coefficients_arousal_study1$Variable %in% features_study2)

# Assuming your coefficients_arousal_study1 data frame is already sorted by Stand_Beta or you may sort it first
# coefficients_arousal_study1 <- coefficients_arousal_study1[order(abs(coefficients_arousal_study1$Stand_Beta), decreasing = TRUE), ]

# Select the top 10 features by absolute standardized beta value
#top_features <- coefficients_arousal_study1[1:10, ]

# # Add specific features if they are not already in the top 10
# additional_features <- c("loudness_sma3_meanFallingSlope", "F3amplitudeLogRelF0_sma3nz_stddevNorm")
# additional_features_df <- coefficients_arousal_study1[coefficients_arousal_study1$Variable %in% additional_features, ]
# 
# # Combine top features with the additional specific features, avoiding duplicates
# coefficients_arousal_study1_subset <- rbind(top_features, additional_features_df)
# coefficients_arousal_study1_subset <- subset_df[!duplicated(subset_df$Variable), ]

# create data frame
combined_df <- data.frame(
  Feature = c(coefficients_arousal_study1$Variable[1:5], coefficients_content_study2$Variable[1:5], coefficients_arousal_study2$Variable[1:5]),
  Stand_Beta = c(coefficients_arousal_study1$Stand_Beta[1:5], coefficients_content_study2$Stand_Beta[1:5], coefficients_arousal_study2$Stand_Beta[1:5]),
  Target = c(rep("Arousal (scripted speech)", 5), rep("Contentedness (free speech)", 5), rep("Arousal (free speech)", 5))#,
  #Feature_Group = c("Frequency", "Spectral", ...)
)

# Create a complete set of all combinations of features and tasks
all_combinations <- expand.grid(Feature = unique(combined_df$Feature), Target = unique(combined_df$Target))

# Left join the original data with all combinations and replace NA with 0
combined_df <- merge(all_combinations, combined_df, by = c("Feature", "Target"), all = TRUE) %>%
  replace(is.na(.), 0)

combined_df$Feature <- factor(combined_df$Feature, levels = unique(combined_df$Feature)) # convert feature to factor

#full_feature_order <- colnames(affect_voice_study1)[14:101]

# # Filter the full order to include only features present in 'combined_df'
# subset_feature_order <- full_feature_order[full_feature_order %in% combined_df$Feature]
# 
# # Reorder the 'Feature' factor levels in 'combined_df' according to 'subset_feature_order'
# combined_df$Feature <- factor(combined_df$Feature, levels = subset_feature_order)

feature_order <- combined_df %>%
  group_by(Feature) %>%
  summarise(max_signed_beta = if_else(max(Stand_Beta) > abs(min(Stand_Beta)), max(Stand_Beta), min(Stand_Beta))) %>%  # Max with sign retention
  arrange(desc(max_signed_beta))

# # Reorder Feature based on the absolute magnitude of Stand_Beta in descending order
# combined_df <- combined_df %>%
#   dplyr::mutate(Feature = fct_reorder(Feature, Stand_Beta, .desc = TRUE))

# # Set the desired order for your tasks
# task_levels <- c("Arousal (scripted speech)", "Arousal (free speech)", "Contentedness (free speech)")
# 
# # Convert Task to a factor and set levels in the desired order
# combined_df$Target <- factor(combined_df$Target, levels = task_levels)

# Join feature order back to the combined_df to ensure consistent feature ordering
combined_df <- left_join(combined_df, feature_order, by = "Feature")
combined_df$Feature <- factor(combined_df$Feature, levels = feature_order$Feature)

# Set the desired order for your targets
task_levels <- c("Arousal (scripted speech)", "Arousal (free speech)", "Contentedness (free speech)")
combined_df$Target <- factor(combined_df$Target, levels = task_levels)


# create the plot

# # connected dots
# betas_plot <- ggplot(combined_df, aes(x = fct_rev(Feature), y = Stand_Beta, group = Target, color = Target)) +
#   geom_point() +
#   geom_line() + # This will connect the dots in the order of factors
#   coord_flip() +
#   theme_minimal(base_size = 25) +
#   labs(x = element_blank() ,y = "Standardized Beta Coefficient") +
#   theme(axis.text.x = element_text(angle = -45, hjust = 0), legend.position = "top") + # Rotate x labels for better readability
#   scale_color_manual(values = c("Arousal (free speech)" = "#1f78b4" ,
#                                 "Arousal (scripted speech)" = "#a6cee3", 
#                                 "Contentedness (free speech)" = "#b2df8a"))

# grouped bar plot
betas_grouped_bar_plot <- ggplot(combined_df, aes(x = fct_rev(Feature), y = Stand_Beta, fill = Target)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) + # Use position_dodge to create grouped bars
  coord_flip() + # Flips the axes so that features are on the y-axis
  theme_minimal(base_size = 25) +
  labs(y = "Standardized Beta Coefficient", x = element_blank()) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0), # Adjust text angle and position for readability
         legend.position = "top") + # Positions the legend at the top
  scale_fill_manual(values = c("Arousal (free speech)" = "#1f78b4",
                               "Arousal (scripted speech)" = "#a6cee3", 
                               "Contentedness (free speech)" = "#b2df8a"))


# save plot 
png(file="figures/betas_plot.png",width=1500, height=1500)

betas_grouped_bar_plot

dev.off()


#### GROUPED FEATURE IMPORTANCE PER FEATURE GROUP ####

# load models 

model_rr_egemaps_arousal_study1 <- readRDS( "results/study1/model_rr_egemaps_arousal_study1.rds") 
model_rr_egemaps_content_study2 <- readRDS("results/study2/model_rr_egemaps_content_study2.rds") 
model_rr_egemaps_arousal_study2 <- readRDS( "results/study2/model_rr_egemaps_arousal_study2.rds") 

## create dalex explainers for each model 

rr_exp_arousal_study1 <- DALEXtra::explain_mlr3(model_rr_egemaps_arousal_study1,
                                                data = affect_voice_study1_arousal_imputed,
                                                y        = affect_voice_study1_arousal_imputed$arousal,
                                                label    = "rr_exp_arousal_study1",
                                                colorize = FALSE)

rr_exp_content_study2 <- DALEXtra::explain_mlr3(model_rr_egemaps_content_study2,
                                                data = affect_voice_study2_content_imputed,
                                                y        = affect_voice_study2_content_imputed$content,
                                                label    = "rr_exp_content_study2",
                                                colorize = FALSE)

rr_exp_arousal_study2 <- DALEXtra::explain_mlr3(model_rr_egemaps_arousal_study2,
                                                data = affect_voice_study2_arousal_imputed,
                                                y        = affect_voice_study2_arousal_imputed$arousal,
                                                label    = "rr_exp_arousal_study2",
                                                colorize = FALSE)

## get feature-group assignment

# load csv with egemaps feature groups 
egemaps_groups <- read.csv2("data/egemaps_feature_groups.csv")

# update names
egemaps_groups$feature <- colnames(affect_voice_study1[,c(which(colnames(affect_voice_study1)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_study1)=="equivalentSoundLevel_dBp"))])

# create nested list containing attributes and their respective features 

variable_groups <- list() # create an empty list 

# get colnames of each features group
variable_groups[[1]] = egemaps_groups %>% filter(parameter_group == "frequency") %>% pull(feature)
variable_groups[[2]] = egemaps_groups %>% filter(parameter_group == "energy_amplitude") %>% pull(feature)
variable_groups[[3]] = egemaps_groups %>% filter(parameter_group == "spectral") %>% pull(feature)
variable_groups[[4]] = egemaps_groups %>% filter(parameter_group == "temporal") %>% pull(feature)

# Optionally, set names for each list element
names(variable_groups) <- c("frequency", "energy_amplitude", "spectral", "temporal") # add names to the list 

## compute grouped permutation feature importance

imp_grouped_arousal_study1 = DALEX::model_parts(explainer = rr_exp_arousal_study1,
                                                loss_function = loss_root_mean_square,
                                                B = 10, # number of permutations
                                                type = "variable_importance",
                                                variable_groups = variable_groups)

imp_grouped_content_study2 = DALEX::model_parts(explainer = rr_exp_content_study2,
                                                loss_function = loss_root_mean_square,
                                                B = 10, # number of permutations
                                                type = "variable_importance",
                                                variable_groups = variable_groups)

imp_grouped_arousal_study2 = DALEX::model_parts(explainer = rr_exp_arousal_study2,
                                                loss_function = loss_root_mean_square,
                                                B = 10, # number of permutations
                                                type = "variable_importance",
                                                variable_groups = variable_groups)


# plot results of grouped feature importance
plot_vi_grouped = plot(imp_grouped_arousal_study1) +
  ggtitle("Mean variable-importance over 10 permutations", "") +
  labs(subtitle = "")

# combine plots

saveRDS(plot_vi_grouped, "plot_imp_grouped.rds")



# ## supplement: get permutation feature importance for single features
# 
# imp_arousal_study1 = DALEX::model_parts(explainer = rr_exp_arousal_study1,
#                                                 loss_function = loss_root_mean_square,
#                                                 B = 10, # number of permutations
#                                                 type = "variable_importance")
# 
# imp_content_study2 = DALEX::model_parts(explainer = rr_exp_content_study2,
#                                         loss_function = loss_root_mean_square,
#                                         B = 10, # number of permutations
#                                         type = "variable_importance")
# 
# imp_arousal_study2 = DALEX::model_parts(explainer = rr_exp_arousal_study2,
#                                         loss_function = loss_root_mean_square,
#                                         B = 10, # number of permutations
#                                         type = "variable_importance")

# ## check feature effects for important features: pdp and ice plot
# 
# # loudness
# effect = FeatureEffect$new(predictor_egemaps_arousal_study1, feature = "loudness_sma3_percentile20.0",
#                            method = "pdp+ice")
# effect$plot()
# 
# pdp_loudness = FeatureEffect$new(predictor_egemaps_arousal_study1, feature = "loudness_sma3_stddevNorm",
#                                      method = "pdp+ice")
# pdp_loudness$plot()
# 
# 
# # spectral flux
# pdp_spectralflux = FeatureEffect$new(predictor_egemaps_arousal_study1, feature = "spectralFluxUV_sma3nz_amean",
#                            method = "pdp+ice")
# pdp_spectralflux$plot()
# 
# # pitch
# pdp_pitch = FeatureEffect$new(predictor_egemaps_arousal_study1, feature = "F0semitoneFrom27.5Hz_sma3nz_percentile50.0",
#                                      method = "pdp+ice")
# pdp_pitch$plot()


## create tasks

# #### STUDY 2: WORD EMBEDDINGS ####
# 
# ## create tasks
# 
# # raw contentedness score
# wordembeddings_content = TaskRegr$new(id = "wordembeddings_content", 
#                                       backend = affect_wordembeddings[,c(which(colnames(affect_wordembeddings)=="content"), 
#                                                                          which(colnames(affect_wordembeddings)=="Dim1"):which(colnames(affect_wordembeddings)=="Dim1024"))], 
#                                       target = "content")
# 
# # raw sadness score
# wordembeddings_sad = TaskRegr$new(id = "wordembeddings_sad", 
#                                   backend = affect_wordembeddings[,c(which(colnames(affect_wordembeddings)=="sad"), 
#                                                                      which(colnames(affect_wordembeddings)=="Dim1"):which(colnames(affect_wordembeddings)=="Dim1024"))], 
#                                   target = "sad")
# 
# # raw arousal score
# wordembeddings_arousal = TaskRegr$new(id = "wordembeddings_arousal", 
#                                       backend = affect_wordembeddings[,c(which(colnames(affect_wordembeddings)=="arousal"), 
#                                                                          which(colnames(affect_wordembeddings)=="Dim1"):which(colnames(affect_wordembeddings)=="Dim1024"))], 
#                                       target = "arousal")
# 
# ## create learner
# 
# lrn_rr = lrn("regr.cv_glmnet") # lasso
# 
# # enable parallelization
# set_threads(lrn_rr, n = detectCores())
# 
# ## train models 
# 
# # contentedness
# future::plan("multisession", workers = detectCores()) # enable parallelization
# 
# model_rr_wordembeddings_content <- lrn_rr$train(wordembeddings_content) # train model
# 
# saveRDS(model_rr_wordembeddings_content, "results/study2/model_rr_wordembeddings_content.RData") # save trained models
# 
# # sadness
# future::plan("multisession", workers = detectCores()) # enable parallelization
# 
# model_rr_wordembeddings_sad <- lrn_rr$train(wordembeddings_sad) # train model
# 
# saveRDS(model_rr_wordembeddings_sad, "results/study2/model_rr_wordembeddings_sad.RData") # save trained models
# 
# # arousal
# future::plan("multisession", workers = detectCores()) # enable parallelization
# 
# model_rr_wordembeddings_arousal <- lrn_rr$train(wordembeddings_arousal) # train model
# 
# saveRDS(model_rr_wordembeddings_arousal, "results/study2/model_rr_wordembeddings_arousal.RData") # save trained models
# 
# # load models
# model_rr_wordembeddings_content <- readRDS( "results/study2/model_rr_wordembeddings_content.RData")
# model_rr_wordembeddings_sad <- readRDS( "results/study2/model_rr_wordembeddings_sad.RData")
# model_rr_wordembeddings_arousal <- readRDS( "results/study2/model_rr_wordembeddings_arousal.RData")
# 
# # create explainers
# exp_rr_wordembeddings_content <- explain_mlr3(
#   model =model_rr_wordembeddings_content,
#   data     = affect_wordembeddings[,c(which(colnames(affect_wordembeddings)=="Dim1"):which(colnames(affect_wordembeddings)=="Dim1024"))], 
#   y        = affect_wordembeddings$content,
#   label    = "LASSO",
#   colorize = FALSE)
# 
# exp_rr_wordembeddings_sad <- explain_mlr3(
#   model =model_rr_wordembeddings_sad,
#   data     = affect_wordembeddings[,c(which(colnames(affect_wordembeddings)=="Dim1"):which(colnames(affect_wordembeddings)=="Dim1024"))], 
#   y        = affect_wordembeddings$sad,
#   label    = "LASSO",
#   colorize = FALSE)
# 
# exp_rr_wordembeddings_arousal <- explain_mlr3(
#   model =model_rr_wordembeddings_arousal,
#   data     = affect_wordembeddings[,c(which(colnames(affect_wordembeddings)=="Dim1"):which(colnames(affect_wordembeddings)=="Dim1024"))], 
#   y        = affect_wordembeddings$arousal,
#   label    = "LASSO",
#   colorize = FALSE)
# 
# 
# # compute permutation feature importance
# importance_rr_wordembeddings_content <- DALEX::model_parts(explainer = exp_rr_wordembeddings_content)
# importance_rr_wordembeddings_sad <- DALEX::model_parts(explainer = exp_rr_wordembeddings_sad)
# importance_rr_wordembeddings_arousal <- DALEX::model_parts(explainer = exp_rr_wordembeddings_arousal)
# 
# # save results
# saveRDS(importance_rr_wordembeddings_content, "results/study2/importance_rr_wordembeddings_content.RData")
# saveRDS(importance_rr_wordembeddings_sad, "results/study2/importance_rr_wordembeddings_sad.RData")
# saveRDS(importance_rr_wordembeddings_arousal, "results/study2/importance_rr_wordembeddings_arousal.RData")
# 
# # check which words score high on given layers?
# 
# # load wordembeddings
# wordembeddings_robertalarge <- readRDS("data/study2/wordembeddings_robertalarge.RData")
# 
# ## contentedness
# 
# # check most important single words for important dimensions
# dim333 <- wordembeddings$singlewords_we %>%
#   filter(n > 10) %>%
#   arrange(Dim333)
# 
# dim136 <- wordembeddings$singlewords_we %>%
#   filter(n > 25) %>%
#   arrange(desc(abs(Dim136))) # arrange in absolute descending order
# 
# dim136$Dim136
# 
# head(dim136$words, n =10)
# 
# ## sadness
# 
# ## finish