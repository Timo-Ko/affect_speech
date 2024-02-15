### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "data.table", "ranger", "ggplot2", "mlr3", "mlr3learners", "parallel", "mlr3tuning", "DALEX", "DALEXtra")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# read in data 
affect_acoustics  <- readRDS("data/affect_acoustics.RData")

### TRAIN MODELS ####

## create tasks

# # raw contentedness score
# egemaps_content = TaskRegr$new(id = "egemaps_content", 
#                                backend = affect_acoustics[,c(#which(colnames(affect_acoustics)=="user_id"), 
#                                                              which(colnames(affect_acoustics)== "content"), 
#                                                              which(colnames(affect_acoustics)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics)=="equivalentSoundLevel_dBp"))], 
#                                target = "content")

# raw arousal score
egemaps_arousal = TaskRegr$new(id = "egemaps_arousal", 
                               backend = affect_acoustics[,c(#which(colnames(affect_acoustics)=="user_id"), 
                                                             which(colnames(affect_acoustics)== "arousal"), 
                                                             which(colnames(affect_acoustics)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics)=="equivalentSoundLevel_dBp"))], 
                               target = "arousal")

# create rf learner 
lrn_rf = lrn("regr.ranger", num.trees =1000)

## train models 

# # contentedness
# future::plan("multisession", workers = detectCores()) # enable parallelization
# 
# model_rr_egemaps_content <- lrn_rf$train(egemaps_content) # train model
# 
# saveRDS(model_rf_egemaps_content, "results/model_rf_egemaps_content.RData") # save trained models

# arousal
future::plan("multisession", workers = detectCores()) # enable parallelization

model_rf_egemaps_arousal <- lrn_rf$train(egemaps_arousal) # train model

saveRDS(model_rf_egemaps_arousal, "results/model_rf_egemaps_arousal.RData") # save trained models

#### GET FEATURE IMPORTANCE ####

# load models
#model_rf_egemaps_content <- readRDS( "results/model_rr_egemaps_content.RData")
model_rf_egemaps_arousal <- readRDS( "results/model_rf_egemaps_arousal.RData")

# create explainer

# rf_exp_content <- explain_mlr3(model_rf_egemaps_content,
#                                data     = affect_egemaps[,c(which(colnames(affect_egemaps)=="F0semitoneFrom275Hzsma3nzamean"):which(colnames(affect_egemaps)=="equivalentSoundLeveldBp"))],
#                                y        = affect_egemaps$arousal,
#                                label    = "Ranger RF",
#                                colorize = FALSE)

rf_exp_arousal <- explain_mlr3(model_rf_egemaps_arousal,
                               data     = affect_acoustics[,c(which(colnames(affect_acoustics)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics)=="equivalentSoundLevel_dBp"))],
                               y        = affect_acoustics$arousal,
                               label    = "Random Forest",
                               colorize = FALSE)

# compute permutation feature importance
#importance_rf_content <- DALEX::model_parts(explainer = rf_exp_content)
importance_rf_arousal <- DALEX::model_parts(explainer = rf_exp_arousal)

# save results
#saveRDS(importance_rf_content, "results/importance_rf_content.RData")
saveRDS(importance_rf_arousal, "results/importance_rf_arousal.RData")

# create plots

# # contentedness
# 
# importance_rf_content_plot <- plot(importance_rf_content , max_vars = 10, show_boxplots = FALSE)
# 
# png(file="figures/importance_rf_content_plot.png",width=500, height=500)
# 
# importance_rf_content_plot 
# 
# dev.off()

# arousal

importance_rf_arousal_plot <- plot(importance_rf_arousal , max_vars = 5, show_boxplots = FALSE)

png(file="figures/importance_rf_arousal_plot.png",width=750, height=500)

importance_rf_arousal_plot 

dev.off()


## FINISH