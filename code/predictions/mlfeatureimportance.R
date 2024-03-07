### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "data.table", "ranger", "ggplot2", "mlr3", "mlr3learners", "parallel", "mlr3tuning", "stringr", "DALEX", "DALEXtra")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

set.seed(123, kind = "L'Ecuyer") # set seed to make sure all results are reproducible

# load data study 1
affect_egemaps  <- readRDS("data/study1/affect_egemaps.RData")

# load data study 2
affect_egemaps  <- readRDS("data/study2/affect_egemaps.RData")

#### STUDY 1 ####

# remove all punctuation in colnames for ml 
names(affect_egemaps)[16:length(names(affect_egemaps))] <- str_replace_all(names(affect_egemaps)[16:length(names(affect_egemaps))], "[:punct:]", "")

# create prediction task for arousal
egemaps_arousal = TaskRegr$new(id = "egemaps_arousal", 
                               backend = affect_egemaps[,c(#which(colnames(affect_egemaps)=="user_id"), 
                                                           which(colnames(affect_egemaps)=="arousal"),  
                                                           which(colnames(affect_egemaps)=="F0semitoneFrom275Hzsma3nzamean"):which(colnames(affect_egemaps)=="equivalentSoundLeveldBp"))], 
                               target = "arousal")

# ## add blocking - do i need this here?
# 
# # Use participant id column as block factor
# egemaps_arousal$col_roles$group = "user_id"
# 
# # Remove Id from feature space
# egemaps_arousal$col_roles$feature = setdiff(egemaps_arousal$col_roles$feature, "user_id")

# create rf learner 
lrn_rf = lrn("regr.ranger", num.trees =1000)

# enable parallelization
set_threads(lrn_rf, n = 2)

# train model 

future::plan("multisession", workers = 2) # enable parallelization

model_rf_egemaps_arousal <- lrn_rf$train(egemaps_arousal) # train model

saveRDS(model_rf_egemaps_arousal, "results/study1/model_rf_egemaps_arousal.RData") # save trained models

# create explainer
rf_exp_arousal <- explain_mlr3(model_rf_egemaps_arousal,
                               data     = affect_egemaps[,c(which(colnames(affect_egemaps)=="F0semitoneFrom275Hzsma3nzamean"):which(colnames(affect_egemaps)=="equivalentSoundLeveldBp"))],
                               y        = affect_egemaps$arousal,
                               label    = "Random Forest",
                               colorize = FALSE)

# compute permutation importance
importance_rf_arousal <- DALEX::model_parts(explainer = rf_exp_arousal)

# save results
saveRDS(importance_rf_arousal, "results/study1/importance_rf_arousal.RData")

# create plot

importance_rf_arousal_plot <- plot(importance_rf_arousal , max_vars = 5, show_boxplots = T)

png(file="figures/importance_rf_arousal_study1_plot.png",width=750, height=500)

importance_rf_arousal_plot 

dev.off()


## test iml package for viz

install.packages("iml")
library(iml)

predictor = Predictor$new(model = model_rf_egemaps_arousal, 
                          data = affect_egemaps[,c(which(colnames(affect_egemaps)=="F0semitoneFrom275Hzsma3nzamean"):which(colnames(affect_egemaps)=="equivalentSoundLeveldBp"))], 
                          y= affect_egemaps$arousal)

importance = FeatureImp$new(predictor, loss = "rmse", n.repetitions = 5)
imp_plot = importance$plot()

customized_plot <- imp_plot+
  ggtitle("Feature Importance") +
  xlab("Importance") +
  ylab("Features") +
  theme_minimal()



### STUDY 2: VOICE ####

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

saveRDS(model_rf_egemaps_arousal, "results/study2/model_rf_egemaps_arousal.RData") # save trained models

#### GET FEATURE IMPORTANCE ####

# load models
#model_rf_egemaps_content <- readRDS( "results/model_rr_egemaps_content.RData")
model_rf_egemaps_arousal <- readRDS( "results/study2/model_rf_egemaps_arousal.RData")

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
saveRDS(importance_rf_arousal, "results/study2/importance_rf_arousal.RData")

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


#### STUDY 2: EMBEDDINGS ####

## create tasks

# raw contentedness score
wordembeddings_content = TaskRegr$new(id = "wordembeddings_content", 
                                      backend = affect_wordembeddings[,c(which(colnames(affect_wordembeddings)=="content"), 
                                                                         which(colnames(affect_wordembeddings)=="Dim1"):which(colnames(affect_wordembeddings)=="Dim1024"))], 
                                      target = "content")

# raw sadness score
wordembeddings_sad = TaskRegr$new(id = "wordembeddings_sad", 
                                  backend = affect_wordembeddings[,c(which(colnames(affect_wordembeddings)=="sad"), 
                                                                     which(colnames(affect_wordembeddings)=="Dim1"):which(colnames(affect_wordembeddings)=="Dim1024"))], 
                                  target = "sad")

# raw arousal score
wordembeddings_arousal = TaskRegr$new(id = "wordembeddings_arousal", 
                                      backend = affect_wordembeddings[,c(which(colnames(affect_wordembeddings)=="arousal"), 
                                                                         which(colnames(affect_wordembeddings)=="Dim1"):which(colnames(affect_wordembeddings)=="Dim1024"))], 
                                      target = "arousal")

## create learner

lrn_rr = lrn("regr.cv_glmnet") # lasso

# enable parallelization
set_threads(lrn_rr, n = detectCores())

## train models 

# contentedness
future::plan("multisession", workers = detectCores()) # enable parallelization

model_rr_wordembeddings_content <- lrn_rr$train(wordembeddings_content) # train model

saveRDS(model_rr_wordembeddings_content, "results/study2/model_rr_wordembeddings_content.RData") # save trained models

# sadness
future::plan("multisession", workers = detectCores()) # enable parallelization

model_rr_wordembeddings_sad <- lrn_rr$train(wordembeddings_sad) # train model

saveRDS(model_rr_wordembeddings_sad, "results/study2/model_rr_wordembeddings_sad.RData") # save trained models

# arousal
future::plan("multisession", workers = detectCores()) # enable parallelization

model_rr_wordembeddings_arousal <- lrn_rr$train(wordembeddings_arousal) # train model

saveRDS(model_rr_wordembeddings_arousal, "results/study2/model_rr_wordembeddings_arousal.RData") # save trained models

# load models
model_rr_wordembeddings_content <- readRDS( "results/study2/model_rr_wordembeddings_content.RData")
model_rr_wordembeddings_sad <- readRDS( "results/study2/model_rr_wordembeddings_sad.RData")
model_rr_wordembeddings_arousal <- readRDS( "results/study2/model_rr_wordembeddings_arousal.RData")

# create explainers
exp_rr_wordembeddings_content <- explain_mlr3(
  model =model_rr_wordembeddings_content,
  data     = affect_wordembeddings[,c(which(colnames(affect_wordembeddings)=="Dim1"):which(colnames(affect_wordembeddings)=="Dim1024"))], 
  y        = affect_wordembeddings$content,
  label    = "LASSO",
  colorize = FALSE)

exp_rr_wordembeddings_sad <- explain_mlr3(
  model =model_rr_wordembeddings_sad,
  data     = affect_wordembeddings[,c(which(colnames(affect_wordembeddings)=="Dim1"):which(colnames(affect_wordembeddings)=="Dim1024"))], 
  y        = affect_wordembeddings$sad,
  label    = "LASSO",
  colorize = FALSE)

exp_rr_wordembeddings_arousal <- explain_mlr3(
  model =model_rr_wordembeddings_arousal,
  data     = affect_wordembeddings[,c(which(colnames(affect_wordembeddings)=="Dim1"):which(colnames(affect_wordembeddings)=="Dim1024"))], 
  y        = affect_wordembeddings$arousal,
  label    = "LASSO",
  colorize = FALSE)


# compute permutation feature importance
importance_rr_wordembeddings_content <- DALEX::model_parts(explainer = exp_rr_wordembeddings_content)
importance_rr_wordembeddings_sad <- DALEX::model_parts(explainer = exp_rr_wordembeddings_sad)
importance_rr_wordembeddings_arousal <- DALEX::model_parts(explainer = exp_rr_wordembeddings_arousal)

# save results
saveRDS(importance_rr_wordembeddings_content, "results/study2/importance_rr_wordembeddings_content.RData")
saveRDS(importance_rr_wordembeddings_sad, "results/study2/importance_rr_wordembeddings_sad.RData")
saveRDS(importance_rr_wordembeddings_arousal, "results/study2/importance_rr_wordembeddings_arousal.RData")

# check which words score high on given layers?

# load wordembeddings
wordembeddings_robertalarge <- readRDS("data/study2/wordembeddings_robertalarge.RData")

## contentedness

# check most important single words for important dimensions
dim333 <- wordembeddings$singlewords_we %>%
  filter(n > 10) %>%
  arrange(Dim333)

dim136 <- wordembeddings$singlewords_we %>%
  filter(n > 25) %>%
  arrange(desc(abs(Dim136))) # arrange in absolute descending order

dim136$Dim136

head(dim136$words, n =10)

## sadness



## arousal




# FINISH

## finish