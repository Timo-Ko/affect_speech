### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "data.table", "ranger", "ggplot2", "mlr3", "mlr3learners", "parallel", "DALEX", "DALEXtra")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# read in data 
affect_wordembeddings  <- readRDS("data/affect_wordembeddings.RData")

#### TRAIN MODELS ####

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

saveRDS(model_rr_wordembeddings_content, "results/model_rr_wordembeddings_content.RData") # save trained models

# sadness
future::plan("multisession", workers = detectCores()) # enable parallelization

model_rr_wordembeddings_sad <- lrn_rr$train(wordembeddings_sad) # train model

saveRDS(model_rr_wordembeddings_sad, "results/model_rr_wordembeddings_sad.RData") # save trained models

# arousal
future::plan("multisession", workers = detectCores()) # enable parallelization

model_rr_wordembeddings_arousal <- lrn_rr$train(wordembeddings_arousal) # train model

saveRDS(model_rr_wordembeddings_arousal, "results/model_rr_wordembeddings_arousal.RData") # save trained models

#### GET FEATURE IMPORTANCE ####

# load models
model_rr_wordembeddings_content <- readRDS( "results/model_rr_wordembeddings_content.RData")
model_rr_wordembeddings_sad <- readRDS( "results/model_rr_wordembeddings_sad.RData")
model_rr_wordembeddings_arousal <- readRDS( "results/model_rr_wordembeddings_arousal.RData")

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
saveRDS(importance_rr_wordembeddings_content, "results/importance_rr_wordembeddings_content.RData")
saveRDS(importance_rr_wordembeddings_sad, "results/importance_rr_wordembeddings_sad.RData")
saveRDS(importance_rr_wordembeddings_arousal, "results/importance_rr_wordembeddings_arousal.RData")

#### INVESTIGATE WORDS IN IMPORTANT DIMENSIONS ####

# check which words score high on given layers?

# load wordembeddings
wordembeddings_robertalarge <- readRDS("data/wordembeddings_robertalarge.RData")

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