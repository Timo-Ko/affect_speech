### Resampled mixed-model

# sample code: https://rstudio-pubs-static.s3.amazonaws.com/241926_6490553c521743529a1d64607be948c3.html
# great source: https://avehtari.github.io/modelselection/rats_kcv.html


# challenge: 
# we want random effect for person
# we want regaularization

# load packages

library(foreign)
library(tidyverse)
require(compiler)
require(parallel)
require(boot)
require(lme4)

# load data


# read in data frames
affect_egemaps  <- readRDS("data/affect_egemaps.RData")

# apply PCA beforehand to reduce number of features for hlm?

# get cv fold for 10 fold-cv instances from mlr3 to make results comparable

# add folds column to df

# create empty df for results

hlm_results_valence <-df%>%mutate(Fold=rep(0,nrow(df)),holdoutpred=rep(0,nrow(df)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))

hlm_results_arousal

# write for loop

for(i in 1:10){
  train=temp[fold$subsets[fold$which != i], ]
  validation=temp[fold$subsets[fold$which == i], ]
  newlm=lme4::lmer(formula=Hdlchol~treatment*week+(1|user_id),data=train) # this is the hlm formula
  newpred=predict(newlm,newdata=validation)
  true=validation$Hdlchol
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  temp[fold$subsets[fold$which == i], ]$holdoutpred <- newpred
  temp[fold$subsets[fold$which == i], ]$RMSE=rmse
  temp[fold$subsets[fold$which == i], ]$MSE=mse
  temp[fold$subsets[fold$which == i], ]$MAE=mae
  temp[fold$subsets[fold$which == i], ]$R2=R2
  temp[fold$subsets[fold$which == i], ]$AIC=AIC(newlm)
  temp[fold$subsets[fold$which == i], ]$BIC=BIC(newlm)
  temp[fold$subsets[fold$which == i], ]$Fold=i
}


# view results

temp


