#' @param path_to_files a path to a folder containing keyboard data from users per time window 
#' @param window_identifier a variable in the dfs that identifies the time window (user_uuid, wave_id, week, date, e_s_questionnaire_id) 
#' @param min_words integer indicating the minimum number of words per time window
#' @param filter_var indicate if a app/ action filter should be applied ("all", "private", "public")
#' @return df_results - a data frame with language features with one row per user per time window
#' @example compute_language_features("data/user_df_emojisent/", user_uuid, 1000)

## Extract results from single iterations across all learners
# bmr is the benchmark result
# mes are the performance measures 


extract_bmr_results = function(bmr, mes){
  
  df = as.data.frame(bmr$aggregate(mes)) # get aggregated performance across folds
  uni.learner = unique(df$learner_id) # get unique learner ids
  uni.task = unique(df$task_id) # get unique task ids
  df_results = data.frame() # create empty df with results
  
  for(i in 1:length(uni.task)){ #iterate through task ids
    
    ## standard fl, lasso, rf
    df_helper = as.data.frame(bmr$aggregate(mes)[learner_id == uni.learner[1]]$resample_result[[i]]$score(mes)) # fl
    df_helper = rbind(df_helper, bmr$aggregate(mes)[learner_id == uni.learner[2]]$resample_result[[i]]$score(mes)) # lasso
    df_helper = rbind(df_helper, bmr$aggregate(mes)[learner_id == uni.learner[3]]$resample_result[[i]]$score(mes)) # rf
    
    df_results = rbind(df_results, df_helper)
    
    }
  
  return(df_results) # return df results data frame 
}


## create summary table with all performance measures and p values

results_table = function(data, bmr_results){
  
  n = nrow(data) # get n from data for t tests
  uni.learner = unique(bmr_results$learner_id) # get the unique learner ids
  uni.tasks = unique(bmr_results$task_id) # get the unique task ids
  
  # create empty results table that is to be filled
  # results.table = data.frame(target = uni.tasks,
  #                            learner = uni.learner,
  #                            #fl
  #                            M_rsq_FL = rep(NA, length(uni.tasks)), 
  #                            SD_rsq_FL = rep(NA, length(uni.tasks)), 
  #                            #lasso
  #                            M_rsq_LASSO = rep(NA, length(uni.tasks)), 
  #                            SD_rsq_LASSO = rep(NA, length(uni.tasks)), 
  #                            p_rsq_LASSO = rep(NA, length(uni.tasks)), 
  #                            p_rsq_LASSO_corrected = rep(NA, length(uni.tasks)),
  #                            #rf
  #                            M_rsq_RF = rep(NA, length(uni.tasks)), 
  #                            SD_rsq_RF = rep(NA, length(uni.tasks)), 
  #                            p_rsq_RF = rep(NA, length(uni.tasks)), 
  #                            p_rsq_RF_corrected = rep(NA, length(uni.tasks))
  #                            )
 
  # results.table = data.frame(task_id = rep(uni.tasks, length(uni.learner)),
  #                            learner_id = rep(uni.learner,  each = length(uni.tasks)),
  #                            
  #                            M_rsq_fl = rep(NA, length(uni.tasks)), SD_rsq_fl = rep(NA, length(uni.tasks)), p_rsq_fl = rep(NA, length(uni.tasks)), p_rsq_corrected_fl = rep(NA, length(uni.tasks)),
  #                            
  #                            M_rsq_lasso = rep(NA, length(uni.tasks)), SD_rsq_lasso = rep(NA, length(uni.tasks)), p_rsq_lasso = rep(NA, length(uni.tasks)), p_rsq_corrected_lasso = rep(NA, length(uni.tasks)),
  #                            M_r_lasso = rep(NA, length(uni.tasks)), SD_r_lasso = rep(NA, length(uni.tasks)), p_r_lasso = rep(NA, length(uni.tasks)), p_r_corrected_lasso = rep(NA, length(uni.tasks)),
  #                            
  #                            M_rsq_rf = rep(NA, length(uni.tasks)), SD_rsq_rf = rep(NA, length(uni.tasks)), p_rsq_rf = rep(NA, length(uni.tasks)), p_rsq_corrected_rf = rep(NA, length(uni.tasks)),
  #                            M_r_rf = rep(NA, length(uni.tasks)), SD_r_rf = rep(NA, length(uni.tasks)), p_r_rf = rep(NA, length(uni.tasks)), p_r_corrected_rf = rep(NA, length(uni.tasks))
  # )  
   
  results.table = data.frame(task_id = rep(uni.tasks, length(uni.learner)),
                             learner_id = rep(uni.learner,  each = length(uni.tasks)),

                             M_rsq = rep(NA, length(uni.tasks)*length(uni.learner)), 
                             SD_rsq = rep(NA, length(uni.tasks)*length(uni.learner)), 
                             p_rsq = rep(NA, length(uni.tasks)*length(uni.learner)), 
                             p_rsq_corrected = rep(NA, length(uni.tasks)*length(uni.learner))#,
                             
                             # M_srho = rep(NA, length(uni.tasks)*length(uni.learner)), 
                             # SD_srho = rep(NA, length(uni.tasks)*length(uni.learner)), 
                             # p_srho = rep(NA, length(uni.tasks)*length(uni.learner)), 
                             # p_srho_corrected = rep(NA, length(uni.tasks)*length(uni.learner))
                            )
  
  for(uni.task in uni.tasks){ #iterate through single tasks and fill results table
    df_test = bmr_results %>% dplyr::filter(task_id == uni.task) # filter out rows for that task
    
    ### featureless learner
    results.table$M_rsq[results.table$task_id == uni.task & results.table$learner_id == uni.learner[1]] = df_test %>% dplyr::filter(learner_id == uni.learner[1]) %>% summarise(value = mean(regr.rsq, na.rm = TRUE)) %>% pull(value)
    results.table$SD_rsq[results.table$task_id == uni.task & results.table$learner_id == uni.learner[1]] = df_test %>% dplyr::filter(learner_id == uni.learner[1]) %>% summarise(value = sd(regr.rsq, na.rm = TRUE)) %>% pull(value)
    
    ### featureless vs Lasso 
    results.table$M_rsq[results.table$task_id == uni.task & results.table$learner_id == uni.learner[3]] = df_test %>% dplyr::filter(learner_id == uni.learner[3]) %>% summarise(value = mean(regr.rsq, na.rm = TRUE)) %>% pull(value)
    results.table$SD_rsq[results.table$task_id == uni.task & results.table$learner_id == uni.learner[3]] = df_test %>% dplyr::filter(learner_id == uni.learner[3]) %>% summarise(value = sd(regr.rsq, na.rm = TRUE)) %>% pull(value)
    results.table$p_rsq[results.table$task_id == uni.task & results.table$learner_id == uni.learner[3]] = sign_test_folds(df_test$regr.rsq[df_test$learner_id == uni.learner[[1]]], df_test$regr.rsq[df_test$learner_id == uni.learner[[3]]], n)
    results.table$p_rsq_corrected[results.table$task_id == uni.task & results.table$learner_id == uni.learner[3]] = p.adjust(c(results.table$p_rsq[results.table$task_id == uni.task & results.table$learner_id == uni.learner[3]]), n = 8, method = "holm")
    
    # results.table$M_srho[results.table$task_id == uni.task & results.table$learner_id == uni.learner[3]] = df_test %>% dplyr::filter(learner_id == uni.learner[3]) %>% summarise(value = mean(regr.srho, na.rm = TRUE)) %>% pull(value)
    # results.table$SD_srho[results.table$task_id == uni.task & results.table$learner_id == uni.learner[3]] = df_test %>% dplyr::filter(learner_id == uni.learner[3]) %>% summarise(value = sd(regr.srho, na.rm = TRUE)) %>% pull(value)
    # results.table$p_srho[results.table$task_id == uni.task & results.table$learner_id == uni.learner[3]] = sign_test_folds(rep(0, length(df_test$regr.srho[df_test$learner_id == uni.learner[[1]]])), df_test$regr.srho[df_test$learner_id == uni.learner[[3]]], n)
    # results.table$p_srho_corrected[results.table$task_id == uni.task & results.table$learner_id == uni.learner[3]] = p.adjust(c(results.table$p_r[results.table$task_id == uni.task & results.table$learner_id == uni.learner[3]]), n = 8, method = "holm")
    # 
    ### featureless vs random forest 
    results.table$M_rsq[results.table$task_id == uni.task & results.table$learner_id == uni.learner[2]] = df_test %>% dplyr::filter(learner_id == uni.learner[[2]]) %>% summarise(value = mean(regr.rsq, na.rm = TRUE)) %>% pull(value)
    results.table$SD_rsq[results.table$task_id == uni.task & results.table$learner_id == uni.learner[2]] = df_test %>% dplyr::filter(learner_id == uni.learner[[2]]) %>% summarise(value = sd(regr.rsq, na.rm = TRUE)) %>% pull(value)
    results.table$p_rsq[results.table$task_id == uni.task & results.table$learner_id == uni.learner[2]] = sign_test_folds(df_test$regr.rsq[df_test$learner_id == uni.learner[[1]]], df_test$regr.rsq[df_test$learner_id == uni.learner[[2]]], n)
    results.table$p_rsq_corrected[results.table$task_id == uni.task & results.table$learner_id == uni.learner[2]] = p.adjust(c(results.table$p_rsq[results.table$task_id == uni.task & results.table$learner_id == uni.learner[2]]), n = 8, method = "holm")
    
    # results.table$M_srho[results.table$task_id == uni.task & results.table$learner_id == uni.learner[2]] = df_test %>% dplyr::filter(learner_id == uni.learner[[2]]) %>% summarise(value = mean(regr.srho, na.rm = TRUE)) %>% pull(value)
    # results.table$SD_srho[results.table$task_id == uni.task & results.table$learner_id == uni.learner[2]] = df_test %>% dplyr::filter(learner_id == uni.learner[[2]]) %>% summarise(value = sd(regr.srho, na.rm = TRUE)) %>% pull(value)
    # results.table$p_srho[results.table$task_id == uni.task & results.table$learner_id == uni.learner[2]] = sign_test_folds(rep(0, length(df_test$regr.srho[df_test$learner_id == uni.learner[[1]]])), df_test$regr.srho[df_test$learner_id == uni.learner[[2]]], n)
    # results.table$p_srho_corrected[results.table$task_id == uni.task & results.table$learner_id == uni.learner[2]] = p.adjust(c(results.table$p_r[results.table$task_id == uni.task & results.table$learner_id == uni.learner[2]]), n = 8, method = "holm")
    # 
   
      }
  
  results.table[,3:ncol(results.table)] = apply(results.table[,3:ncol(results.table)], 2, function(x) round(x, 3)) # round results
  
  return(results.table)
}



# function to create plot showing prediction across folds

results_plot = function(bmr_results, results_table){

  uni.tasks = unique(bmr_results$task_id) # get the unique task ids
  
  # add column with p values
  #bmr_results <- base::merge(bmr_results, results_table[,c("target", "learner", "p_rsq", "p_rsq_corrected")], by = c("task_id" == "target", "learner_id" == "learner"))
  bmr_results <- dplyr::left_join(bmr_results, results_table[,c("task_id", "learner_id", "p_rsq", "p_rsq_corrected")], by = c("task_id", "learner_id"))
  
  # create significance column
  bmr_results$significance <- as.factor(ifelse(bmr_results$p_rsq_corrected >= 0.05 | is.na(bmr_results$p_rsq_corrected), "no", "yes"))
  
  # rename variables
  bmr_results <- bmr_results %>% 
    mutate(learner_id = case_when(
      learner_id == "regr.featureless" ~    "Baseline",
      learner_id == "scale.imputemedian.regr.ranger" ~ "Random Forest",
      learner_id == "scale.imputemedian.regr.cv_glmnet" ~ "LASSO")) %>% 
    mutate(task_id = case_when(
      task_id == "keyboardlanguage_pa" ~    "Positive Affect",
      task_id == "keyboardlanguage_na" ~ "Negative Affect"))
  
  # create figure
  bmr_plot <- ggplot(bmr_results, aes(x= task_id , y= regr.rsq, color = significance, shape = learner_id)) + 
    geom_boxplot(width = 0.3,lwd = 1, aes(color = significance), alpha = 0.3, outlier.shape=NA, position=position_dodge(0.5)) +  
    geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5), size = 2) +
    scale_x_discrete(element_blank()) +
    scale_y_continuous(name = bquote("R"^2), limits = c(-0.15, 0.15)) + 
    theme_minimal(base_size = 20) +
    labs(title = "Prediction performance across cv folds") + # add title
    labs(colour = "Significance", shape = "Algorithm") + # change legend title
    # scale_shape_manual(name = "Algorithm",
    #                    labels = c("Baseline", "LASSO", "Random Forest"),
    #                    values = c(17, 18, 19)) + 
    #guides(shape = guide_legend(override.aes = list(size = 2))) +
    #scale_shape_manual(values = c(15,16,17)) + #add shapes for different learners
    theme(axis.text.x=element_text(angle = -45, hjust = 0)) # rotate x axis labels
  
  # save figure
  
  png(file=paste0("figures/bmr_", paste0(uni.tasks),"_plot.png"),width=700, height=500)
  bmr_plot
  dev.off()
  
}
