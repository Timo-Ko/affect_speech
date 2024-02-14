### PREPARATION ####

# Install and load required packages 

packages <- c("dplyr", "data.table", "psych","ggplot2", "ggcorrplot", "ggrepel", "tidyr", "tibble", "patchwork")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

source("r_code/functions/filter_corrs.R")

# read in data frame
affect_acoustics  <- readRDS("data/affect_acoustics.RData")

## compute corrs of targets w egemaps voice features

cor_egemaps_affect <- psych::corr.test(affect_acoustics[,c(which(colnames(affect_acoustics)=="content"), 
                                                         which(colnames(affect_acoustics)=="diff_content"),
                                                         which(colnames(affect_acoustics)=="sad"),
                                                         which(colnames(affect_acoustics)=="diff_sad"),
                                                         which(colnames(affect_acoustics)=="arousal"), 
                                                         which(colnames(affect_acoustics)=="diff_arousal"), 
                                                         which(colnames(affect_acoustics)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_acoustics)=="equivalentSoundLevel_dBp"))],  
                                       use="pairwise.complete.obs", 
                                       method = "pearson")

# filter corrs

targets <- c("content", "diff_content", "sad", "diff_sad", "arousal", "diff_arousal") # get specific col names
cor_egemaps_affect_filtered <- filter_corrs(cor_egemaps_affect, targets) # filter correlations

# create plot 

cor_egemaps_affect_plot_1 = ggcorrplot(cor_egemaps_affect_filtered[,c(1:(round(ncol(cor_egemaps_affect_filtered)/2)))], 
                                       method = "square", 
                                       legend.title = "Pearson\ncorrelation", 
                                       lab = TRUE, 
                                       lab_size = 3, 
                                       ggtheme = theme_minimal, 
                                       outline.color = "lightgray",
                                       colors = c("#6D9EC1", "white", "#E46726")) +  
  theme(axis.text.x = ggplot2::element_text(size = 12),
        axis.text.y = ggplot2::element_text(size = 12)) +
  scale_x_discrete(labels=c('Contentedness', 'Contentedness Fluct.', 'Sadness', 'Sadness Fluct.', 'Arousal', 'Arousal Fluct.')) 


cor_egemaps_affect_plot_2 = ggcorrplot(cor_egemaps_affect_filtered[,c((round(ncol(cor_egemaps_affect_filtered)/2)):ncol(cor_egemaps_affect_filtered))], 
                                       method = "square", 
                                       legend.title = "Pearson\ncorrelation", 
                                       lab = TRUE, 
                                       lab_size = 3, 
                                       ggtheme = theme_minimal, 
                                       outline.color = "lightgray",
                                       colors = c("#6D9EC1", "white", "#E46726")) +  
  theme(axis.text.x = ggplot2::element_text(size = 12),
        axis.text.y = ggplot2::element_text(size = 12)) +
  scale_x_discrete(labels=c('Contentedness', 'Contentedness Fluct.', 'Sadness', 'Sadness Fluct.', 'Arousal', 'Arousal Fluct.')) 


# save figures

png(file="figures/cor_egemaps_affect_us_plot_1.png",width=750, height=1500)

cor_egemaps_affect_plot_1

dev.off()


png(file="figures/cor_egemaps_affect_us_plot_2.png",width=750, height=1500)

cor_egemaps_affect_plot_2

dev.off()


## idea: computed corrs per feature group

# # compute avg corrs per group
# 
# # read cvs file
# egemaps_feature_groups <- read.csv2("data/egemaps_feature_groups.csv")
# 
# # match w feature groups
# group_corr_df <- as.data.frame(t(cor_egemaps_affect_filtered))
# group_corr_df  <- tibble::rownames_to_column(group_corr_df , "feature")
# group_corr_df$feature <- gsub('[[:punct:] ]+','',group_corr_df$feature)
# # remove all punctuation
# 
# # merge
# group_corr_df2  <- left_join(group_corr_df,egemaps_feature_groups, by = "feature")
# 
# group_corr_df2 %>% group_by(parameter_group) %>% summarise(across(everything(), mean))
# 
# group_corr_df$parameter_group



### DISTRIBUTION OF AFFECT OUTCOMES ACROSS DATA SETS ####

hist_content <- ggplot(affect_acoustics, aes(x=content)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,10000))+ 
  scale_x_continuous(name = "Contentedness", breaks = c(0:3)) + 
  theme_minimal(base_size = 20)

hist_diff_content <- ggplot(affect_acoustics, aes(x=diff_content)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,10000))+ 
  scale_x_continuous(name = "Contentedness Fluctuation", seq(-3, 3, 1)) + 
  theme_minimal(base_size = 20)

hist_sad <- ggplot(affect_acoustics, aes(x=sad)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,10000))+ 
  scale_x_continuous(name = "Sadness", breaks = c(0:3)) + 
  theme_minimal(base_size = 20)

hist_diff_sad <- ggplot(affect_acoustics, aes(x=diff_sad)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,10000))+ 
  scale_x_continuous(name = "Sadness Fluctuation", seq(-3, 3, 1)) + 
  theme_minimal(base_size = 20)

hist_arousal <- ggplot(affect_acoustics, aes(x=arousal)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,10000))+ 
  scale_x_continuous(name = "Arousal", breaks = c(0:4)) + 
  theme_minimal(base_size = 20)

hist_diff_arousal <- ggplot(affect_acoustics, aes(x=diff_arousal)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,10000))+ 
  scale_x_continuous(name = "Arousal Fluctuation", seq(-4, 4, 1)) + 
  theme_minimal(base_size = 20)

# arrange histograms (2x2 matrix)

affect_dist_voice <- 
  (hist_content + hist_diff_content) /
  (hist_sad + hist_diff_sad) /
  (hist_arousal + hist_diff_arousal)

# save figure
png(file="figures/us_affect_dist_overview.png",width=1000, height=1500)
affect_dist_voice
dev.off()

## FINISH


## FINISH





## old code




# # then keep all in corr matrix that are TRUE
# 
# # only keep those COLUMNS where 0 is not in the CI for ANY of the targets
# 
# upper_ci_smaller_zero <- as.data.frame(upper_ci)  %>%
#   filter(if_all(everything(), ~ . < 0)) # filter out features where all upper ci bounds are smaller than zero
# 
# # different approach: use provided CIS and match w r table
# 
# # create new column from rownames
# cor_egemaps_affect_ci <- setDT(cor_egemaps_affect$ci, keep.rownames = "targets")
# 
# cor_egemaps_affect_ci_filtered <- cor_egemaps_affect$ci %>%
#   filter() %>% # filter out
#   filter(lower > 0 & upper > 0  | lower < 0 & upper < 0 )
# 
# dplyr::filter(df, !grepl("RTB",TrackingPixel))
# 
# # here we want to filter columns and not rows!!! this requires a different approach
# 
# as.data.frame(upper_ci)%>%
#   select_if(function(.) ~ . > 0)
# 
# as.data.frame(upper_ci) %>% select(where(function(x) x < 0))
# 
# abc = as.data.frame(upper_ci) %>% select_if(~max(.)>0) # this on works!!!!! no
# 
# 
# upper_ci_larger_zero <- as.data.frame(upper_ci)  %>%
#   filter(if_all(everything(), ~ . > 0))
# 
# lower_ci_smaller_zero <- as.data.frame(lower_ci)  %>%
#   filter(if_any(everything(), ~ . < 0))
# 
# lower_ci_larger_zero <- as.data.frame(lower_ci)  %>%
#   filter(if_any(everything(), ~ . > 0))
# 
# # filter corr data frame based on these conditions
# 
# 
# # write function that filters out features with 0 not in CI
# 
# cor_egemaps_affect_ci_filtered <- cor_egemaps_affect$ci %>%
#   filter(lower > 0 & upper > 0  | lower < 0 & upper < 0 )
# 
# 
# # filter these new matrices
# cor_egemaps_affect_r_filtered <- cor_egemaps_affect$r %>%
#   filter(lower_ci > 0 & upper_ci > 0  | lower_ci < 0 & upper_ci < 0 )
# 
# upper_ci_smaller_zero <- as.data.frame(upper_ci) %>% filter_all( any_vars(. > 0))
# 
# 
# # test it
# df <- as.data.frame(c(-1,0,1))  %>%
#   filter(if_any(everything(), ~ . < 0))
# 
# # data %>%
# #   filter(across(starts_with("cp"), ~ . > .2))
# 
# 
# head(abc)
# 
# dim(upper_ci_smaller_zero )
# 
# ?filter
# 
# 
# upper_ci_larger_zero
# 
# lower_ci_smaller_zero
# lower_ci_larger_zero
# 
# # filter these new matrices
# cor_egemaps_affect_ci_filtered <- cor_egemaps_affect$r %>%
#   filter(lower_ci > 0 & upper_ci > 0  | lower_ci < 0 & upper_ci < 0 )
# 
# sub_matrix <- r_matrix[targets, features] # subset matrix with all correlations 
# 
# 
# 
# 
# cor_egemaps_affect_filtered <- cor_egemaps_affect$r %>%
#   filter(lower > 0 & upper > 0  | lower < 0 & upper < 0 )
# 
# # or just compute cis using the se and then filter?
# 
# # filter correlations 
# test <- cor_egemaps_affect$r 
# 
# 
# # filter out correlations where 0 is not in the CI for any of the targets, big overlap with keyboard project!
# # or significant (adjusted) correlations?
# 
# r_matrix <- cor_egemaps_affect$r # get corr coefficients
# 
# all_pmat <- cor_egemaps_affect$p  # get p values
# 
# targets <- c("valence", "diff_valence", "arousal", "diff_arousal") # get specific col names
# 
# '%ni%' <- Negate('%in%')
# 
# features <- colnames(r_matrix)[colnames(r_matrix) %ni% targets]
# 
# sub_matrix <- r_matrix[targets, features] # subset matrix with all correlations 
# 
# # select only top correlations / sign correlations (otherwise it's too many)
# # or only show correlation from selected feature groups based on lit?
# 
# sub_pmat <- all_pmat[targets, features] # subset matrix with all adj p values 
