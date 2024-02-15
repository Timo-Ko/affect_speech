### PREPARATION ####

# Install and load required packages 

packages <- c("text", "dplyr", "tm", "tidytext", "stopwords")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# read in data 
affect_wordembeddings  <- readRDS("data/affect_wordembeddings.RData")

# read in wordembeddings
wordembeddings_robertalarge <- readRDS("data/wordembeddings_robertalarge.RData")


### CREATE & PLOT PROJECTIONS ####

# prepare the text

# # tokenize corpus
# chat_text_tokens <- udpipe(data.frame(doc_id = chat_corpus[["MsgNr"]], 
#                                       text = chat_corpus[["Msg"]]), 
#                            parallel.cores = 20,
#                            object = "german")


# # Create stopword dictionaries from tm r package
stop_words <- tibble(
  token = stopwords("en", source ="stopwords-iso"),
  lexicon = "tm"
)

stop_words <- c("and", "but")
# 
# # remove stopwords
# tf_words_msg_stopremoved  <- tf_words_msg %>%
#   anti_join(stop_words)



## create projections

# for content x arousal 
projection_content_arousal <- textProjection(
                                  words = affect_acoustics$Text, 
                                  word_embeddings = wordembeddings_robertalarge$x,
                                  single_word_embeddings = wordembeddings_robertalarge$singlewords_we,
                                  x = affect_wordembeddings$content, 
                                  y = affect_wordembeddings$arousal
)

test_data %>%
  unnest_tokens(review, review) %>%
  anti_join(stop_words, by= c("review" = "word"))


# for sad x arousal
projection_sad_arousal <- textProjection(
  words = affect_acoustics$Text, 
  word_embeddings = wordembeddings_robertalarge$x,
  single_word_embeddings = wordembeddings_robertalarge$singlewords_we,
  x = affect_wordembeddings$sad, 
  y = affect_wordembeddings$arousal
)

## filter projections for stopwords
# make a better selection of words!!

projection_content_arousal$word_data <- projection_content_arousal$word_data %>%
  filter(!words %in% stopwords::stopwords())


projection_sad_arousal$word_data <- projection_sad_arousal$word_data %>%
  filter(!words %in% stopwords::stopwords())


## create plots

# for content x arousal

plot_projection_content_arousal <- textProjectionPlot(
  word_data = projection_content_arousal,
  k_n_words_to_test = FALSE,
  min_freq_words_plot = 5,
  plot_n_words_square = 5,
  plot_n_words_p = 5,
  plot_n_word_extreme = 1,
  plot_n_word_frequency = 1,
  plot_n_words_middle = 1,
  y_axes = TRUE,
  p_alpha = 0.05,
  title_top = " Supervised Bicentroid Projection of Affect Words",
  x_axes_label = "Low vs. High contentedness score",
  y_axes_label = "Low vs. High arousal score",
  p_adjust_method = "holm",
  points_without_words_size = 0.4,
  points_without_words_alpha = 0.4
)

plot_projection_content_arousal

# for sad x arousal

plot_projection_sad_arousal <- textProjectionPlot(
  word_data = projection_sad_arousal,
  k_n_words_to_test = T,
  min_freq_words_plot = 100,
  plot_n_words_square = 5,
  plot_n_words_p = 5,
  plot_n_word_extreme = 10,
  plot_n_word_frequency = 5,
  plot_n_words_middle = 1,
  y_axes = TRUE,
  p_alpha = 0.05,
  title_top = " Supervised Bicentroid Projection of Affect Words",
  x_axes_label = "Low vs. High sadness score",
  y_axes_label = "Low vs. High arousal score",
  p_adjust_method = "holm",
  points_without_words_size = 0.4,
  points_without_words_alpha = 0.4
)

plot_projection_sad_arousal

# do some testing
plot_projection <- textProjectionPlot(
  word_data = projection_content_arousal,
  min_freq_words_plot = 100,
  y_axes = TRUE,
  title_top = " Supervised Bicentroid Projection of Affect words",
  x_axes_label = "Low vs. High sadness score",
  y_axes_label = "Low vs. High arousal score",
  # position_jitter_hight = 0.5,
  # position_jitter_width = 0.8
)
plot_projection

## FINISH
