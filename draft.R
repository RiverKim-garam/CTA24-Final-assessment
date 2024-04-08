rm(list=ls())

# Data importing
library(ggplot2)
library(plyr)
library(gdata)
library(stringr)
library(data.table)

## Prep Osnabrugge et al.

data = fread("/Users/garamkim/Downloads/dataverse_files/uk_data.csv", encoding="UTF-8")


data$date = as.Date(data$date)


#Table 2: Examples: Emotive and neutral speeches
example1 = subset(data, id_speech==854597)
example1$emotive_rhetoric
example1$text

example2 = subset(data, id_speech==778143)
example2$emotive_rhetoric
example2$text

#Create time variable
data$time= NA
data$time[data$date>=as.Date("2001-01-01") & data$date<=as.Date("2001-06-30")] = "01/1"
data$time[data$date>=as.Date("2001-07-01") & data$date<=as.Date("2001-12-31")] = "01/2"
data$time[data$date>=as.Date("2002-01-01") & data$date<=as.Date("2002-06-30")] = "02/1"
data$time[data$date>=as.Date("2002-07-01") & data$date<=as.Date("2002-12-31")] = "02/2"
data$time[data$date>=as.Date("2003-01-01") & data$date<=as.Date("2003-06-30")] = "03/1"
data$time[data$date>=as.Date("2003-07-01") & data$date<=as.Date("2003-12-31")] = "03/2"
data$time[data$date>=as.Date("2004-01-01") & data$date<=as.Date("2004-06-30")] = "04/1"
data$time[data$date>=as.Date("2004-07-01") & data$date<=as.Date("2004-12-31")] = "04/2"
data$time[data$date>=as.Date("2005-01-01") & data$date<=as.Date("2005-06-30")] = "05/1"
data$time[data$date>=as.Date("2005-07-01") & data$date<=as.Date("2005-12-31")] = "05/2"
data$time[data$date>=as.Date("2006-01-01") & data$date<=as.Date("2006-06-30")] = "06/1"
data$time[data$date>=as.Date("2006-07-01") & data$date<=as.Date("2006-12-31")] = "06/2"
data$time[data$date>=as.Date("2007-01-01") & data$date<=as.Date("2007-06-30")] = "07/1"
data$time[data$date>=as.Date("2007-07-01") & data$date<=as.Date("2007-12-31")] = "07/2"
data$time[data$date>=as.Date("2008-01-01") & data$date<=as.Date("2008-06-30")] = "08/1"
data$time[data$date>=as.Date("2008-07-01") & data$date<=as.Date("2008-12-31")] = "08/2"
data$time[data$date>=as.Date("2009-01-01") & data$date<=as.Date("2009-06-30")] = "09/1"
data$time[data$date>=as.Date("2009-07-01") & data$date<=as.Date("2009-12-31")] = "09/2"
data$time[data$date>=as.Date("2010-01-01") & data$date<=as.Date("2010-06-30")] = "10/1"
data$time[data$date>=as.Date("2010-07-01") & data$date<=as.Date("2010-12-31")] = "10/2"
data$time[data$date>=as.Date("2011-01-01") & data$date<=as.Date("2011-06-30")] = "11/1"
data$time[data$date>=as.Date("2011-07-01") & data$date<=as.Date("2011-12-31")] = "11/2"
data$time[data$date>=as.Date("2012-01-01") & data$date<=as.Date("2012-06-30")] = "12/1"
data$time[data$date>=as.Date("2012-07-01") & data$date<=as.Date("2012-12-31")] = "12/2"
data$time[data$date>=as.Date("2013-01-01") & data$date<=as.Date("2013-06-30")] = "13/1"
data$time[data$date>=as.Date("2013-07-01") & data$date<=as.Date("2013-12-31")] = "13/2"
data$time[data$date>=as.Date("2014-01-01") & data$date<=as.Date("2014-06-30")] = "14/1"
data$time[data$date>=as.Date("2014-07-01") & data$date<=as.Date("2014-12-31")] = "14/2"
data$time[data$date>=as.Date("2015-01-01") & data$date<=as.Date("2015-06-30")] = "15/1"
data$time[data$date>=as.Date("2015-07-01") & data$date<=as.Date("2015-12-31")] = "15/2"
data$time[data$date>=as.Date("2016-01-01") & data$date<=as.Date("2016-06-30")] = "16/1"
data$time[data$date>=as.Date("2016-07-01") & data$date<=as.Date("2016-12-31")] = "16/2"
data$time[data$date>=as.Date("2017-01-01") & data$date<=as.Date("2017-06-30")] = "17/1"
data$time[data$date>=as.Date("2017-07-01") & data$date<=as.Date("2017-12-31")] = "17/2"
data$time[data$date>=as.Date("2018-01-01") & data$date<=as.Date("2018-06-30")] = "18/1"
data$time[data$date>=as.Date("2018-07-01") & data$date<=as.Date("2018-12-31")] = "18/2"
data$time[data$date>=as.Date("2019-01-01") & data$date<=as.Date("2019-06-30")] = "19/1"
data$time[data$date>=as.Date("2019-07-01") & data$date<=as.Date("2019-12-31")] = "19/2"

data$time2 = data$time
data$time2 = str_replace(data$time2, "/", "_")

data$stage = 0
data$stage[data$m_questions==1]= 1
data$stage[data$u_questions==1]= 2
data$stage[data$queen_debate_others==1]= 3
data$stage[data$queen_debate_day1==1]= 4
data$stage[data$pm_questions==1]= 5

library(tidyverse)
library(readr)
library(tidytext)
library(quanteda)
library(textdata)

colnames(data)
data <- data %>%
  select(last_name, first_name, date, female, age, party, text)
head(data)

# Define the keywords to search for
immig_words <- c('immigration', 'immigrant', 'asylum')
visa_words <- "\\b(UK)?visas?\\b"
all_words <- paste0(c(paste0(immig_words, collapse = "|"), visa_words), collapse = "|")

# lower case text with keywords
tidy_data_notoken <- data %>%
  mutate(desc = tolower(text)) %>%
  filter(grepl(all_words, desc))

# Count the number of texts in tidy_data by year and party
tidy_data_notoken$Year <- format(as.Date(tidy_data$date), "%Y")
data$Year <- format(as.Date(data$date), "%Y")

text_count <- tidy_data %>% 
  group_by(Year, party) %>% 
  summarise(n = n(), .groups = 'drop')

text_count$Year <- as.numeric(as.character(text_count$Year))

# simple counts of immigration texts by party
ggplot(text_count, aes(x = Year, y = n, group = party, color = party)) +
  geom_line() +
  labs(y = "Immigration texts num", x = "Year") +
  scale_x_continuous(breaks = seq(min(text_count$Year), max(text_count$Year), by = 4))
  theme_minimal()

# Count the total number of texts in data by year and party
data_count <- data %>%
  group_by(Year, party) %>%
  summarise(total_n = n(), .groups = 'drop')

# Merge the counts and calculate the ratio
ratio_count <- merge(text_count, data_count, by = c("Year", "party"))
ratio_count$ratio <- with(ratio_count, n / total_n)

# more than 100 texts
filtered_ratio_count <- ratio_count %>%
  filter(n > 100)

filtered_ratio_count$Year <- as.numeric(as.character(filtered_ratio_count$Year))

#Plotting the ratios
ggplot(filtered_ratio_count, aes(x = Year, y = ratio, group = party, color = party)) +
  geom_line() +
  labs(y = "% Immigration Texts (n>100)", x = "Year") +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0), limits = c(0, NA)) +
  scale_x_continuous(breaks = seq(min(filtered_ratio_count$Year), max(filtered_ratio_count$Year), by = 4)) +
  theme_minimal()

# tokenisation & removing stop words
tidy_data <- tidy_data %>%
  unnest_tokens(word, desc) %>%
  filter(str_detect(word, "[a-z]")) %>%
  filter(!word %in% stop_words$word)

tidy_data <- tidy_data %>%
  arrange(date)
tidy_data$order <- 1:nrow(tidy_data)

# common tokens?
word_count <- tidy_data %>%
  count(word, sort = T)
# common tokens by year
word_count_year <- tidy_data %>%
  group_by(Year) %>%
  count(word, sort = T)
# coomon tokens by party
word_count_party <- tidy_data %>%
  group_by(party) %>%
  count(word, sort = T)

## Sentiment analysis
#1) bing
get_sentiments("bing")

bing_data <- tidy_data %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(date, index = order %/% 1000, sentiment) %>%
  spread(key = sentiment, value = n, fill = 0) %>%
  mutate(ratio = negative / (positive+1))
  
bing_data %>%
  ggplot(aes(date, ratio)) +
  geom_point(alpha=0.25) +
  geom_smooth(method="loess", alpha=0.25) +
  labs(y = "Bing sentiment negative ratio", x = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2007-09-14")), col="red") +
  annotate("text", x = as.Date("2007-09-24"), y = .0015, label="Northern Rock seeking liquidity support", angle=90, color = "black", size = 4)

#bing sentiment by party
bing_data_party <- tidy_data %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(party, date) %>%
  count(sentiment) %>%
  spread(key = sentiment, value = n, fill = 0) %>%
  mutate(ratio = negative / (positive+1)) %>%
  ungroup()

filtered_bing_data_party <- bing_data_party %>%
  group_by(party) %>%
  filter(n() > 100) %>%
  ungroup()

filtered_bing_data_party %>%
  ggplot(aes(date, ratio, color = party)) +
  geom_point(alpha=0.25) +
  geom_smooth(method="loess", se = T, alpha=0.25, size = 1.5) +
  labs(y = "Bing sentiment negative ratio by party", x = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2007-09-14")), col="red") +
  annotate("text", x = as.Date("2007-09-24"), y = .0015, label="Northern Rock seeking liquidity support", angle=90, color = "black", size = 4) +
  theme(legend.position = "bottom") 

#(FAIL) most common sentiments (counts)
top_words_party <- filtered_bing_data_party %>%
  count(party, sentiment) %>%
  group_by(party) %>%
  slice_max(n, n = 10) %>%
  ungroup()
top_words_party %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = party)) +
  geom_col(show.legend = F) +
  facet_wrap(~ party, scales = "free_y") +
  coord_flip() +
  labs(x = "Frequent Words", y = NULL)
#-------------------------------------------

#NRC
get_sentiments("nrc")

nrc_data <- tidy_data %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  count(date, index = order %/% 1000, sentiment) %>%
  spread(key = sentiment, value = n, fill = 0) %>%
  mutate(ratio = negative / (positive+1))

nrc_data %>%
  ggplot(aes(date, ratio)) +
  geom_point(alpha=0.25) +
  geom_smooth(method="loess", alpha=0.25) +
  labs(y = "NRC sentiment negative ratio", x = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2007-09-14")), col="red") +
  annotate("text", x = as.Date("2007-09-24"), y = .0015, label="Northern Rock seeking liquidity support", angle=90, color = "black", size = 4)

#nrc sentiment by party
nrc_data_party <- tidy_data %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  group_by(party, date) %>%
  count(sentiment) %>%
  spread(key = sentiment, value = n, fill = 0) %>%
  mutate(ratio = negative/(positive+1)) %>%
  ungroup()

filtered_nrc_data_party <-nrc_data_party %>%
  group_by(party) %>%
  filter(n() > 100) %>%
  ungroup()

filtered_nrc_data_party %>%
  ggplot(aes(date, ratio, color = party)) +
  geom_point(alpha=0.25) +
  geom_smooth(method="loess", se = T, alpha=0.5, size = 1.5) +
  labs(y = "NRC sentiment by party", x = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2007-09-14")), col="red") +
  annotate("text", x = as.Date("2007-09-24"), y = .0015, label="Northern Rock seeking liquidity support", angle=90, color = "black", size = 4) +
  theme(legend.position = "bottom") 

filtered_nrc_data_party %>%
  ggplot(aes(date, ratio, color = party)) +
  geom_point(alpha=0.5, size = 1) +
  geom_smooth(method="loess", se = F, alpha=0.7, size = 1) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(y = "NRC Sentiment Negative Ratio by Party", x = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2007-09-14")), col="red", size = 1) +
  annotate("text", x = as.Date("2007-09-24"), y = 4, label="Northern Rock\nLiquidity Support", angle=90, color = "black", size = 4) +
  theme_minimal() +
  theme(legend.position = "bottom")

filtered_nrc_data_party %>%
  ggplot(aes(x = date, y = ratio, color = party)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "loess", se = FALSE, alpha = 0.7, size = 1) +
  labs(y = "NRC Sentiment Negative Ratio", x = "Date") +
  facet_wrap(~party, scales = "free_y") +  
  geom_vline(xintercept = as.numeric(as.Date("2007-09-14")), col = "red", size = 1) +
  theme_minimal() +
  theme(legend.position = "none")


dominant_senti_nrc_party <-filtered_nrc_data_party %>%
  group_by(party) %>%
  summarise(across(c(anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative, positive), sum, na.rm = T))

show(dominant_senti_nrc_party)

# sentiment change by party
long_nrc_data <- filtered_nrc_data_party %>%
  pivot_longer(cols = c(anger, fear, trust, sadness, disgust, anticipation, surprise, joy), names_to = "sentiment", values_to = "score")
  
long_nrc_data %>%
  ggplot(aes(x = date, y = score, color = sentiment)) +
  geom_smooth(method = "loess", se = F, alpha = 1, size = 1) +
  geom_point(alpha = 0.5, size = 0.5) + 
  facet_wrap(~ party, scales = "free_y") +
  labs(x = "Date", y = "Score") +
  theme_minimal() +
  geom_vline(xintercept = as.numeric(as.Date("2007-09-14")), col = "red") +
  theme_minimal() +
  theme(legend.position = "bottom")

shape_mapping <- c("anger" = 17, "disgust" = 18, "fear" = 19, "sadness" = 8, "trust" = 21, "anticipation" = 22, "surprise" = 23, "joy" = 24)

long_nrc_data %>%
  ggplot(aes(x = date, y = score, color = sentiment)) +
  geom_smooth(method = "loess", se = F, alpha = 0.75, size = 0.75) +  
  geom_point(aes(shape = sentiment), alpha = 0.5, size = 0.5) + 
  facet_wrap(~ party, scales = "free_y", nrow = 2) + 
  labs(x = "Date", y = "Score") +
  geom_vline(xintercept = as.numeric(as.Date("2007-09-14")), col = "red") +
  theme_minimal() +
  theme(legend.position = "bottom")

long_nrc_data_neg <- filtered_nrc_data_party %>%
  pivot_longer(cols = c(anger, fear, sadness, disgust), names_to = "sentiment_negative", values_to = "score")

long_nrc_data_neg %>%
  ggplot(aes(x = date, y = score, color = sentiment_negative)) +
  geom_smooth(method = "loess", se = F, alpha = 0.75, size = 1) +  
  geom_point(aes(shape = sentiment_negative), alpha = 0.5, size = 1) + 
  facet_wrap(~ party, scales = "free_y", nrow = 2) + 
  labs(x = "Date", y = "Score") +
  geom_vline(xintercept = as.numeric(as.Date("2007-09-14")), col = "red") +
  theme_minimal() +
  theme(legend.position = "bottom")

#4) random sample sentiment (validation) 
data_sample <- data %>%
  sample_n(10000)

tidy_samp <- data_sample %>%
  mutate(desc = tolower(text)) %>%
  unnest_tokens(word, desc) %>%
  filter(str_detect(word, "[a-z]")) %>%
  filter(!word %in% stop_words$word) %>%
  arrange(date)

tidy_samp$order <- 1:nrow(tidy_samp)

samp_bing_data <- tidy_samp %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(date, index = order %/% 1000, sentiment) %>%
  spread(key = sentiment, value = n, fill = 0) %>%
  mutate(ratio = negative / (positive+1))

samp_bing_party <- tidy_samp %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(party, date) %>%
  count(sentiment) %>%
  spread(key = sentiment, value = n, fill = 0) %>%
  mutate(ratio = negative / (positive+1)) %>%
  ungroup()

filtered_samp_bing_party <- samp_bing_party %>%
  group_by(party) %>%
  filter(n() > 100) %>%
  ungroup()

filtered_samp_bing_party %>%
  ggplot(aes(date, ratio, color = party)) +
  geom_point(alpha=0.25) +
  geom_smooth(method="loess", se = T, alpha=0.25, size = 1.5) +
  labs(y = "Bing sentiment negative ratio by party", x = "Date")

samp_nrc_data <- tidy_samp %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  count(date, index = order %/% 1000, sentiment) %>%
  spread(key = sentiment, value = n, fill = 0) %>%
  mutate(ratio = negative / (positive+1))

samp_nrc_party <- tidy_samp %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  group_by(party, date) %>%
  count(sentiment) %>%
  spread(key = sentiment, value = n, fill = 0) %>%
  mutate(ratio = negative / (positive+1)) %>%
  ungroup()

filtered_samp_nrc_party <- samp_nrc_party %>%
  group_by(party) %>%
  filter(n() > 100) %>%
  ungroup()

filtered_samp_nrc_party %>%
  ggplot(aes(date, ratio, color = party)) +
  geom_point(alpha=0.25) +
  geom_smooth(method="loess", se = F, alpha=0.25, size = 1.5) +
  labs(y = "Sample NRC sentiment negative ratio by party", x = "Date")

filtered_samp_nrc_party %>%
  ggplot(aes(date, ratio, color = party)) +
  geom_point(alpha=0.25) +
  geom_smooth(method="loess", se = F, alpha=0.25, size = 1.5) +
  labs(y = "Sample NRC sentiment negative ratio by party", x = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2007-09-14")), col = "red", size = 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

filtered_samp_nrc_party %>%
  ggplot(aes(date, ratio, color = party)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = F, alpha = 0.7, size = 1) +
  labs(y = "Sample NRC Sentiment Negative Ratio", x = "Date") +
  facet_wrap(~party, scales = "free_y") +  
  geom_vline(xintercept = as.numeric(as.Date("2007-09-14")), col = "red", size = 1) +
  theme_minimal() +
  theme(legend.position = "bottom")


#5) Word embedding by party (two sets of data -> GloVe)
library(text2vec) # for implementation of GloVe algorithm
library(stringr) # to handle text strings
library(umap) # for dimensionality reduction later on
library(ggrepel)
# ================================ choice parameters
tidy_data_notoken <- data %>%
  mutate(desc = tolower(text)) %>%
  filter(grepl(all_words, desc))

# Count the number of texts in tidy_data by year and party
tidy_data_notoken$Year <- format(as.Date(tidy_data$date), "%Y")

##Define a Function for GloVe Training and UMAP Reduction
# choice parameters
WINDOW_SIZE <- 6
DIM <- 300
ITERS <- 100
COUNT_MIN <- 10

# shuffle text
set.seed(42L) # for reproducibility
glove_text <- sample(tidy_data_notoken$text)

# create vocab
tokens <- space_tokenizer(glove_text)
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vocab_pruned <- prune_vocabulary(vocab, term_count_min = COUNT_MIN)  # keep only words that meet count threshold

#create term co-occurrence matrix
vectorizer <- vocab_vectorizer(vocab_pruned)
tcm <- create_tcm(it, vectorizer, skip_grams_window = WINDOW_SIZE, skip_grams_window_context = "symmetric", 
                  weights = rep(1, WINDOW_SIZE))

#set model parameters
glove <- GlobalVectors$new(rank = DIM, x_max = 100, learning_rate = 0.05)

# fit model
word_vectors_main <- glove$fit_transform(tcm, n_iter = ITERS, convergence_tol = 0.001, n_threads = RcppParallel::defaultNumThreads())

# get output 
word_vectors_context <- glove$components
glove_embedding <- word_vectors_main + t(word_vectors_context)  # word vectors. combine main and context word vectors

#save 
saveRDS(glove_embedding, file = "local_glove.rds")

# GloVe dimension reduction
glove_umap <- umap(glove_embedding, n_components = 2, metric = "cosine", n_neighbors = 25, min_dist = 0.1, spread=2)

# Put results in a dataframe for ggplot
df_glove_umap <- as.data.frame(glove_umap[["layout"]])

# Add the labels of the words to the dataframe
df_glove_umap$word <- rownames(df_glove_umap)
colnames(df_glove_umap) <- c("UMAP1", "UMAP2", "word")
#---------------------training whole outcomes-------------
library(RcppParallel)
## filter data and apply function for each party
data_conser_pre <- filter(tidy_data_notoken, party == "Conservative" & date < as.Date("2007-09-24"))
data_labour_pre <- filter(tidy_data_notoken, party == "Labour" & date < as.Date("2007-09-24"))

data_conser_post <- filter(tidy_data_notoken, party == "Conservative" & date >= as.Date("2007-09-24"))
data_labour_post <- filter(tidy_data_notoken, party == "Labour" & date >= as.Date("2007-09-24"))

# for conser_pre
set.seed(42L)
glove_text_conser_pre <- sample(data_conser_pre$desc)

tokens_conser_pre <- space_tokenizer(glove_text_conser_pre)
it_conser_pre <- itoken(tokens_conser_pre, progressbar = FALSE)
vocab_conser_pre <- create_vocabulary(it_conser_pre)
vocab_pruned_conser_pre <- prune_vocabulary(vocab_conser_pre, term_count_min = COUNT_MIN)

vectorizer_conser_pre <- vocab_vectorizer(vocab_pruned_conser_pre)
tcm_conser_pre <- create_tcm(it_conser_pre, vectorizer_conser_pre, skip_grams_window = WINDOW_SIZE, skip_grams_window_context = "symmetric", weights = rep(1, WINDOW_SIZE))

glove_conser_pre <- GlobalVectors$new(rank = DIM, x_max = 100, learning_rate = 0.05)
word_vectors_main_conser_pre <- glove_conser_pre$fit_transform(tcm_conser_pre, n_iter = ITERS, convergence_tol = 0.001, n_threads = defaultNumThreads())

word_vectors_context_conser_pre <- glove_conser_pre$components
glove_embedding_conser_pre <- word_vectors_main_conser_pre + t(word_vectors_context_conser_pre)

saveRDS(glove_embedding_conser_pre, file = "local_glove_conser_pre.rds")

# Example of applying the function for one subset
umap_conser_pre <- umap(glove_embedding_conser_pre, n_components = 2, metric = "cosine", n_neighbors = 25, min_dist = 0.1, spread = 2)

df_umap_conser_pre <- as.data.frame(umap_conser_pre[["layout"]])
df_umap_conser_pre$word <- rownames(df_umap_conser_pre)
colnames(df_umap_conser_pre) <- c("Pre_Cons1", "Pre_Cons2", "word")

ggplot(df_umap_conser_pre) +
  geom_point(aes(x = Pre_Cons1, y = Pre_Cons2), color = 'blue', size = 0.05) +
  labs(title = "Conservative Party (Pre-Crisis): Word Embeddings via GloVe and UMAP") +
  theme_minimal()

# Plot the word embedding of words that are related for the GloVe model (immigration)
word_conser_pre_1 <- glove_embedding_conser_pre["immigration",, drop = FALSE]
cos_sim = sim2(x = glove_embedding_conser_pre, y = word_conser_pre_1, method = "cosine", norm = "l2")
select <- data.frame(rownames(as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 25))))
colnames(select) <- "word"
selected_words_conser_pre_1 <- df_umap_conser_pre %>% 
  inner_join(y=select, by= "word")

#The ggplot visual for GloVe
ggplot(selected_words_conser_pre_1, aes(x = Pre_Cons1, y = Pre_Cons2)) + 
  geom_point(show.legend = FALSE) + 
  geom_text(aes(Pre_Cons1, Pre_Cons2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "Conservative party (Pre-Crisis) GloVe word embedding of words related to 'immigration'") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# Plot the word embedding of words that are related for the GloVe model (immigrants) OKAY
word_conser_pre_2 <- glove_embedding_conser_pre["immigrants",, drop = FALSE]
cos_sim = sim2(x = glove_embedding_conser_pre, y = word_conser_pre_2, method = "cosine", norm = "l2")
select <- data.frame(rownames(as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 25))))
colnames(select) <- "word"
selected_words_conser_pre_2 <- df_umap_conser_pre %>% 
  inner_join(y=select, by= "word")

#The ggplot visual for GloVe
ggplot(selected_words_conser_pre_2, aes(x = Pre_Cons1, y = Pre_Cons2)) + 
  geom_point(show.legend = FALSE) + 
  geom_text(aes(Pre_Cons1, Pre_Cons2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "Conservative party (Pre-Crisis) GloVe word embedding of words related to 'immigrants'") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# Plot the word embedding of words that are related for the GloVe model (asylums)
word_conser_pre_3 <- glove_embedding_conser_pre["asylums",, drop = FALSE]
cos_sim = sim2(x = glove_embedding_conser_pre, y = word_conser_pre_3, method = "cosine", norm = "l2")
select <- data.frame(rownames(as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 25))))
colnames(select) <- "word"
selected_words_conser_pre_3 <- df_umap_conser_pre %>% 
  inner_join(y=select, by= "word")

#The ggplot visual for GloVe
ggplot(selected_words_conser_pre_3, aes(x = Pre_Cons1, y = Pre_Cons2)) + 
  geom_point(show.legend = FALSE) + 
  geom_text(aes(Pre_Cons1, Pre_Cons2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "Conservative party (Pre-Crisis) GloVe word embedding of words related to 'asylums'") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# Plot the word embedding of words that are related for the GloVe model (asylum)
word_conser_pre_4 <- glove_embedding_conser_pre["asylum",, drop = FALSE]
cos_sim = sim2(x = glove_embedding_conser_pre, y = word_conser_pre_4, method = "cosine", norm = "l2")
select <- data.frame(rownames(as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 25))))
colnames(select) <- "word"
selected_words_conser_pre_4 <- df_umap_conser_pre %>% 
  inner_join(y=select, by= "word")

#The ggplot visual for GloVe
ggplot(selected_words_conser_pre_4, aes(x = Pre_Cons1, y = Pre_Cons2)) + 
  geom_point(show.legend = FALSE) + 
  geom_text(aes(Pre_Cons1, Pre_Cons2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "Conservative party (Pre-Crisis) GloVe word embedding of words related to 'asylum'") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# for conser_post
set.seed(42L)
glove_text_conser_post <- sample(data_conser_post$desc)

tokens_conser_post <- space_tokenizer(glove_text_conser_post)
it_conser_post <- itoken(tokens_conser_post, progressbar = FALSE)
vocab_conser_post <- create_vocabulary(it_conser_post)
vocab_pruned_conser_post <- prune_vocabulary(vocab_conser_post, term_count_min = COUNT_MIN)

vectorizer_conser_post <- vocab_vectorizer(vocab_pruned_conser_post)
tcm_conser_post <- create_tcm(it_conser_post, vectorizer_conser_post, skip_grams_window = WINDOW_SIZE, skip_grams_window_context = "symmetric", weights = rep(1, WINDOW_SIZE))

glove_conser_post <- GlobalVectors$new(rank = DIM, x_max = 100, learning_rate = 0.05)
word_vectors_main_conser_post <- glove_conser_post$fit_transform(tcm_conser_post, n_iter = ITERS, convergence_tol = 0.001, n_threads = defaultNumThreads())

word_vectors_context_conser_post <- glove_conser_post$components
glove_embedding_conser_post <- word_vectors_main_conser_post + t(word_vectors_context_conser_post)

saveRDS(glove_embedding_conser_post, file = "local_glove_conser_post.rds")

# Example of applying the function for one subset
umap_conser_post <- umap(glove_embedding_conser_post, n_components = 2, metric = "cosine", n_neighbors = 25, min_dist = 0.1, spread = 2)

df_umap_conser_post <- as.data.frame(umap_conser_post[["layout"]])
df_umap_conser_post$word <- rownames(df_umap_conser_post)
colnames(df_umap_conser_post) <- c("Post_Cons1", "Post_Cons2", "word")

ggplot(df_umap_conser_post) +
  geom_point(aes(x = Post_Cons1, y = Post_Cons2), color = 'blue', size = 0.05) +
  labs(title = "Conservative Party (Post-Crisis): Word Embeddings via GloVe and UMAP") +
  theme_minimal()

# Plot the word embedding of words that are related for the GloVe model (immigration)
word_conser_post_1 <- glove_embedding_conser_post["immigration",, drop = FALSE]
cos_sim = sim2(x = glove_embedding_conser_post, y = word_conser_post_1, method = "cosine", norm = "l2")
select <- data.frame(rownames(as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 25))))
colnames(select) <- "word"
selected_words_conser_post_1 <- df_umap_conser_post %>% 
  inner_join(y=select, by= "word")

#The ggplot visual for GloVe
ggplot(selected_words_conser_post_1, aes(x = Post_Cons1, y = Post_Cons2)) + 
  geom_point(show.legend = FALSE) + 
  geom_text(aes(Post_Cons1, Post_Cons2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "Conservative party (Post-Crisis) GloVe word embedding of words related to 'immigration'") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# Plot the word embedding of words that are related for the GloVe model (immigrants)
word_conser_post_2 <- glove_embedding_conser_post["immigrants",, drop = FALSE]
cos_sim = sim2(x = glove_embedding_conser_post, y = word_conser_post_2, method = "cosine", norm = "l2")
select <- data.frame(rownames(as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 25))))
colnames(select) <- "word"
selected_words_conser_post_2 <- df_umap_conser_post %>% 
  inner_join(y=select, by= "word")

#The ggplot visual for GloVe
ggplot(selected_words_conser_post_2, aes(x = Post_Cons1, y = Post_Cons2)) + 
  geom_point(show.legend = FALSE) + 
  geom_text(aes(Post_Cons1, Post_Cons2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "Conservative party (Post-Crisis) GloVe word embedding of words related to 'immigrants'") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# Plot the word embedding of words that are related for the GloVe model (asylums)
word_conser_post_3 <- glove_embedding_conser_post["asylums",, drop = FALSE]
cos_sim = sim2(x = glove_embedding_conser_post, y = word_conser_post_3, method = "cosine", norm = "l2")
select <- data.frame(rownames(as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 25))))
colnames(select) <- "word"
selected_words_conser_post_3 <- df_umap_conser_post %>% 
  inner_join(y=select, by= "word")

#The ggplot visual for GloVe
ggplot(selected_words_conser_post_3, aes(x = Post_Cons1, y = Post_Cons2)) + 
  geom_point(show.legend = FALSE) + 
  geom_text(aes(Post_Cons1, Post_Cons2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "Conservative party (Post-Crisis) GloVe word embedding of words related to 'asylums'") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# Plot the word embedding of words that are related for the GloVe model (asylum)
word_conser_post_4 <- glove_embedding_conser_post["asylum",, drop = FALSE]
cos_sim = sim2(x = glove_embedding_conser_post, y = word_conser_post_4, method = "cosine", norm = "l2")
select <- data.frame(rownames(as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 25))))
colnames(select) <- "word"
selected_words_conser_post_4 <- df_umap_conser_post %>% 
  inner_join(y=select, by= "word")

#The ggplot visual for GloVe
ggplot(selected_words_conser_post_4, aes(x = Post_Cons1, y = Post_Cons2)) + 
  geom_point(show.legend = FALSE) + 
  geom_text(aes(Post_Cons1, Post_Cons2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "Conservative party (Post-Crisis) GloVe word embedding of words related to 'asylum'") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# for labour_pre
set.seed(42L)
glove_text_labour_pre <- sample(data_labour_pre$desc)

tokens_labour_pre <- space_tokenizer(glove_text_labour_pre)
it_labour_pre <- itoken(tokens_labour_pre, progressbar = FALSE)
vocab_labour_pre <- create_vocabulary(it_labour_pre)
vocab_pruned_labour_pre <- prune_vocabulary(vocab_labour_pre, term_count_min = COUNT_MIN)

vectorizer_labour_pre <- vocab_vectorizer(vocab_pruned_labour_pre)
tcm_labour_pre <- create_tcm(it_labour_pre, vectorizer_labour_pre, skip_grams_window = WINDOW_SIZE, skip_grams_window_context = "symmetric", weights = rep(1, WINDOW_SIZE))

glove_labour_pre <- GlobalVectors$new(rank = DIM, x_max = 100, learning_rate = 0.05)
word_vectors_main_labour_pre <- glove_labour_pre$fit_transform(tcm_labour_pre, n_iter = ITERS, convergence_tol = 0.001, n_threads = defaultNumThreads())

word_vectors_context_labour_pre <- glove_labour_pre$components
glove_embedding_labour_pre <- word_vectors_main_labour_pre + t(word_vectors_context_labour_pre)

saveRDS(glove_embedding_labour_pre, file = "local_glove_labour_pre.rds")

# Example of applying the function for one subset
umap_labour_pre <- umap(glove_embedding_labour_pre, n_components = 2, metric = "cosine", n_neighbors = 25, min_dist = 0.1, spread = 2)

df_umap_labour_pre <- as.data.frame(umap_labour_pre[["layout"]])
df_umap_labour_pre$word <- rownames(df_umap_labour_pre)
colnames(df_umap_labour_pre) <- c("Pre_Lab1", "Pre_Lab2", "word")

ggplot(df_umap_labour_pre) +
  geom_point(aes(x = Pre_Lab1, y = Pre_Lab2), color = 'blue', size = 0.05) +
  labs(title = "Labour Party (Pre-Crisis): Word Embeddings via GloVe and UMAP") +
  theme_minimal()

# Plot the word embedding of words that are related for the GloVe model (immigration)
word_labour_pre_1 <- glove_embedding_labour_pre["immigration",, drop = FALSE]
cos_sim = sim2(x = glove_embedding_labour_pre, y = word_labour_pre_1, method = "cosine", norm = "l2")
select <- data.frame(rownames(as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 25))))
colnames(select) <- "word"
selected_words_labour_pre_1 <- df_umap_labour_pre %>% 
  inner_join(y=select, by= "word")

#The ggplot visual for GloVe
ggplot(selected_words_labour_pre_1, aes(x = Pre_Lab1, y = Pre_Lab2)) + 
  geom_point(show.legend = FALSE) + 
  geom_text(aes(Pre_Lab1, Pre_Lab2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "Labour party (Pre-Crisis) GloVe word embedding of words related to 'immigration'") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# Plot the word embedding of words that are related for the GloVe model (immigrants)
word_labour_pre_2 <- glove_embedding_labour_pre["immigrants",, drop = FALSE]
cos_sim = sim2(x = glove_embedding_labour_pre, y = word_labour_pre_2, method = "cosine", norm = "l2")
select <- data.frame(rownames(as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 25))))
colnames(select) <- "word"
selected_words_labour_pre_2 <- df_umap_labour_pre %>% 
  inner_join(y=select, by= "word")

#The ggplot visual for GloVe
ggplot(selected_words_labour_pre_2, aes(x = Pre_Lab1, y = Pre_Lab2)) + 
  geom_point(show.legend = FALSE) + 
  geom_text(aes(Pre_Lab1, Pre_Lab2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "Labour party (Pre-Crisis) GloVe word embedding of words related to 'immigrants'") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# Plot the word embedding of words that are related for the GloVe model (asylum)
word_labour_pre_3 <- glove_embedding_labour_pre["asylum",, drop = FALSE]
cos_sim = sim2(x = glove_embedding_labour_pre, y = word_labour_pre_3, method = "cosine", norm = "l2")
select <- data.frame(rownames(as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 25))))
colnames(select) <- "word"
selected_words_labour_pre_3 <- df_umap_labour_pre %>% 
  inner_join(y=select, by= "word")

#The ggplot visual for GloVe
ggplot(selected_words_labour_pre_3, aes(x = Pre_Lab1, y = Pre_Lab2)) + 
  geom_point(show.legend = FALSE) + 
  geom_text(aes(Pre_Lab1, Pre_Lab2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "Labour party (Pre-Crisis) GloVe word embedding of words related to 'asylum'") +
  theme(plot.title = element_text(hjust = .5, size = 14))



# for labour_post
set.seed(42L)
glove_text_labour_post <- sample(data_labour_post$desc)

tokens_labour_post <- space_tokenizer(glove_text_labour_post)
it_labour_post <- itoken(tokens_labour_post, progressbar = FALSE)
vocab_labour_post <- create_vocabulary(it_labour_post)
vocab_pruned_labour_post <- prune_vocabulary(vocab_labour_post, term_count_min = COUNT_MIN)

vectorizer_labour_post <- vocab_vectorizer(vocab_pruned_labour_post)
tcm_labour_post <- create_tcm(it_labour_post, vectorizer_labour_post, skip_grams_window = WINDOW_SIZE, skip_grams_window_context = "symmetric", weights = rep(1, WINDOW_SIZE))

glove_labour_post <- GlobalVectors$new(rank = DIM, x_max = 100, learning_rate = 0.05)
word_vectors_main_labour_post <- glove_labour_post$fit_transform(tcm_labour_post, n_iter = ITERS, convergence_tol = 0.001, n_threads = defaultNumThreads())

word_vectors_context_labour_post <- glove_labour_post$components
glove_embedding_labour_post <- word_vectors_main_labour_post + t(word_vectors_context_labour_post)

saveRDS(glove_embedding_labour_post, file = "local_glove_labour_post.rds")

# 
umap_labour_post <- umap(glove_embedding_labour_post, n_components = 2, metric = "cosine", n_neighbors = 25, min_dist = 0.1, spread = 2)

df_umap_labour_post <- as.data.frame(umap_labour_post[["layout"]])
df_umap_labour_post$word <- rownames(df_umap_labour_post)
colnames(df_umap_labour_post) <- c("Post_Lab1", "Post_Lab2", "word")

ggplot(df_umap_labour_post) +
  geom_point(aes(x = Post_Lab1, y = Post_Lab2), color = 'blue', size = 0.05) +
  labs(title = "Labour Party (Post-Crisis): Word Embeddings via GloVe and UMAP") +
  theme_minimal()

# Plot the word embedding of words that are related for the GloVe model (immigration)
word_labour_post_1 <- glove_embedding_labour_post["immigration",, drop = FALSE]
cos_sim = sim2(x = glove_embedding_labour_post, y = word_labour_post_1, method = "cosine", norm = "l2")
select <- data.frame(rownames(as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 25))))
colnames(select) <- "word"
selected_words_labour_post_1 <- df_umap_labour_post %>% 
  inner_join(y=select, by= "word")

#The ggplot visual for GloVe
ggplot(selected_words_labour_post_1, aes(x = Post_Lab1, y = Post_Lab2)) + 
  geom_point(show.legend = FALSE) + 
  geom_text(aes(Post_Lab1, Post_Lab2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "Labour party (Post-Crisis) GloVe word embedding of words related to 'immigration'") +
  theme(plot.title = element_text(hjust = .5, size = 14))

# Plot the word embedding of words that are related for the GloVe model (immigrants)
word_labour_post_2 <- glove_embedding_labour_post["immigrants",, drop = FALSE]
cos_sim = sim2(x = glove_embedding_labour_post, y = word_labour_post_2, method = "cosine", norm = "l2")
select <- data.frame(rownames(as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 25))))
colnames(select) <- "word"
selected_words_labour_post_2 <- df_umap_labour_post %>% 
  inner_join(y=select, by= "word")

#The ggplot visual for GloVe
ggplot(selected_words_labour_post_2, aes(x = Post_Lab1, y = Post_Lab2)) + 
  geom_point(show.legend = FALSE) + 
  geom_text(aes(Post_Lab1, Post_Lab2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "Labour party (Post-Crisis) GloVe word embedding of words related to 'immigrants'") +
  theme(plot.title = element_text(hjust = .5, size = 14))


# Plot the word embedding of words that are related for the GloVe model (asylum)
word_labour_post_3 <- glove_embedding_labour_post["asylum",, drop = FALSE]
cos_sim = sim2(x = glove_embedding_labour_post, y = word_labour_post_3, method = "cosine", norm = "l2")
select <- data.frame(rownames(as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 25))))
colnames(select) <- "word"
selected_words_labour_post_3 <- df_umap_labour_post %>% 
  inner_join(y=select, by= "word")

#The ggplot visual for GloVe
ggplot(selected_words_labour_post_3, aes(x = Post_Lab1, y = Post_Lab2)) + 
  geom_point(show.legend = FALSE) + 
  geom_text(aes(Post_Lab1, Post_Lab2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "Labour party (Post-Crisis) GloVe word embedding of words related to 'asylum'") +
  theme(plot.title = element_text(hjust = .5, size = 14))