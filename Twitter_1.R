#Name: Ahmed Al Wattar
#Date 05-05-2020
#___________________________

rm(list = ls())
cat("\014")


library(rtweet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(RedditExtractoR)
#library(igraph)
library(tidytext)
library(tibble)
library(tidyverse)
library(SnowballC)



# load the keys
load(file = '~/R/R_Programming_Udemy/TextMiningWebAnalytics/MytwitterAuthentication.Rds')


# Setup the keys for twitter
token <- create_token(
  app = app_name,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret
)


#--1)Pick two different countries and search for tweets not users

TX <- search_tweets(
  "#Texas",
  n = 1000,
  include_rts = FALSE,
  retryonratelimit = FALSE
)


TX.tweets = TX %>% select(screen_name, text, created_at)
TX.tweets
#-------------------------------------------------------------
MA <- search_tweets(
  "#Boston",
  n = 1000,
  include_rts = FALSE,
  retryonratelimit = FALSE
)

MA.tweets = MA %>% select(screen_name, text, created_at)
MA.tweets

#--2 and 3) Process each set of tweets into tidy text


# Remove http elements manually from Texas tweets
TX.tweets$stripped_text <- gsub("http\\S+", "", TX.tweets$text)
TX.tweets$stripped_text <-
  gsub("[^\u0020-\u007F]+", "", TX.tweets$stripped_text)
TX.tweets$stripped_text <-
  gsub("'|’", "", TX.tweets$stripped_text)
TX.tweets$stripped_text <-
  gsub(" \\b\\d+\\b", "", TX.tweets$stripped_text)# remove numeric followed by space
# remove Texas and texas from the tweets
TX.tweets$stripped_text <- gsub("Texas|texas","",TX.tweets$stripped_text)
#TX.tweets$stripped_text <- gsub("texas","",TX.tweets$stripped_text)
#TX.tweets$stripped_text <- gsub("gregabbott_tx","gregabbott",TX.tweets$stripped_text)
TX.tweets$stripped_text <- gsub("amp|de","",TX.tweets$stripped_text)
head(TX.tweets$stripped_text)
#---------------------------------------------------------

# Remove http elements manually from Boston tweets

MA.tweets$stripped_text <- gsub("http\\S+|fuc\\S+", "", MA.tweets$text)
MA.tweets$stripped_text <-
  gsub("[^\u0020-\u007F]+", "", MA.tweets$stripped_text)
MA.tweets$stripped_text <- gsub("", "", MA.tweets$stripped_text)
MA.tweets$stripped_text <-
  gsub("'|’", "", MA.tweets$stripped_text)
MA.tweets$stripped_text <-
  gsub(" \\b\\d+\\b", "", MA.tweets$stripped_text)# remove numeric followed by space
# remove the word Boston and boston from the tweets
MA.tweets$stripped_text <- gsub("Boston|boston","",MA.tweets$stripped_text)
MA.tweets$stripped_text <- gsub("boston","",MA.tweets$stripped_text)
MA.tweets$stripped_text <- gsub("amp","",MA.tweets$stripped_text)
MA.tweets$stripped_text <-
  gsub(" \\b\\d+\\b", "", MA.tweets$stripped_text)# remove numeric followed by space
MA.tweets$stripped_text <-
  gsub("[^\u0020-\u007F]+", "", MA.tweets$stripped_text)
head(TX.tweets$stripped_text)
head(MA.tweets$stripped_text)


# Use the unnest_tokens() function to convert to lowercase,
# remove punctuation, and add id for each tweet

# Texas tweets with stem words
twt.tx_cleanS <- TX.tweets %>%
select(stripped_text) %>%
unnest_tokens(word, stripped_text) %>%
mutate(word = wordStem(word))

head(twt.tx_cleanS, n = 20)

# TX tweets without without stem words

twt.tx_cleanW <- TX.tweets %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

head(twt.tx_cleanW, n = 10)

# will do same above for Boston Tweets

twt.ma_cleanS <- MA.tweets %>%
select(stripped_text) %>%
unnest_tokens(word, stripped_text) %>%
mutate(word = wordStem(word))

head(twt.ma_cleanS, n = 20)

# MA without stem words

twt.ma_cleanW <- MA.tweets %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

head(twt.ma_cleanW, n = 10)

# Remove the stop words from the text part for the tweets

# Load stop words from the tidytext package

data("stop_words")

# And remove stop words from Texas list of without stem words

Wcleaned_tx.twt <- twt.tx_cleanW %>%
  anti_join(stop_words)



# And remove stop words from Boston list of without stem words

Wcleaned_ma.twt <- twt.ma_cleanW %>%
  anti_join(stop_words)
#################################################################################
# Review the head for the tweets for both countries and see how stem affect them#

head(twt.tx_cleanS, n = 20) # with stem
head(twt.tx_cleanW, n = 20) # no stem
head(twt.ma_cleanS, n = 20) # with stem
head(twt.ma_cleanW, n = 20) # no stem
head(Wcleaned_tx.twt, n=10) #Texas tokens without stop words + no stem
head(Wcleaned_ma.twt, n=10) #Boston tokens without stop words + no stem
##################################################################################

#--4) List of the most frequent words
# Plots for frequent words in #Texas and #Boston after removing step words

view(Wcleaned_tx.twt)
Wcleaned_tx.twt %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n,fill=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_light() +
  labs(x = "Count",
       y = "Words",
       title = "Word counts found in #Texas tweets")
#-----------------------------------------------------------------------

Wcleaned_ma.twt %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_light() +
  labs(x = "Count",
       y = "Words",
       title = "Word counts found in #Boston tweets")
#------------------------------------------------------------------------

# Create the list of top 10 words and combine them, and find the common words
Freq.tx <- as.data.frame(Wcleaned_tx.twt %>%
                           count(word, sort = TRUE) %>%
                           top_n(10))
Freq.ma <- as.data.frame(Wcleaned_ma.twt %>%
                           count(word, sort = TRUE) %>%
                           top_n(10))

cat("Words in #Texas are \n",
    Freq.tx$word,
    "\n",
    "\nWords in #Boston are \n",
    Freq.ma$word)

# Convert the lists to tibble to find the common words
t <- as.tibble(Freq.tx$word)
t
b <- as.tibble(Freq.ma$word)
b
# Common Words in both lists
inner_join(t, b)

# the frequent words combined from the two countries
full_join(t, b)

#------------------------------------------------------------

#--5) Show the top pairs for each country

# Explore words that occur together in pairs in Texas tweets
# with out leaving the stop words
twt.tx_pairs = TX.tweets %>%
  select(stripped_text) %>%
  unnest_tokens(pairs, stripped_text, token = "ngrams", n = 2)
head(twt.tx_pairs)
twt.tx_pairs_counts <- twt.tx_pairs %>%
  count(pairs, sort = TRUE)
head(twt.tx_pairs_counts, n = 5)

# After removing the stop words "Pairs in Texas tweets)

twt.tx_pairs_separate = twt.tx_pairs %>%
  separate(pairs, c("Word1", "Word2"), sep = " ")
head(twt.tx_pairs_separate)

twt.tx_pairs_clean <- twt.tx_pairs_separate %>%
  filter(!Word1 %in% stop_words$word) %>%
  filter(!Word2 %in% stop_words$word)
head(twt.tx_pairs_clean)

# Pairs with the count
twt.tx_pairs_counts <- twt.tx_pairs_clean %>%
  count(Word1, Word2, sort = TRUE)
head(twt.tx_pairs_counts)

# Do the same to Boston tweets

twt.ma_pairs = MA.tweets %>%
  select(stripped_text) %>%
  unnest_tokens(pairs, stripped_text, token = "ngrams", n = 2)
head(twt.ma_pairs)
twt.ma_pairs_counts <- twt.ma_pairs %>%
  count(pairs, sort = TRUE)
head(twt.ma_pairs_counts, n = 5)


twt.ma_pairs_separate = twt.ma_pairs %>%
  separate(pairs, c("Word1", "Word2"), sep = " ")
head(twt.ma_pairs_separate)

twt.ma_pairs_clean <- twt.ma_pairs_separate %>%
  filter(!Word1 %in% stop_words$word) %>%
  filter(!Word2 %in% stop_words$word)
head(twt.ma_pairs_clean)

# Pairs with the count
twt.ma_pairs_counts <- twt.ma_pairs_clean %>%
  count(Word1, Word2, sort = TRUE)
head(twt.ma_pairs_counts)

#-----------------------------------------------

#--Create a word cloud for Boston Tweets
library(wordcloud)
library(viridis)
twt.ma.counts = Wcleaned_ma.twt %>% count(word)
wordcloud(words = twt.ma.counts$word, freq =twt.ma.counts$n, min.freq =5,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, 'Dark2'))

#--Create a word cloud for Texas tweets


twt.tx.counts = Wcleaned_tx.twt %>% count(word)
wordcloud(words =twt.tx.counts$word, freq =twt.tx.counts$n, min.freq = 15,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, 'Dark2'))
#----------------------------------------------------

#--Compute senitment score, compare the sentiment for both hashtags

### --- Sentiment Analysis

library(tidytext)
#install.packages("textdata")
# using nrc sentiment for Saif Mohammed , you can use bing dataset
get_sentiments('nrc')

#--Will use the cleaned tibble from step (4) 


Wcleaned_tx.twt# cleaned tibble for Texas tweets
Wcleaned_ma.twt# cleaned tibble for Boston tweets

# bing sentiment analysis

nrc_texas = Wcleaned_tx.twt %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

nrc_mass = Wcleaned_ma.twt %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
# plot the negative and positive words
nrc_mass %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "#Boston Tweets Sentiments ",
       subtitle = "Using NRC Sentiment Dataset",
       y = NULL,
       x = NULL) +
  coord_flip() + theme_get()



nrc_texas %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "#Texas Tweets Sentiments",
       subtitle = "Using NRC Sentiment DataSet",
       y = NULL,
       x = NULL) +
  coord_flip() + theme_get()

#--------------------------------------------------


################################### End ######################################

