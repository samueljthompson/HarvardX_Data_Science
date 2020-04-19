library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

#Reference Data Set (Already Compiled using RTweet)
data("trump_tweets")
head(trump_tweets)
names(trump_tweets)

#Filter our retweets
trump_tweets %>%
  extract(source, "source", "Twitter for (.*)") %>%
  count(source)

#Specify Campaign Tweets and Devices in Question
campaign_tweets <- trump_tweets %>%
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") &
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of Day (EST)", y = "% of Tweets", color = "")

library(tidytext)
i <- 3008 #Extraction Example Using Tweet 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>%
  unnest_tokens(word, text) %>%
  select(word)

pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))" #Defining a Pattern excluding Twitter syntax
campaign_tweets[i,] %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|%amp;", "")) %>% #Removing Picture Links
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

#Extracting Words from All Tweets
tweet_words <- campaign_tweets %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|%amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern)

#Identifying the Most Common Words
tweet_words %>%
  count(word) %>%
  arrange(desc(n))

#Filtering out meaningless words and other unwanted characteristics
tweet_words <- campaign_tweets %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|%amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

tweet_words %>%
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

#Determining Tweet Source Likelihood using Odds Ratio
android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>% 
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) /
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

#Filtering for total frequency
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)

#Sentiment Analysis
sentiments
get_sentiments("bing") #Positive and Negative (Binary)
get_sentiments("afinn") #Positive and Negative (Spectrum)
get_sentiments("nrc") %>% count(sentiment) #Varied Sentiments

nrc <- get_sentiments("nrc") %>% select(word, sentiment)
tweet_words %>% inner_join(nrc, by = "word") %>%
  select(source, word, sentiment) %>% sample_n(10)

sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

tweet_words %>% group_by(source) %>% summarize(n = n())

#Odds Ratio Comparing likelihood of sentiment laden diction per device
sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android),
         iPhone = iPhone / (sum(iPhone) - iPhone),
         or = Android/iPhone) %>%
  arrange(desc(or))

library(broom)
log_or <- sentiment_counts %>%
  mutate(log_or = log((Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
         se = sqrt(1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
         conf.low = log_or - qnorm(.975)*se,
         conf.high = log_or + qnorm(.975)*se) %>%
  arrange(desc(log_or))
log_or #Confidence Intervals for top five sentiments significantly favor Android

log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and Sentiment") +
  coord_flip()

#Exploring the specific words driving the difference in sentiment odds
android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
         arrange(desc(or))

android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Assessment (Pride and Prejudice)
library(gutenbergr)
library(tidytext)
options(digits = 3)
head(gutenberg_metadata)
gutenberg_metadata %>%
    filter(str_detect(title, "Pride and Prejudice"))
gutenberg_works(str_detect(gutenberg_metadata$title, "Pride and Prejudice"))
Pride_and_Prejudice <- (gutenberg_download(gutenberg_id = 1342))

words <- Pride_and_Prejudice %>%
  unnest_tokens(word, text)

words <- words %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "\\d")) 
words %>%
  group_by(word) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  filter(n >= 100)

afinn <- get_sentiments("afinn")
afinn_sentiments <- words %>% inner_join(afinn, by = "word")
afinn_sentiments
mean(afinn_sentiments$value >= 0)
afinn_sentiments %>%
  filter(value == 4)