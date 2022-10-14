cat("\014")
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(qdap)
library(tm)
library(gridExtra)
library(dendextend)
library(ggthemes)
library(RWeka)
library(tidytext)
library(textdata)
library(lubridate)
library(anytime)
data_tibble <- data %>%
  unnest_tokens(output = "words", input = text, token = "words")
tweets_tibble_clean <- data_tibble %>%
  anti_join(stop_words, by=c("words"="word"))
word_freq <- tweets_tibble_clean %>% 
  inner_join(get_sentiments("bing"), by = c("words" = "word")) %>% 
  count(words, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative)%>%
  filter(abs(polarity) >= 9) %>%
  mutate(
    pos_or_neg = ifelse(polarity > 0, "positive", "negative")
  )
ggplot(word_freq, aes(x = reorder(words, polarity),y =  polarity, fill = pos_or_neg)) +
  geom_col() + 
  labs(
    x = "words",
    title = "Sentiment Word Frequency"
  )+
  theme_func()+
  theme(axis.text.x = element_text(angle = 55)) 
year_tibble <- data%>%
  mutate(date = anytime::anydate(data$created_at))%>%
  mutate(year = format(as.Date(date, format="%d/%m/%Y"),"%Y"))%>%
  unnest_tokens(output = "words", input = text, token = "words")%>%
 anti_join(stop_words, by=c("words"="word"))%>%
  group_by(words)%>%
  mutate(count=n())
word_sentiments <- year_tibble %>%
  inner_join(get_sentiments("afinn"), by = c("words"="word")) %>%
  group_by(year) %>%
  summarize(score = sum(value *count) / sum(count))
word_sentiments %>%
  mutate(year = reorder(year, score)) %>%
  ggplot(aes(year, score, fill = score > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ggtitle("Sentiment Score of Elon Musk Tweets") + 
  labs(subtitle = "2010-2017",
       y = "Average sentiment score") +
  theme_func()
twenty_ten <- data%>%
  mutate(date = anytime::anydate(data$created_at))%>%
  mutate(year = format(as.Date(date, format="%d/%m/%Y"),"%Y"))%>%
  filter(year==2010)%>%
  select(text,year)  
twenty_ten
hour_tibble <- data%>%
  mutate(hour=format(data$created_at, "%H"))%>%
  unnest_tokens(output = "words", input = text, token = "words")%>%
  anti_join(stop_words, by=c("words"="word"))%>%
  
  group_by(words)%>%
  mutate(count=n())
letters_sentiments1 <- hour_tibble %>%
  inner_join(get_sentiments("afinn"), by = c("words"="word")) %>%
  group_by(hour) %>%
  summarize(score = sum(value *count) / sum(count))
ggplot(letters_sentiments1,aes(hour,score))+
  geom_segment(aes(x=hour,xend=hour,y=0,yend=score))+
  geom_point(size=5,color="red",fill=alpha("orange",0.3),alpha=0.7,shape=21,stroke=2)+
  theme_func()+
  labs(title = "Sentiment Score in Each Hour")

data_tibble <- data %>%
  unnest_tokens(output = "words", input = text, token = "words")

tweets_tibble_clean <- data_tibble %>%
  anti_join(stop_words, by=c("words"="word"))%>%
  group_by(words)%>%
  summarise(count=n())


word_freq_nrc <- tweets_tibble_clean %>% 
  inner_join(get_sentiments("nrc"), by = c("words" = "word")) %>% 
  filter(!grepl("positive|negative", sentiment)) %>%
  group_by(sentiment) %>% 
  summarize(total_count = sum(count))

ggplot(word_freq_nrc, aes(reorder(x = sentiment,total_count), y = total_count,fill = sentiment)) +
  geom_col()+
  theme_func()+
  coord_flip() + theme(legend.position="none") +
  labs(x = "Sentiments",title = "Top Sentiments using NRC Lexicon") 

data %>%
  unnest_tokens(output = "words", input = text, token = "words")%>%
  anti_join(stop_words, by=c("words"="word"))%>%
  count(words) %>%
  inner_join(get_sentiments("nrc"), by = c("words"="word")) %>%
  group_by(sentiment) %>%
  filter(!grepl("positive|negative", sentiment)) %>%
  top_n(5, n) %>%
  ungroup() %>%
  mutate(word = reorder(words, n)) %>%
  ggplot(aes(word, n,fill=sentiment)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ sentiment, scales = "free") +
  theme_func()+
  labs(y = "count",title = "Top Words under Each Sentiment")
