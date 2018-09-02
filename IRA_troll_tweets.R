# The code to load tweets and users is from Jonathan Bouchet

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("gridExtra")
#install.packages("ggthemes")
#install.packages("RColorBrewer")
#install.packages("tm")
#install.packages("tidytext")
#install.packages("wordcloud")
#install.packages("lubridate")
#install.packages("viridis")
#install.packages("tidyr")
#install.packages("tidyverse")
#install.packages("syuzhet")

library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggthemes)
library(RColorBrewer)
library(tm)
library(tidytext)
library(wordcloud)
library(lubridate)
library(viridis)
library(tidyr)
library(tidyverse)
library(syuzhet)


# read in tweet data files and combine
tweet_list <- list() 
listcsv <- dir(pattern = "IRAhandle_tweets_.") 
for (k in 1:length(listcsv)){
  print(paste("beginning processing of file", listcsv[k], Sys.time()," ")) 
  tweets_df <- read.csv(listcsv[k], sep=',', stringsAsFactors=F)
  
  
  tweets_df$publish_date<-mdy_hm(tweets_df$publish_date)
  tweets_df$publish_day<-as.Date(tweets_df$publish_date)
  tweets_df$publish_hour<-hour(tweets_df$publish_date)
  tweets_df$harvested_date<-mdy_hm(tweets_df$harvested_date)
  tweets_df$weekdays<-weekdays(tweets_df$publish_date)
  tweets_df$weekdays <- factor(tweets_df$weekdays, levels = rev(c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday")))
  
  
  #control how many tweets you work with
  tweets_sub <- tweets_df[1:10000,]
  
  # get sentiment and create columns in tweet data set
  tweets_sub$nrc_sentiment <- get_nrc_sentiment(tweets_sub$content)
  tweets_sub$anger <- tweets_sub$nrc_sentiment$anger
  tweets_sub$anticipation <- tweets_sub$nrc_sentiment$anticipation
  tweets_sub$disgust <- tweets_sub$nrc_sentiment$disgust
  tweets_sub$fear <- tweets_sub$nrc_sentiment$fear
  tweets_sub$joy <- tweets_sub$nrc_sentiment$joy
  tweets_sub$sadness <- tweets_sub$nrc_sentiment$sadness
  tweets_sub$surprise <- tweets_sub$nrc_sentiment$surprise
  tweets_sub$trust <- tweets_sub$nrc_sentiment$trust
  tweets_sub$negative <- tweets_sub$nrc_sentiment$negative
  tweets_sub$positive <- tweets_sub$nrc_sentiment$positive
  tweets_sub$nrc_sentiment <- NULL

  tweet_list[[k]] <- tweets_sub
  print(paste("finishing processing of file", listcsv[k], Sys.time()," "))  
  
}


tweets <- bind_rows(tweet_list) #combine all the tweet file data

# total author sentiment 
author_sentiment <- select(tweets, author, anger,
                           anticipation, disgust, fear, joy, 
                           sadness, surprise, trust, negative, positive) %>% 
  group_by(author) %>% 
  summarise(
    anger = sum(anger, na.rm = TRUE),
    anticipation = sum(anticipation, na.rm = TRUE),
    disgust = sum(disgust, na.rm = TRUE),
    fear = sum(fear, na.rm = TRUE),
    joy = sum(joy, na.rm = TRUE),
    sadness = sum(sadness, na.rm = TRUE),
    surprise = sum(surprise, na.rm = TRUE),
    trust = sum(trust, na.rm = TRUE),
    negative = sum(negative, na.rm = TRUE),
    positive = sum(positive, na.rm = TRUE), 
    tweet_count = n()
  )
View(author_sentiment)


#sentiment over time


sentiment_over_time <- select(tweets, publish_day,
                           anger, anticipation, disgust, fear, joy, 
                           sadness, surprise, trust, negative, 
                           positive ) %>%
  group_by(publish_day) %>% 
  summarise(
    anger = sum(anger, na.rm = TRUE),
    anticipation = sum(anticipation, na.rm = TRUE),
    disgust = sum(disgust, na.rm = TRUE),
    fear = sum(fear, na.rm = TRUE),
    joy = sum(joy, na.rm = TRUE),
    sadness = sum(sadness, na.rm = TRUE),
    surprise = sum(surprise, na.rm = TRUE),
    trust = sum(trust, na.rm = TRUE),
    negative = sum(negative, na.rm = TRUE),
    positive = sum(positive, na.rm = TRUE), 
    tweet_count = n()
  )
  
View(sentiment_over_time)

ggplot(data = sentiment_over_time) + 
  geom_line(mapping = aes(x = publish_day, y = positive, color="Positive")) +
  geom_line(mapping = aes(x = publish_day, y = negative, color="Negative"))

ggplot(data = sentiment_over_time) + 
  #geom_line(mapping = aes(x = publish_day, y = anger, color="Anger")) + 
  #geom_line(mapping = aes(x = publish_day, y = anticipation, color="Anticipation")) + 
  #geom_line(mapping = aes(x = publish_day, y = disgust, color="Disgust")) + 
  geom_line(mapping = aes(x = publish_day, y = fear, color="Fear")) + 
  geom_line(mapping = aes(x = publish_day, y = joy, color="Joy"))  
  #geom_line(mapping = aes(x = publish_day, y = sadness, color="Sadness")) + 
  #geom_line(mapping = aes(x = publish_day, y = surprise, color="Surprise")) + 
  #geom_line(mapping = aes(x = publish_day, y = trust, color="Trust"))

ggplot(data = tweets) + 
  geom_bar(mapping = aes(x = publish_hour))
