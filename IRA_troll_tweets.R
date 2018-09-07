# The code to load tweets and users is from Jonathan Bouchet
# The LDA code is from Rachael Tatman

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
#install.packages("topicmodels") # for LDA topic modelling 
#install.packages("SnowballC") # for stemming

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
library(topicmodels) # for LDA topic modelling 
library(SnowballC) # for stemming


top_terms_by_topic_LDA <- function(input_text, # should be a columm from a dataframe
                                   plot = T, # return a plot? TRUE by defult
                                   number_of_topics = 4) # number of topics (4 by default)
{    
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- Corpus(VectorSource(input_text)) # make a corpus object
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
  
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(10, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta) # arrange words in descending informativeness
  
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label 
      coord_flip() # turn bars sideways
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}


# categories for day hours
day_bins <- data.frame("publish_hour" = 0:23, 
                       "time_category" = c("evening leisure", "evening leisure", "evening leisure", "evening leisure",
                                           "sleep", "sleep", "sleep", "sleep", "sleep", "sleep", "sleep", "sleep",
                                           "2 hrs before work", "2 hrs before work",
                                           "morning work", "morning work", 
                                           "lunch", "lunch", 
                                           "afternoon work", "afternoon work", "afternoon work", "afternoon work",
                                           "2 hrs after work", "2 hrs after work"))
                         
                         
                         


# read in tweet data files and combine
tweet_list <- list() 
listcsv <- dir(pattern = "IRAhandle_tweets_.") 
for (k in 1:length(listcsv)){
  print(paste("beginning processing of file", listcsv[k], Sys.time()," ")) 
  tweets_df <- read.csv(listcsv[k], sep=',', stringsAsFactors=F)
  
  # filter out non-English tweets before any other processing
  tweets_df <- filter(tweets_df, account_category != "NonEnglish")
  
  tweets_df$publish_date<-mdy_hm(tweets_df$publish_date)
  tweets_df$publish_day<-as.Date(tweets_df$publish_date)
  tweets_df$publish_hour<-hour(tweets_df$publish_date)
  tweets_df$harvested_date<-mdy_hm(tweets_df$harvested_date)
  tweets_df$weekdays<-weekdays(tweets_df$publish_date)
  tweets_df$weekdays <- factor(tweets_df$weekdays, levels = rev(c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday")))

   
  #control how many tweets you work with
  tweets_sub <- tweets_df [1:1000,]

  # join to daytime_categories
  tweets_sub <-dplyr::select(tweets_sub, everything()) %>%
    left_join(day_bins, by = "publish_hour")
  
    
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


# *********************** Experimenting with topic analysis **********************

# create a document term matrix to clean
tweetCorpus <- Corpus(VectorSource(tweets$content)) 
tweetDTM <- DocumentTermMatrix(tweetCorpus)

# convert the document term matrix to a tidytext corpus
tweetDTM_tidy <- tidy(tweetDTM)

# I'm going to add my own custom stop words that I don't think will be
# very informative in hotel reviews
custom_stop_words <- tibble(word = c("https", "http", "amp"))

# remove stopwords
tweetDTM_tidy_cleaned <- tweetDTM_tidy %>% # take our tidy dtm and...
  anti_join(stop_words, by = c("term" = "word")) %>% # remove English stopwords and...
  anti_join(custom_stop_words, by = c("term" = "word")) # remove my custom stopwords

# reconstruct cleaned documents (so that each word shows up the correct number of times)
cleaned_documents <- tweetDTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(term, count))) %>%
  dplyr::select(document, terms) %>%
  unique()

# check out what the cleaned documents look like (should just be a bunch of content words)
# in alphabetic order
head(cleaned_documents)

top_terms_by_topic_LDA(cleaned_documents$terms, number_of_topics = 4)

# ***************************** End Experimenting with topic analyis ******************

# July 28, 2017 - Aug 26, 2017 "Right" topic analysis ******************************

july17_df <- dplyr::select(tweets, everything()) %>%
  filter(publish_date >= "2016-07-28" & publish_date <= "2017-08-26" )


july17Corpus <- Corpus(VectorSource(july17_df$content)) 
july17DTM <- DocumentTermMatrix(july17Corpus)

# convert the document term matrix to a tidytext corpus
july17DTM_tidy <- tidy(july17DTM)

# I'm going to add my own custom stop words that I don't think will be
# very informative in hotel reviews
custom_stop_words <- tibble(word = c("https", "http", "amp"))

# remove stopwords
july17DTM_tidy_cleaned <- july17DTM_tidy %>% # take our tidy dtm and...
  anti_join(stop_words, by = c("term" = "word")) %>% # remove English stopwords and...
  anti_join(custom_stop_words, by = c("term" = "word")) # remove my custom stopwords

# reconstruct cleaned documents (so that each word shows up the correct number of times)
july17_cleaned_documents <- july17DTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(term, count))) %>%
  dplyr::select(document, terms) %>%
  unique()

# check out what the cleaned documents look like (should just be a bunch of content words)
# in alphabetic order
#head(cleaned_documents)


top_terms_by_topic_LDA(july17_cleaned_documents$terms, number_of_topics = 6)

# **********************************************************************************

# total author sentiment 
author_sentiment <- dplyr::select(tweets, author, account_type, anger,
                           anticipation, disgust, fear, joy, 
                           sadness, surprise, trust, negative, positive) %>% 
    group_by(author, account_type) %>% 
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
#View(author_sentiment)


#sentiment over time


sentiment_over_time <- dplyr::select(tweets, publish_date, publish_day, account_type,
                           anger, anticipation, disgust, fear, joy, 
                           sadness, surprise, trust, negative, 
                           positive, time_category ) %>%
  filter(., publish_date > "2015-01-01" & account_type %in% c("Right", "left")) %>%
  group_by(publish_day, account_type) %>% 
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
  


ggplot(data = sentiment_over_time) + 
  geom_line(mapping = aes(x = publish_day, y = positive, color="Positive")) +
  geom_line(mapping = aes(x = publish_day, y = negative, color="Negative")) + 
facet_wrap(~account_type, nrow = 2)

ggplot(data = sentiment_over_time) + 
  #geom_line(mapping = aes(x = publish_day, y = anger, color="Anger")) +
  #geom_line(mapping = aes(x = publish_day, y = anticipation, color="Anticipation")) + 
  #geom_line(mapping = aes(x = publish_day, y = disgust, color="Disgust")) + 
  #geom_line(mapping = aes(x = publish_day, y = fear, color="Fear")) + 
  #geom_line(mapping = aes(x = publish_day, y = joy, color="Joy")) + 
  #geom_line(mapping = aes(x = publish_day, y = sadness, color="Sadness")) + 
  #geom_line(mapping = aes(x = publish_day, y = surprise, color="Surprise")) + 
  geom_line(mapping = aes(x = publish_day, y = trust, color="Trust")) + 
facet_wrap(~account_type, nrow = 2)


publish_hour <- dplyr::select(tweets, publish_hour, publish_date, account_type) %>%
  filter(., publish_date > "2015-01-01" & account_type %in% c("Right", "left")) 
  
ggplot(data = publish_hour) + 
  geom_bar(mapping = aes(x = publish_hour)) + 
facet_wrap(~account_type, nrow = 2)


dplyr::select(tweets, time_category, positive) %>%
  group_by(time_category) %>%
  summarise(pos_sentiment  = sum(positive, na.rm = TRUE)) %>%
ggplot(aes(x = time_category, y = pos_sentiment)) + 
  geom_bar(stat = "identity")


