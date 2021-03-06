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
#install.packages("wordcloud")
#install.packages("radarchart")

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
library(wordcloud)
library(radarchart)


# function to perform sentiment and emotion analysis
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
  
  # adding code to remove numeric-only values
  numeric_terms <- dplyr::select(topics, term) %>%
    subset(grepl('^\\d+$', topics$term))
  
  topics <- topics %>%
    anti_join(numeric_terms, by = c("term" = "term"))  
  
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
      theme(axis.text = element_text(size=20,face="bold")) + 
      coord_flip()   # turn bars sideways
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}


clean_text_for_LDA <- function(input_text) 
{ 
  # clean tweets
  time_type_Corpus <- Corpus(VectorSource(input_text)) 
  time_type_DTM <- DocumentTermMatrix(time_type_Corpus)
  
  # convert the document term matrix to a tidytext corpus
  time_type_DTM_tidy <- tidy(time_type_DTM)
  
  # Add custom stop words
  custom_stop_words <- tibble(word = c("https", "http", "amp", "231s", "231t"))
  
  # remove stopwords
  time_type_DTM_tidy_cleaned <- time_type_DTM_tidy %>% # take our tidy dtm and...
    anti_join(stop_words, by = c("term" = "word")) %>% # remove English stopwords and...
    anti_join(custom_stop_words, by = c("term" = "word")) # remove custom stopwords
  
  # find numeric-only terms
  numeric_terms <- dplyr::select(time_type_DTM_tidy_cleaned, term) %>%
    subset(grepl('^\\d+$', time_type_DTM_tidy_cleaned$term))
  
  # remove numeric-only terms
  time_type_DTM_tidy_cleaned <- time_type_DTM_tidy_cleaned %>%
    anti_join(numeric_terms, by = c("term" = "term"))
  
  #reconstruct cleaned documents (so that each word shows up the correct number of times)
  time_type_cleaned_documents <- time_type_DTM_tidy_cleaned %>%
    group_by(document) %>% 
    mutate(terms = toString(rep(term, count))) %>%
    dplyr::select(document, terms) %>%
    unique() 
  
}

get_tweets_w_topics <- function(topic_df_in, tweet_df_in) 
{ 
  
  # find unique terms across topics
  topic_keywords <- dplyr::select(topic_df_in, term) %>%
    group_by(term) %>%
    unique()
  
  # create comma-delimited list of terms for debugging purposes only
  topic_list <- paste(as.character(topic_keywords), collapse=", ")
  tweet_df_in$topic_list <- topic_list
  
  # find all the tweets that match one or more unique topic terms
  topic_match_df <- as.data.frame(sapply(topic_keywords$term, grepl, tweet_df_in$content))
  topic_match_df <- tibble::rowid_to_column(topic_match_df, "ID")
  term_gathered_df <- gather(topic_match_df,  key = "term", value = "found", -one_of("ID"))

  index_to_keep_df <- dplyr::select(term_gathered_df, ID, term, found) %>%
    filter(found == TRUE) %>%
    group_by(ID) %>%
    summarize(count = n()) %>%
    select(ID)
    
  # return the tweets that have one or more topic terms
  tweet_df_in[index_to_keep_df$ID, ]
  
}


#++++++++++++++++++++++++++++++++++
# rquery.wordcloud() : Word cloud generator
# - http://www.sthda.com
#+++++++++++++++++++++++++++++++++++
# x : character string (plain text, web url, txt file path)
# type : specify whether x is a plain text, a web page url or a file path
# lang : the language of the text
# excludeWords : a vector of words to exclude from the text
# textStemming : reduces words to their root form
# colorPalette : the name of color palette taken from RColorBrewer package, 
# or a color name, or a color code
# min.freq : words with frequency below min.freq will not be plotted
# max.words : Maximum number of words to be plotted. least frequent terms dropped
# value returned by the function : a list(tdm, freqTable)
rquery.wordcloud <- function(x, type=c("text", "url", "file"), 
                             lang="english", excludeWords=NULL, 
                             textStemming=FALSE,  colorPalette="Dark2",
                             min.freq=3, max.words=200)
{ 
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("RColorBrewer") 
  
  if(type[1]=="file") text <- readLines(x)
  else if(type[1]=="url") text <- html_to_text(x)
  else if(type[1]=="text") text <- x
  
  # Load the text as a corpus
  docs <- Corpus(VectorSource(text))
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove stopwords for the language 
  docs <- tm_map(docs, removeWords, stopwords(lang))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Remove your own stopwords
  if(!is.null(excludeWords)) 
    docs <- tm_map(docs, removeWords, excludeWords) 
  # Text stemming
  if(textStemming) docs <- tm_map(docs, stemDocument)
  # Create term-document matrix
  tdm <- TermDocumentMatrix(docs)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  # check the color palette name 
  if(!colorPalette %in% rownames(brewer.pal.info)) colors = colorPalette
  else colors = brewer.pal(8, colorPalette) 
  # Plot the word cloud
  set.seed(1234)
  wordcloud(d$word,d$freq, min.freq=min.freq, max.words=max.words,
            random.order=FALSE, rot.per=0.35, 
            use.r.layout=FALSE, colors=colors)
  
  invisible(list(tdm=tdm, freqTable = d))
}
#++++++++++++++++++++++
# Helper function
#++++++++++++++++++++++
# Download and parse webpage
html_to_text<-function(url){
  library(RCurl)
  library(XML)
  # download html
  html.doc <- getURL(url)  
  #convert to plain text
  doc = htmlParse(html.doc, asText=TRUE)
  # "//text()" returns all text outside of HTML tags.
  # We also don’t want text such as style and script codes
  text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  # Format text vector into one character string
  return(paste(text, collapse = " "))
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

# factor ordering
time_category_levels <- c(
  "2 hrs before work", "morning work", "lunch", "afternoon work", 
  "2 hrs after work", "evening leisure", "sleep")


# *************************** begin loop through tweet files *******************************
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
 
   
  # uncomment the row brackets to control how many tweets per file you work with
  tweets_sub <- tweets_df #[1:10000,]

  # join to daytime_categories
  tweets_sub <-dplyr::select(tweets_sub, everything()) %>%
    left_join(day_bins, by = "publish_hour")
  
    
  # get sentiment and create columns in tweet data set
  
  #comment out this line for faster performance without sentiment analysis
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

# optionally write out a file with all non-English tweets (~2.1M, 650MB)
write.csv(tweets, file="tweets_w_sentiment.csv")

# *************************** end loop through tweet files *******************************


# How many tweets per time_category and account type are there
time_and_type_df <-dplyr::select(tweets, time_category, account_type) %>%
      filter(account_type %in% c("Right", "left", "Hashtager", "news", "local"))  %>%
      group_by(time_category, account_type) %>%
      summarise(tweet_count = n())
      

topic_tweets_list <- list() # intialize list to hold data frames of tweets

# loop through all the combinations of account type and time category
for (row in 1:nrow(time_and_type_df)){
    time_category_txt <- as.character(time_and_type_df[row, "time_category"]$time_category)
    account_type_txt <- as.character(time_and_type_df[row, "account_type"])
    tweet_count <- time_and_type_df[row, "tweet_count"]

    print(paste("starting LDA and tweet extraction for account type", account_type_txt, "and time category", time_category_txt, Sys.time(), "" ))

    time_acct_df <-dplyr::select(tweets, everything()) %>%
      filter(time_category == time_category_txt & account_type == account_type_txt)
    
    time_type_cleaned_documents <- clean_text_for_LDA(time_acct_df$content)
    lda_df <- top_terms_by_topic_LDA(time_type_cleaned_documents, plot = F, number_of_topics = 4)
    lda_df$time_category = time_category_txt
    lda_df$account_category = account_type_txt
    lda_df$tweet_count = as.integer(tweet_count)

    topic_tweets <- get_tweets_w_topics(lda_df, time_acct_df)    
        
    topic_tweets_list[[row]] <- topic_tweets

    print(paste("ending LDA and tweet extraction for account type", account_type_txt, "and time category", time_category_txt, Sys.time(), "" ))
    
}

# holds all the tweets that match one or more LDA topic terms for a given troll/time cateogry
keyword_tweets <- bind_rows(topic_tweets_list) 

# optionally write out a file with all the filtered tweets (~600K, 340MB)
write.csv(keyword_tweets, file="topic_tweets_w_sentiment.csv")


# ******** start radar chart emotion by troll type 2015-2017  ******************

dplyr::select(tweets, account_type, publish_date, positive, negative,
              anger, anticipation, disgust, fear, joy, 
              sadness, surprise, trust) %>%
  filter(publish_date >= "2015-01-01" & publish_date <= "2017-12-31" &
           account_type %in% c("Right", "left")) %>%
  group_by(account_type) %>%
  summarise(pos_sentiment  = sum(positive, na.rm = TRUE),
            neg_sentiment = sum(negative, na.rm = TRUE),
            anger = sum(anger, na.rm = TRUE),
            anticipation = sum(anticipation, na.rm = TRUE),
            disgust = sum(disgust, na.rm = TRUE),
            fear = sum(fear, na.rm = TRUE),
            joy = sum(joy, na.rm = TRUE),
            sadness = sum(sadness, na.rm = TRUE),
            surprise = sum(surprise, na.rm = TRUE),
            trust = sum(trust, na.rm = TRUE),
            tweet_count = n()) %>%
  mutate(
    #Positive  = pos_sentiment / tweet_count,
    #Negative = neg_sentiment / tweet_count,
    Anger = anger / tweet_count,
    Anticipation = anticipation / tweet_count,
    Disgust = disgust / tweet_count,
    Fear = fear / tweet_count,
    Joy = joy / tweet_count,
    Sadness = sadness / tweet_count,
    Surprise = surprise / tweet_count,
    Trust = trust / tweet_count)    %>%
  gather(
    #Positive, Negative,
    Anger,Anticipation, Disgust, Fear, 
    Joy, Sadness, Surprise, Trust, 
    key = "emotion", value = average_per_tweet) %>%
  dplyr::select(account_type, emotion, average_per_tweet) %>%
  spread(key = account_type, value = average_per_tweet) %>%
  chartJSRadar(showLegend = F)

# ******** end radar chart emotion by troll type 2015-2017  **********************

# ******** right troll topics and word cloud July 20-24, 2015 ********************
rightJuly2015 <-dplyr::select(tweets, everything()) %>%
  filter(publish_date >= "2015-07-20" & publish_date <= "2015-07-24" &
           account_type %in% c("Right"))

rightJuly2015_cleaned_documents <- clean_text_for_LDA(rightJuly2015$content)
top_terms_by_topic_LDA(rightJuly2015_cleaned_documents, plot = T, number_of_topics = 2)

df <- tryCatch(rquery.wordcloud(rightJuly2015_cleaned_documents$terms, type=c("text", "url", "file"), 
                 lang="english", excludeWords = NULL, 
                 textStemming = FALSE,  colorPalette="Dark2",
                 max.words=100))
if("try-error" %in% class(df)) print(paste("error producing word cloud", geterrmessage(), ": "))



# ******** right troll topics and word cloud Mar 21-23, 2016 ********************
rightMar2016 <-dplyr::select(tweets, everything()) %>%
  filter(publish_date >= "2016-03-21" & publish_date <= "2016-03-23" &
           account_type %in% c("Right"))

rightMar2016_cleaned_documents <- clean_text_for_LDA(rightMar2016$content)
top_terms_by_topic_LDA(rightMar2016_cleaned_documents, plot = T, number_of_topics = 2)

df <- tryCatch(rquery.wordcloud(rightMar2016_cleaned_documents$terms, type=c("text", "url", "file"), 
                                lang="english", excludeWords = NULL, 
                                textStemming = FALSE,  colorPalette="Dark2",
                                max.words=100))
if("try-error" %in% class(df)) print(paste("error producing word cloud", geterrmessage(), ": "))



# ******** right troll topics and word cloud Sep 11-30, 2016 ********************
rightSep2016 <-dplyr::select(tweets, everything()) %>%
  filter(publish_date >= "2016-09-11" & publish_date <= "2016-09-30" &
           account_type %in% c("Right"))

rightSep2016_cleaned_documents <- clean_text_for_LDA(rightSep2016$content)
top_terms_by_topic_LDA(rightSep2016_cleaned_documents, plot = T, number_of_topics = 2)


# have to use a subset of timeframe to avoid memory error
rightSep2016Limited <-dplyr::select(tweets, everything()) %>%
  filter(publish_date >= "2016-09-18" & publish_date <= "2016-09-23" &
           account_type %in% c("Right"))

rightSep2016Limited_cleaned_documents <- clean_text_for_LDA(rightSep2016Limited$content)

df <- tryCatch(rquery.wordcloud(rightSep2016Limited_cleaned_documents$terms, type=c("text", "url", "file"), 
                                lang="english", excludeWords = NULL, 
                                textStemming = FALSE,  colorPalette="Dark2",
                                max.words=100))
if("try-error" %in% class(df)) print(paste("error producing word cloud", geterrmessage(), ": "))



# ******** left troll topics and word cloud Oct 5-8, 2016 ********************
leftOct2016 <-dplyr::select(tweets, everything()) %>%
  filter(publish_date >= "2016-10-05" & publish_date <= "2016-10-08" &
           account_type %in% c("left"))

leftOct2016_cleaned_documents <- clean_text_for_LDA(leftOct2016$content)
top_terms_by_topic_LDA(leftOct2016_cleaned_documents, plot = T, number_of_topics = 2)

df <- tryCatch(rquery.wordcloud(leftOct2016_cleaned_documents$terms, type=c("text", "url", "file"), 
                                lang="english", excludeWords = NULL, 
                                textStemming = FALSE,  colorPalette="Dark2",
                                max.words=100))
if("try-error" %in% class(df)) print(paste("error producing word cloud", geterrmessage(), ": "))



# ******** right troll topics and word cloud July 28, 2017 - Aug 26, 2017 ********************
rightJul2017 <-dplyr::select(tweets, everything()) %>%
  filter(publish_date >= "2017-07-28" & publish_date <= "2017-08-26" &
           account_type %in% c("Right"))

rightJul2017_cleaned_documents <- clean_text_for_LDA(rightSep2016$content)
top_terms_by_topic_LDA(rightJul2017_cleaned_documents, plot = T, number_of_topics = 2)


# have to use a subset of timeframe to avoid memory error
rightJul2017Limited <-dplyr::select(tweets, everything()) %>%
  filter(publish_date >= "2017-08-15" & publish_date <= "2017-08-16" &
           account_type %in% c("Right"))

rightJul2017Limited_cleaned_documents <- clean_text_for_LDA(rightJul2017Limited$content)

df <- tryCatch(rquery.wordcloud(rightJul2017Limited_cleaned_documents$terms, type=c("text", "url", "file"), 
                                lang="english", excludeWords = NULL, 
                                textStemming = FALSE,  colorPalette="Dark2",
                                max.words=100))
if("try-error" %in% class(df)) print(paste("error producing word cloud", geterrmessage(), ": "))




# ******** changes in emotion by time of day **********************************
emotion_by_time_day <- dplyr::select(keyword_tweets, time_category,
                                  anger, anticipation, disgust, fear, joy, 
                                  sadness, surprise, trust) %>%
  group_by(time_category) %>%
  summarise(anger = sum(anger, na.rm = TRUE),
            anticipation = sum(anticipation, na.rm = TRUE),
            disgust = sum(disgust, na.rm = TRUE),
            fear = sum(fear, na.rm = TRUE),
            joy = sum(joy, na.rm = TRUE),
            sadness = sum(sadness, na.rm = TRUE),
            surprise = sum(surprise, na.rm = TRUE),
            trust = sum(trust, na.rm = TRUE),
            tweet_count = n()) %>%
  mutate(
    time_category = factor(time_category, levels = time_category_levels),
    avg_anger = anger / tweet_count,
    avg_anticipation = anticipation / tweet_count,
    avg_disgust = disgust / tweet_count,
    avg_fear = fear / tweet_count,
    avg_joy = joy / tweet_count,
    avg_sadness = sadness / tweet_count,
    avg_surprise = surprise / tweet_count,
    avg_trust = trust / tweet_count)    %>%
  gather(
    avg_anger,avg_anticipation, avg_disgust, avg_fear, 
    avg_joy, avg_sadness, avg_surprise, avg_trust, 
    key = "sentiment", value = "avg_value") %>%
  dplyr::select(time_category, sentiment, avg_value)

ggplot(data = emotion_by_time_day, aes(x = time_category, y = avg_value, group=sentiment, color=sentiment)) + 
  geom_line(size = 2) + 
  labs(x = "Time of Day (CDT)", y = "Average Emotion") + 
  theme_bw() 



# ******** changes in sentiment by time of day **********************************
sent_by_time_day <- dplyr::select(keyword_tweets, time_category, 
                                  positive, negative) %>%
  group_by(time_category) %>%
  summarise(pos_sentiment  = sum(positive, na.rm = TRUE),
            neg_sentiment = sum(negative, na.rm = TRUE),
            tweet_count = n()) %>%
  mutate(
    time_category = factor(time_category, levels = time_category_levels),
    avg_pos_sentiment  = pos_sentiment / tweet_count,
    avg_neg_sentiment = neg_sentiment / tweet_count)    %>%
  gather(
    avg_pos_sentiment, avg_neg_sentiment,
    key = "sentiment", value = "avg_value") %>%
  dplyr::select(time_category, sentiment, avg_value)

ggplot(data = sent_by_time_day, aes(x = time_category, y = avg_value, group=sentiment, color=sentiment)) + 
  geom_line(size = 2) + 
  labs(x = "Time of Day (CDT)", y = "Average Sentiment") + 
  theme_bw() 



# # ***************************** unused data and plots ***********************************
# 
# # total author sentiment data frame for plotting purposes
# author_sentiment <- dplyr::select(tweets, author, account_type, anger,
#                                   anticipation, disgust, fear, joy,
#                                   sadness, surprise, trust, negative, positive) %>%
#   group_by(author, account_type) %>%
#   summarise(
#     anger = sum(anger, na.rm = TRUE),
#     anticipation = sum(anticipation, na.rm = TRUE),
#     disgust = sum(disgust, na.rm = TRUE),
#     fear = sum(fear, na.rm = TRUE),
#     joy = sum(joy, na.rm = TRUE),
#     sadness = sum(sadness, na.rm = TRUE),
#     surprise = sum(surprise, na.rm = TRUE),
#     trust = sum(trust, na.rm = TRUE),
#     negative = sum(negative, na.rm = TRUE),
#     positive = sum(positive, na.rm = TRUE),
#     tweet_count = n()
#   )
# 
# # sentiment over time data frame for plotting purposes
# sentiment_over_time <- dplyr::select(tweets, publish_date, publish_day, account_type,
#                                      anger, anticipation, disgust, fear, joy,
#                                      sadness, surprise, trust, negative,
#                                      positive, time_category ) %>%
#   filter(publish_date >= "2015-01-01" & publish_date <= "2017-12-31" &
#            account_type %in% c("Right")) %>%
#   group_by(publish_day, account_type) %>%
#   summarise(
#     anger = sum(anger, na.rm = TRUE),
#     anticipation = sum(anticipation, na.rm = TRUE),
#     disgust = sum(disgust, na.rm = TRUE),
#     fear = sum(fear, na.rm = TRUE),
#     joy = sum(joy, na.rm = TRUE),
#     sadness = sum(sadness, na.rm = TRUE),
#     surprise = sum(surprise, na.rm = TRUE),
#     trust = sum(trust, na.rm = TRUE),
#     negative = sum(negative, na.rm = TRUE),
#     positive = sum(positive, na.rm = TRUE),
#     tweet_count = n()
#   )
# 
# # publish data by hour for plotting purposes
# publish_hour <- dplyr::select(tweets, publish_hour, publish_date, account_type) %>%
#   filter(., publish_date > "2015-01-01" & account_type %in% c("Right", "left"))
# 
# # plot sentiment over time
# ggplot(data = sentiment_over_time) +
#   geom_line(mapping = aes(x = publish_day, y = positive, color="Positive")) +
#   geom_line(mapping = aes(x = publish_day, y = negative, color="Negative")) +
#   facet_wrap(~account_type, nrow = 2)
# 
# # plot emotion over time
# ggplot(data = sentiment_over_time) +
#   geom_line(mapping = aes(x = publish_day, y = anger, color="Anger")) +
#   geom_line(mapping = aes(x = publish_day, y = anticipation, color="Anticipation")) +
#   geom_line(mapping = aes(x = publish_day, y = disgust, color="Disgust")) +
#   geom_line(mapping = aes(x = publish_day, y = fear, color="Fear")) +
#   geom_line(mapping = aes(x = publish_day, y = joy, color="Joy")) +
#   geom_line(mapping = aes(x = publish_day, y = sadness, color="Sadness")) +
#   geom_line(mapping = aes(x = publish_day, y = surprise, color="Surprise")) +
#   geom_line(mapping = aes(x = publish_day, y = trust, color="Trust")) +
#   facet_wrap(~account_type, nrow = 2)
# 
# # plot tweets by publish hour
# ggplot(data = publish_hour) +
#   geom_bar(mapping = aes(x = publish_hour)) +
#   facet_wrap(~account_type, nrow = 2)
# s


