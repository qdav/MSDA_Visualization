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


tweet_list <- list() 
listcsv <- dir(pattern = "IRAhandle_tweets_.") 
for (k in 1:length(listcsv)){
  tweet_list[[k]] <- read.csv(listcsv[k], sep=',', stringsAsFactors=F)
}

tweets <- bind_rows (tweet_list)

tweets$publish_date<-mdy_hm(tweets$publish_date)
tweets$harvested_date<-mdy_hm(tweets$harvested_date)
tweets$weekdays<-weekdays(tweets$publish_date)
tweets$weekdays <- factor(tweets$weekdays, levels = rev(c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday")))
View(tweets)

tweets_subset <- tweets[1:10000,]
tweets_vector <- tweets_subset$content
#a <- tweets_vector

a_corpus = Corpus(VectorSource(tweets_vector))
a_tm <- TermDocumentMatrix(a_corpus)
a_tmx <- as.matrix(a_tm)
a_df<-data.frame(text=unlist(sapply(a, `[`)), stringsAsFactors=F)
a_df$nrc_sent<-get_nrc_sentiment(a_df$text) 
View(a_df)
colSums(a_df)
