# This code is from Jonathan BouchetBeware

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

# users dataset
users<-read.csv('users.csv',sep=',',stringsAsFactors=F)
users$month<-sapply(users$created_at, function(x) match(strsplit(x," ")[[1]][2], month.abb))
users$year<-sapply(users$created_at, function(x) as.numeric(strsplit(x," ")[[1]][6]))
users$day<-sapply(users$created_at, function(x) as.numeric(strsplit(x," ")[[1]][3]))
users$DayTS<-as.Date(paste0(users$year,'-',users$month,'-',users$day), format="%Y-%m-%d")
#clean from empty creation date
users<-data.frame(users %>% filter(created_at !=""))

head(users, 1)

#tweets dataset
tweets<-read.csv('tweets.csv',sep=',',stringsAsFactors=F)
tweets$DayTS<-as.Date(tweets$created_str,format="%Y-%m-%d")
tweets$year<-year(tweets$DayTS)
tweets$month<-month(tweets$DayTS)
tweets$day<-day(tweets$DayTS)
tweets$weekdays<-weekdays(tweets$DayTS)
tweets$week<-week(tweets$DayTS)
tweets$weekdays <- factor(tweets$weekdays, levels = rev(c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday")))



per_week <- tweets %>% select(year, week, weekdays) %>% na.omit() %>% group_by(year,week,weekdays) %>% summarize(week_count=n())         
pal='D'
ggplot(per_week, aes(x = week, y = weekdays, fill=week_count)) + 
  scale_fill_viridis(name="", 
                     option = pal,  # Variable color palette
                     direction = -1,  # Variable color direction
                     na.value = "grey93",
                     limits = c(0, max(per_week$week_count))) +
  geom_tile(color = "white", size = 0.2) +
  facet_wrap("year", ncol = 1) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(1, 52, length = 12),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),axis.text = element_text(size=10),panel.grid.major.y= element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        strip.text = element_text(hjust = 0.01, face = "bold", size = 10)) + ggtitle('Daily count of Tweets') + guides(fill = guide_colorbar(barwidth = 53, barheight = 1))


g1<-tweets %>% filter(year==2016) %>% group_by(DayTS) %>% summarise(count=n()) %>% 
  ggplot(aes(x=DayTS,y=count)) + geom_point(size=1) + geom_line(alpha=.5,size=1) +
  theme_fivethirtyeight() + labs(title="2016 timeline: daily tweets count")
g1 + 
  geom_vline(xintercept=as.numeric(as.Date('2016-07-21')),color='red') + 
  ggplot2::annotate("text", x=as.Date("2016-06-01"), y = 3000, label = "RNC Convention:\nTrump won the presidential nomination", size=4, colour="red") + geom_vline(xintercept=as.numeric(as.Date('2016-11-08')),color='red') + 
  ggplot2::annotate("text", x=as.Date("2016-12-12"), y = 3000, label = "U.S Presidential election:\n Trump won the election", size=4, colour="red") + geom_vline(xintercept=as.numeric(as.Date('2016-03-19')),color='blue') + 
  ggplot2::annotate("text", x=as.Date("2016-02-19"), y = 3000, label = "Clinton\'s campaign\n chairman incident[1]", size=4, colour="blue") + 
  labs(subtitle='[1]: Clinton campaign chairman John Podesta is sent an email that encourages him to change his email password, probably precipitating the hack of his account') + 
  geom_vline(xintercept=as.numeric(as.Date('2016-10-06')),color='#006400') + 
  ggplot2::annotate("text", x=as.Date("2016-08-28"), y = 4000, label = "WikiLeaks begins releasing\n emails stolen from Podesta", size=4, colour="darkgreen")

