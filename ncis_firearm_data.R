#install.packages("tidyverse")
#install.packages("zoo")
#install.packages("usmap")


library(tidyverse)
library(zoo)
library(usmap)

options(stringsAsFactors = FALSE)

# retrieve data from comma delimited file/adjust data types
permits <- read.csv("nics-firearm-background-checks.csv")
permits$month <- as.yearmon(permits$month)
permits$state <- as.factor(permits$state)

# show map of permits by state for 2017
permits2017 <- filter(permits, 
                       permits$month >= as.yearmon("2017-01") &
                       permits$month < as.yearmon("2018-01"))

usmap::plot_usmap(data = permits2017, values = "totals", lines = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Permits By State 2017", label = scales::comma
  ) + theme(legend.position = "right")


# Texas long gun vs hand gun permits over time
texas_perm <- filter(permits, 
                      permits$state == "Texas")

ggplot(data = texas_perm) + 
  geom_line(mapping = aes(x = month, y = long_gun, color="Long Gun")) +
  geom_line(mapping = aes(x = month, y = handgun, color="Handgun"))



