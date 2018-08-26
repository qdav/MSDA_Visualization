#install.packages("tidyverse")
#install.packages("zoo")
#install.packages("usmap")


library(tidyverse)
library(zoo)
library(usmap)

options(stringsAsFactors = FALSE)

# get list of permits by state
permits <- read.csv("nics-firearm-background-checks.csv")
permits$month <- as.yearmon(permits$month)
permits$state <- as.factor(permits$state)

# get population by state
state_pop_temp <- read_csv("nst-est2017-alldata.csv") %>%
  gather(`POPESTIMATE2010`, `POPESTIMATE2011`, 
         `POPESTIMATE2012`, `POPESTIMATE2013`, 
         `POPESTIMATE2014`, `POPESTIMATE2015`,
         `POPESTIMATE2016`, `POPESTIMATE2017`,
         key = "year", value = "population") %>%
  separate("year", c("nothing", "year"), 11) %>%
  select("SUMLEV", "NAME", "year", "population") 
 
  colnames(state_pop_temp)[colnames(state_pop_temp) == 'NAME'] <- 'state'
  state_pop <- filter( state_pop_temp, SUMLEV == 40)

  
  # show map of population by state for 2017
  state_pop_2017 <- filter(state_pop, year == "2017")
  
  usmap::plot_usmap(data = state_pop_2017, values = "population", lines = "red") + 
    scale_fill_continuous(
      low = "white", high = "red", name = "Population By State 2017", label = scales::comma
    ) + theme(legend.position = "right")  
  


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



