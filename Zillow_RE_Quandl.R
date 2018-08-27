#install.packages("Quandl")

# Quinn's api_key being used here
Quandl.api_key("awJDWs_WvUiV6VWC8rLb")
library(Quandl)
library(tidyverse)

#creating a list of all-inclusive dates from which to left-join
Date <- as.Date(as.yearmon(seq.Date(as.Date('2010-01-01'),by='month',
                                    length.out = 108)),frac=1)
date_master_df <- data.frame(Date)

# code C11731 refers to the city of San Antonio (good to double-check)
mydata1 <- Quandl('ZILLOW/C11731_PRRAH') 
colnames(mydata1)[colnames(mydata1) == 'Value'] <- 'price_rent_ratio'
mydata2 <- Quandl('ZILLOW/C11731_MLPFAH') 
colnames(mydata2)[colnames(mydata2) == 'Value'] <- 'med_list_price_sf'
mydata3 <- Quandl('ZILLOW/C11731_MLPAH') 
colnames(mydata3)[colnames(mydata3) == 'Value'] <- 'median_list_price'
mydata4 <- Quandl('ZILLOW/C11731_IMP') 
colnames(mydata4)[colnames(mydata4) == 'Value'] <- 'inv_measure'
mydata5 <- Quandl('ZILLOW/C11731_PLPRAH') 
colnames(mydata5)[colnames(mydata5) == 'Value'] <- 'pct_listings_price_red'

city_re_stats <- list(date_master_df, mydata1, mydata2, mydata3, mydata4, mydata5) %>% reduce(left_join, by = "Date")

str(city_re_stats)
View(city_re_stats)

Quandl('ZILLOW/C11731_IMP') 
