#install.packages("Quandl")

# Quinn's api_key being used here
Quandl.api_key("awJDWs_WvUiV6VWC8rLb")
library(Quandl)

#creating a list of all-inclusive dates from which to left-join
Date <- as.Date(as.yearmon(seq.Date(as.Date('2010-01-01'),by='month',
                                    length.out = 108)),frac=1)
date_master_df <- data.frame(Date)

mydata1 <- Quandl('ZILLOW/C11731_PRRAH') 
colnames(mydata1)[colnames(mydata1) == 'Value'] <- 'price_rent_ratio'
mydata2 <- Quandl('ZILLOW/C11731_MLPFAH') 
colnames(mydata2)[colnames(mydata2) == 'Value'] <- 'med_list_price_sf'
mydata3 <- Quandl('ZILLOW/C11731_MLPAH') 
colnames(mydata3)[colnames(mydata3) == 'Value'] <- 'median_list_price'
mydata4 <- Quandl('ZILLOW/C11731_MSPFSF') 
colnames(mydata4)[colnames(mydata4) == 'Value'] <- 'median_sold_price_sfh'


city_re_stats <- list(date_master_df, mydata1, mydata2, mydata3, mydata4) %>% 
                      reduce(left_join, by = "Date")

str(city_re_stats)
View(city_re_stats)

