#install.packages("Quandl")

# Quinn's api_key being used here
Quandl.api_key("awJDWs_WvUiV6VWC8rLb")
library(Quandl)
library(tidyverse)

#creating a list of all-inclusive dates from which to left-join
Date <- as.Date(as.yearmon(seq.Date(as.Date('2010-01-01'),by='month',
                                    length.out = 108)),frac=1)
date_master_df <- data.frame(Date)

# vectors to hold Zillow codes and new column names
zillow_code <- c('IMP', 'IMSP', 'LPCSAL', 'MLPAH')
zillow_code_column <- c('inv_measure', 'inv_measure_ssa', 
                        'list_price_cut_all_homes', 'med_list_price_all')
code_list_df <- data.frame(zillow_code, zillow_code_column)


output <- list()
output[[1]] <- as.data.frame(date_master_df)

for (i in 1:nrow(code_list_df)) {            
  print(code_list_df[i, 1])
  # code C11731 refers to the city of San Antonio (good to double-check)
  zillow_c = paste('ZILLOW/C11731_', code_list_df[i, 1], sep="")
  temp_df <- Quandl(zillow_c)
  colnames(temp_df)[colnames(temp_df) == 'Value'] <- as.character (code_list_df[i, 2])
  output[[i+1]] <- as.data.frame(temp_df)
}

city_re_stats <-  output %>% reduce(left_join, by = "Date")

str(city_re_stats)
View(city_re_stats)

#Quandl('ZILLOW/C11731_MRPAH') 
