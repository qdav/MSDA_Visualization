#install.packages("Quandl")
#install.packages("tidyverse")

# Quinn's api_key being used here
library(Quandl)
library(tidyverse)
Quandl.api_key("awJDWs_WvUiV6VWC8rLb")

getZillowData <- function(zillow_const, zillow_area_cat, zillow_area_code, 
                          indicator_code,  new_column_name) {
  zillow_string = paste(zillow_const, zillow_area_cat,
                        zillow_area_code, "_",indicator_code,  sep="")
  #print(zillow_string)
  temp_df <- Quandl(zillow_string)
  colnames(temp_df)[colnames(temp_df) == 'Value'] <- new_column_name
  return(temp_df)
}

# month-end dates since 2010
elapsed_months <- length(seq(from=as.Date("2010-01-01"), Sys.Date(), by='month')) - 1
Date <- as.Date(as.yearmon(seq.Date(as.Date('2010-01-01'),by='month',
                                    length.out = elapsed_months)),frac=1)
date_master_df <- data.frame(Date)

# zillow re codes and column names
code_list_df <- read_csv("zillow_codes.csv")

# list to hold zillow data for each indicator
zillow_df_list <- list()
zillow_df_list[[1]] <- as.data.frame(date_master_df)

# get zillow data for San Antonio 11731
for (i in 1:nrow(code_list_df)) {   
    #print(code_list_df[i, 1])
    df = try(getZillowData("ZILLOW/", "C", "11731", 
                  code_list_df[i, 1], code_list_df[i, 2]), FALSE)
    if("try-error" %in% class(df)) df = data.frame(Date, NA)
    
    zillow_df_list[[i+1]] <- as.data.frame(df)
}

# massive left-join of indicator data frames
city_re_stats <-  zillow_df_list %>% reduce(left_join, by = "Date")

str(city_re_stats)
View(city_re_stats)
