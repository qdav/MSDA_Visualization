#install.packages("Quandl")

# Quinn's api_key being used here
Quandl.api_key("awJDWs_WvUiV6VWC8rLb")
library(Quandl)
library(tidyverse)

#creating a list of all-inclusive dates from which to left-join
Date <- as.Date(as.yearmon(seq.Date(as.Date('2010-01-01'),by='month',
                                    length.out = 108)),frac=1)
date_master_df <- data.frame(Date)

# experiments to do this in a loop
zillow_code <- c('IMP', 'IMSP', 'LPCSAL')
zillow_code_column <- c('inv_measure', 'inv_measure_ssa', 'list_price_cut_all_homes')
code_list_df <- data.frame(zillow_code, zillow_code_column)
#View(code_list_df)

output <- vector("double", ncol(code_list_df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
  output[[i]]      # 3. body
}
output


# code C11731 refers to the city of San Antonio (good to double-check)

mydata1 <- Quandl('ZILLOW/C11731_IMP') 
colnames(mydata1)[colnames(mydata1) == 'Value'] <- 'inv_measure'

mydata2 <- Quandl('ZILLOW/C11731_IMSP') 
colnames(mydata2)[colnames(mydata2) == 'Value'] <- 'inv_measure_ssa'

mydata3 <- Quandl('ZILLOW/C11731_LPCSAL') 
colnames(mydata3)[colnames(mydata3) == 'Value'] <- 'list_price_cut_all_homes'

mydata4 <- Quandl('ZILLOW/C11731_MLPAH') 
colnames(mydata4)[colnames(mydata4) == 'Value'] <- 'med_list_price_all'

mydata5 <- Quandl('ZILLOW/C11731_MLPCC') 
colnames(mydata5)[colnames(mydata5) == 'Value'] <- 'med_list_price_condo'

mydata6 <- Quandl('ZILLOW/C11731_MLPDT') 
colnames(mydata6)[colnames(mydata6) == 'Value'] <- 'med_list_price_duplex'

mydata7 <- Quandl('ZILLOW/C11731_MLP5B') 
colnames(mydata7)[colnames(mydata7) == 'Value'] <- 'med_list_price_5b'

mydata8 <- Quandl('ZILLOW/C11731_MLP4B') 
colnames(mydata8)[colnames(mydata8) == 'Value'] <- 'med_list_price_4b'

mydata9 <- Quandl('ZILLOW/C11731_MLP3B') 
colnames(mydata9)[colnames(mydata9) == 'Value'] <- 'med_list_price_3b'

mydata10 <- Quandl('ZILLOW/C11731_MLP2B') 
colnames(mydata10)[colnames(mydata10) == 'Value'] <- 'med_list_price_2b'

mydata11 <- Quandl('ZILLOW/C11731_MLP1B') 
colnames(mydata11)[colnames(mydata11) == 'Value'] <- 'med_list_price_1b'

mydata12 <- Quandl('ZILLOW/C11731_MLPSF') 
colnames(mydata12)[colnames(mydata12) == 'Value'] <- 'med_list_price_sing_fam'

mydata13 <- Quandl('ZILLOW/C11731_MLPFAH') 
colnames(mydata13)[colnames(mydata13) == 'Value'] <- 'med_list_price_sf_all'

mydata14 <- Quandl('ZILLOW/C11731_MLPFCC') 
colnames(mydata14)[colnames(mydata14) == 'Value'] <- 'med_list_price_sf_condo'

mydata15 <- Quandl('ZILLOW/C11731_MLPFDT') 
colnames(mydata15)[colnames(mydata15) == 'Value'] <- 'med_list_price_sf_duplex'

mydata16 <- Quandl('ZILLOW/C11731_MLPF5B') 
colnames(mydata16)[colnames(mydata16) == 'Value'] <- 'med_list_price_sf_5b'

mydata17 <- Quandl('ZILLOW/C11731_MLPF4B') 
colnames(mydata17)[colnames(mydata17) == 'Value'] <- 'med_list_price_sf_4b'

mydata18 <- Quandl('ZILLOW/C11731_MLPF3B') 
colnames(mydata18)[colnames(mydata18) == 'Value'] <- 'med_list_price_sf_3b'

mydata19 <- Quandl('ZILLOW/C11731_MLPF2B') 
colnames(mydata19)[colnames(mydata19) == 'Value'] <- 'med_list_price_sf_2b'

mydata20 <- Quandl('ZILLOW/C11731_MLPF1B') 
colnames(mydata20)[colnames(mydata20) == 'Value'] <- 'med_list_price_sf_1b'

mydata21 <- Quandl('ZILLOW/C11731_MLPFSF') 
colnames(mydata21)[colnames(mydata21) == 'Value'] <- 'med_list_price_sf_sing_fam'

city_re_stats <- list(date_master_df, 
                      mydata1, mydata2, mydata3, mydata4, 
                      mydata5, mydata6, mydata7, mydata8,
                      mydata9, mydata10, mydata11, mydata12,
                      mydata13, mydata14, mydata15, mydata16,
                      mydata17, mydata18, mydata19, mydata20,
                      mydata21 ) %>% reduce(left_join, by = "Date")

str(city_re_stats)
View(city_re_stats)

Quandl('ZILLOW/C11731_MRPAH') 
