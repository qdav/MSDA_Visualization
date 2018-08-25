#install.packages("Quandl")

# Quinn's api_key being used here
Quandl.api_key("awJDWs_WvUiV6VWC8rLb")
library(Quandl)

#mydata = Quandl("FRED/GDP")
mydata = Quandl('ZILLOW/C25709_ZRISFRR')
View(mydata)

