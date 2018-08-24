library(tidyverse)
options(stringsAsFactors = FALSE)

permits <- read.csv("nics-firearm-background-checks.csv")
permits$month <- as.yearmon(permits$month)

texas_perm <- filter(permits, permits$state == "Texas")

ggplot(data = texas_perm) + 
  geom_line(mapping = aes(x = month, y = totals))


