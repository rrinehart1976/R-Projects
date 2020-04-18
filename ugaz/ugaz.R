# analyze ugaz 
# 1. Does UGAZ lose more value during rollover period
library(tidyverse)
library(lubridate)
setwd("C:/Users/My Surface/Documents/ugaz")

a <- readr::read_csv('UGAZ.csv')
a %>% select(a, 
