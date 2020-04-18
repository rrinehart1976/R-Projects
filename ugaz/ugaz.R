# analyze ugaz 
# 1. Does UGAZ lose more value during rollover period
library(tidyverse)
library(lubridate)
setwd("C:/Users/aborst/R-SCRIPTS/ugaz")

a <- readr::read_csv('UGAZ.csv')

a %>% select(Date, Close)

