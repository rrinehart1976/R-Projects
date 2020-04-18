#checks for duplicates in data export
library(tidyverse)
library(lubridate)

setwd("H:/Barcode Files/Dynamics Export Files")

v1 <- read.csv('Job Line Items Scheduled - Indy.csv')
v2 <- read.csv('Job Line Items Scheduled - Indy TEST.csv')

ord1 <- unique(v1$ShopOrderNumber)
ord2 <- unique(v2$ShopOrderNumber)

group_by(v2, ShopOrderNumber, line) %>% summarise(cnt = n()) %>% filter(cnt > 1)

ord1[!ord1 %in% ord2]

min(as.Date(unique(v2$ScheduledCompleteDate)))
