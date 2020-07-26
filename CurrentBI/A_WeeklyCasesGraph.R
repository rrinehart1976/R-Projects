##:::::::::::::::::::::::::::::::::
##  Graphs weekly cases counts and average cabinet price.
##  Andrew Borst 
##  Last Update 5/27/20
##:::::::::::::::::::::::::::::::::
library(tidyverse)
library(tidyquant)
library(readr)
library(lubridate)
library(reshape2)
source(file = "Data_Access/database_functions.R")

v <- sql01_view("Coin", "vwReportKeyData")

v$SubmittedDate <- as.Date(v$SubmittedDate)

caseorders <- v %>% 
    select(JobNumber, SubmittedDate, OrderCaseTotal, AdjustPrice, OrderTotal, Brand, ProductLine, Overlay) %>% 
    filter(OrderCaseTotal > 0) %>%
    filter(OrderCaseTotal != "NULL") %>%  
    filter(year(SubmittedDate) == 2020) %>% 
    mutate(OrderCaseTotal = as.integer(OrderCaseTotal)) %>% 
    mutate(weekNo = case_when(is.na(SubmittedDate) ~ 99, TRUE ~ isoweek(SubmittedDate))) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>% 
    mutate(NetPrice = OrderTotal + AdjustPrice)

weeklyBrand <- caseorders %>% 
    group_by(weekNo, Brand) %>% 
    summarise(Cases = sum(OrderCaseTotal), CaseAverage = sum(NetPrice)/sum(OrderCaseTotal)) 

weeklyBoth <- caseorders %>% 
  group_by(weekNo) %>% 
  summarise(Cases = sum(OrderCaseTotal), CaseAverage = sum(NetPrice)/sum(OrderCaseTotal)) %>% 
  mutate(Brand = "Both")  
  

ggplot(weeklyBrand, aes(weekNo, Cases)) +
  geom_bar(aes(weekNo, Cases, fill=Brand), stat="identity") +
  geom_line(aes(weekNo, CaseAverage, color=Brand)) +
  geom_line(data=weeklyBoth, aes(weekNo, CaseAverage)) 
  
ggplot(weeklyBoth, aes(weekNo, Cases)) +
  geom_bar(aes(weekNo, Cases), stat="identity") +
  geom_line(aes(weekNo, CaseAverage)) +
  geom_line(data=weeklyBoth, aes(weekNo, CaseAverage)) +
  geom_ma(ma_fun = SMA, n=5, color = "red") + 
  theme_bw() +
  theme(panel.ontop = TRUE, panel.background = element_rect(color = NA, fill = NA))


