library(tidyverse)
library(tidymodels)
library(skimr)
library(lubridate)
source(file = "Data_Access/database_functions.R")

conn <- sql01_con("Coin")

db_list_tables(conn) %>% .[matches("eek",vars=.)]
#   
# Weeks <- tbl(conn,"Weeks") %>%
#   collect()
LineMap <- tbl(conn,"ddfJobType") %>%
  select(JobPrefix, ProductLine) %>%
  mutate(JobPrefix = str_trim(JobPrefix)) %>% 
  filter(!is.na(ProductLine)) %>% 
  collect()

weekRange <- c(seq(isoweek(today()), isoweek(today()) + 8), 99)

vw <- tbl(conn,"vwReportKeyData") %>% 
  select(Brand, ScheduledCompleteDate, Plant, ReceivedDate, OrderCaseTotal, ShopFloorNumber, OrderTotal) %>% 
  collect()

t <- vw %>%
  filter(!is.na(ReceivedDate))  %>% 
  mutate(JobPrefix = str_sub(ShopFloorNumber, 1, 2)) %>%
  mutate(weekNo = if_else(year(ScheduledCompleteDate) == 2001, 99, isoweek(ScheduledCompleteDate))) 
  

g <- inner_join(t,LineMap,by="JobPrefix") %>% 
  select(weekNo, OrderCaseTotal,OrderTotal, Brand, Plant, ProductLine) %>% 
  filter(weekNo %in% weekRange) %>% 
  group_by(weekNo, Brand, Plant, ProductLine) %>% 
  summarise(Cases = sum(OrderCaseTotal), Dollars = sum(OrderTotal)) %>% 
  ungroup()

d <- g %>% filter(Plant=="Elkins") %>% 
  pivot_wider(names_from = c("Brand", "ProductLine"), values_from = c("Cases", "Dollars"))
              