library(tidyverse)
library(lubridate)
library(odbc)
source(file = "Data_Access/database_functions.R")

con <- sql01_con("Corsi1App")

l <- db_list_tables(con)

str_subset(l, pattern = "AP")

dbListFields(con, "Vendor") %>% str_subset(pattern = "Id")
dbListFields(con, "APHist") %>% str_subset(pattern = "Id")

dbGetQuery(con, 'select * from Vendor') %>% 
  filter(str_detect(Name, "rontier"))

vend <- dbGetQuery(con, 'select * from APHist') %>% 
  filter(str_detect(VendId, 'GF0003'))
 
