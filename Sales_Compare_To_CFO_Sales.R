# Compare sales from order entry data to CFO spreadsheet
library(tidyverse)
library(readr)
library(skimr)

DealerSubmitted2018 <- read_csv("C:/Users/aborst/R-Scripts/PrivateData/DealerSubmitted2018.csv")

CFODealerSubmitted <- read_csv("C:/Users/aborst/R-Scripts/PrivateData/0420 Incoming Order Analysis.xlsx")

aj <- anti_join(DealerSubmitted2018, CFODealerSubmitted, by="ShopOrderNumber")

View(aj)

View(filter(DealerSubmitted2018, str_sub(ShopOrderNumber, 1, 9)=="SO1803339"))
filter(CFODealerSubmitted, ShopOrderNumber=="SO1801140")

#check excluded orders from invoices
library(readxl)
Invoice_2018 <- read_excel("C:/Users/aborst/R-Scripts/PrivateData/Seradex/Invoice_2018.xls")
db_Q1_Q2 <- DealerSubmitted2018 %>%
    mutate(JobNumber = str_sub(ShopOrderNumber, 1, 9)) %>% 
    select(JobNumber, DealerTotal, SubmittedDate) %>% 
    filter(SubmittedDate < "2018-07-01") %>% 
    group_by(JobNumber) %>% 
    summarise(DealerJobTotal = sum(DealerTotal))

inv <- Invoice_2018 %>%
  rename(JobNumber = "Sales Order", DealerPrice = "Extended Price", InvoiceNumber = "Document No" ) 
  
inv_group <- Invoice_2018 %>%
  select(JobNumber = "Sales Order", DealerPrice = "Extended Price" ) %>% 
  group_by(JobNumber) %>% h
  summarise(DealerTotalInvoiced = sum(DealerPrice))

filter(db_Q1_Q2, JobNumber %in% inv_group$JobNumber)
anti_join(db_Q1_Q2, inv_group, by="JobNumber")
inner_join(db_Q1_Q2, inv_group, by="JobNumber")
filter(inner_join(db_Q1_Q2, inv_group, by="JobNumber"), JobNumber == "SO1803339")
View(filter(inv, DealerPrice == 250))
