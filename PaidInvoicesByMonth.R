library(tidyverse)
library(lubridate)
# setwd("~/SL_upgrade/CommissionsIT")
setwd("I:/Documents/Andrew/Reports")
# c12 <- read.csv("CommissionExport201812_Check.csv", strip.white = TRUE)
comm1 <- read.csv("Invoice_2019_YTD.csv", strip.white = TRUE, stringsAsFactors = FALSE)

colnames(comm1)[1] <- "slsperid"


# comm1s <- comm1 %>%
#   mutate(slsperid = case_when
#           (refnbr == "I0003200  " ~ "CRE",
#             TRUE ~ slsperid)) %>%
#   mutate(name = case_when
#          (refnbr == "I0003200  " ~ "CRESCENT MARKETING GROUP, INC.",
#            TRUE ~ name)) %>% 
#   mutate(slsperid = case_when
#        (custid == "53072" ~ "GTL",
#          TRUE ~ slsperid)) %>%
#   mutate(name = case_when
#          (custid == "53072" ~ "GTL Associates",
#            TRUE ~ name)) %>% 
#   mutate(slsperid = case_when
#        (custid == "53052" ~ "SRP",
#          TRUE ~ slsperid)) %>%
#   mutate(name = case_when
#          (custid == "53052" ~ "Atlantic Sales and Marketing, Inc",
#            TRUE ~ name))

# View(filter(comm1s, slsperid == "          "))
# View(filter(comm1s, refnbr == "I0003200  "))
# View(filter(comm1s, custid == "53072"))

#             slsperid == "          " ~ "CRE",
# a <- filter(c12, str_detect(name, 'Atlantic')) %>% 
#   select(slsperid, trantype, refnbr, invtid, trandate, tranamt, drcr, acct, ordnbr)

# filter(a, ordnbr == 'GA14432' & refnbr == 'I0000903') %>% group_by(drcr) %>% 
#   summarise(orderTotal = round(sum(tranamt),2))

g <- comm1 %>%  
  mutate(tranamt = case_when(drcr == "D" ~ -tranamt, TRUE ~ tranamt)) %>% 
  select(cpnyid, slsperid, name, custid, ordnbr, refnbr, trantype, trandate, CustomerName, DealerName, tranamt, perclosed) %>% 
  group_by(name, cpnyid, refnbr, trantype, trandate, custid, slsperid, ordnbr, CustomerName, DealerName, perclosed)

commSummary <- summarise(g, salesTotal = round(sum(tranamt), 2)) %>% 
  filter(salesTotal != 0)            


month2019 <- commSummary %>% group_by(perclosed) %>% 
    summarise(monthSales = sum(salesTotal)) %>% 
    select(perclosed, monthSales)
    
month2019 %>% 
ggplot(aes(x=perclosed, y=monthSales)) +
       geom_bar(stat="identity")
