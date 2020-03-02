library(tidyverse)

# setwd("~/SL_upgrade/CommissionsIT")
setwd("I:/Documents/Andrew/Commissions")
# c12 <- read.csv("CommissionExport201812_Check.csv", strip.white = TRUE)
comm1 <- read.csv("CommissionExport1909.csv", strip.white = TRUE, stringsAsFactors = FALSE)

colnames(comm1)[1] <- "slsperid"

View(filter(comm1, slsperid == "          "))

# comm1s <- comm1 %>%
#   mutate(slsperid = case_when
#           (refnbr == "I0003200  " ~ "Test Dealer",
#             TRUE ~ slsperid)) %>%
#   mutate(name = case_when
#          (refnbr == "I0003200  " ~ "Test Dealer 2",
#            TRUE ~ name)) %>% 
#   mutate(slsperid = case_when
#        (custid == "53072" ~ "GTL",
#          TRUE ~ slsperid)) %>%
#   mutate(name = case_when
#          (custid == "53072" ~ "Test Dealer 3",
#            TRUE ~ name)) %>% 
#   mutate(slsperid = case_when
#        (custid == "53052" ~ "Test Dealer 4",
#          TRUE ~ slsperid)) %>%
#   mutate(name = case_when
#          (custid == "53052" ~ "Test Dealer 5",
#            TRUE ~ name))

# View(filter(comm1s, slsperid == "          "))
# View(filter(comm1s, refnbr == "I0003200  "))
# View(filter(comm1s, custid == "53072"))

#             slsperid == "          " ~ "Test Dealer 1",
# a <- filter(c12, str_detect(name, 'Dealer 5')) %>% 
#   select(slsperid, trantype, refnbr, invtid, trandate, tranamt, drcr, acct, ordnbr)

# filter(a, ordnbr == 'GA14432' & refnbr == 'I0000903') %>% group_by(drcr) %>% 
#   summarise(orderTotal = round(sum(tranamt),2))

g <- comm1 %>%  
  mutate(tranamt = case_when(drcr == "D" ~ -tranamt, TRUE ~ tranamt)) %>% 
  select(cpnyid, slsperid, name, custid, ordnbr, refnbr, trantype, trandate, CustomerName, DealerName, tranamt) %>% 
  group_by(name, cpnyid, refnbr, trantype, trandate, custid, slsperid, ordnbr, CustomerName, DealerName)

commSummary <- summarise(g, salesTotal = round(sum(tranamt), 2)) %>% 
  filter(salesTotal != 0 || slsperid == "HA")            


commSummary$ordnbr <- str_trim(commSummary$ordnbr)
commSummary$slsperid <- str_trim(commSummary$slsperid)
commSummary$name <- str_trim(commSummary$name)

write.csv(commSummary,"commSummary201909.csv", row.names = FALSE)


View(filter(c12, str_detect(ordnbr, 'GR52023')))
filter(c01s, str_detect(refnbr,'873070'))
filter(c, slsperid == 'KRE')
head(c)
unique(c$ordnbr)
