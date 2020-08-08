# --Greenfield 2020 order quote status 
# if object_id('tempdb..#lastQuote') is not null drop table #lastQuote;
# select b.orderid, min(b.EffDT) as statusDate, max(b.OrderStatusID) as orderStatus, max(c.PackageRunID) as prID
# into #lastQuote
# from GP6Local.dbo.ord_Order b
# inner join GP6Local.dbo.PackageRuns e on b.PackageRunID = e.PackageRunID
# inner join PackageRuns f on e.SyncRunID = f.SyncRunID and f.PackageName = '02_ord_OrderCalcResult'
# inner join 
# ord_OrderCalcResult c on b.OrderID = c.OrderID --eliminate orders with no price
# and c.PackageRunID = f.PackageRunID 
# and c.PriceMultiplierType = 'corsi'
# and c.NetFinalPrice IS NOT NULL
# where b.OrderJobName NOT LIKE '%test%' 
# and b.OrderStatusID IN ('quote')
# and year(b.effdt) = 2020
# and b.IsRNumber = 0 
# group by b.OrderID
# 
# if object_id('tempdb..#subQuote') is not null drop table #subQuote;
# select a.OrderID, a.orderStatus, a.statusDate as FirstQuoteDate, 
# b.OrderLastSubmittedDate, 
# c.NetFinalPrice as QuoteFinalPrice, d.NetFinalPrice, 
# DATEDIFF(day, a.statusDate, b.OrderLastSubmittedDate) as DaysDiff
# into #subQuote
# from #lastQuote a
# inner join 
# (select a.OrderID, max(a.OrderLastSubmittedDate) as OrderLastSubmittedDate	
#   from #lastQuote
#   inner join ord_Order a on #lastQuote.OrderID = a.OrderID
#   group by a.orderid) b
# on a.orderid = b.orderid
# inner join 
# ord_OrderCalcResult c 
# on a.OrderID = c.OrderID and a.prID = c.PackageRunID and c.PriceMultiplierType = 'Corsi'
# inner join 
# ord_OrderCalcResult d 
# on a.OrderID = d.OrderID and d.CurInd = 1 and d.PriceMultiplierType = 'Corsi' 
# 
# select a.OrderShopNum, #subQuote.*
# from ord_Order a
# inner join #subQuote on a.OrderID = #subQuote.OrderID
# where a.CurInd = 1

library(tidymodels)
library(stringr)
library(lubridate)
library(skimr)

gp <- read_csv("C:/Users/aborst/R-Scripts/PrivateData/GF_Quotes2020.csv")

source(file = "Data_Access/database_functions.R")

v <- sql01_view("Coin", "vwReportKeyData")


coin_submitted <- v %>% 
    filter(Brand=="Greenfield") %>% 
    filter(!str_detect(JobNumber, "[Tt]est"))  %>% 
    filter(!is.na(SubmittedDate))

gp2 <- gp %>% 
  filter(QuoteFinalPrice<150000) %>% 
  mutate(weekNo = isoweek(FirstQuoteDate)) %>% 
  group_by(weekNo) %>% 
  summarise(orderTotal = sum(QuoteFinalPrice)) %>% 
  mutate(status = "Quote")

gp_submitted <- gp %>% 
  filter(OrderLastSubmittedDate != "NULL") %>% 
  filter(QuoteFinalPrice<150000) %>% 
  mutate(weekNo = isoweek(OrderLastSubmittedDate)) %>% 
  group_by(weekNo) %>% 
  summarise(orderTotal = sum(NetFinalPrice)) %>% 
  mutate(status = "Submitted")


gpplot <- union_all(gp2, gp_submitted)

ggplot(data = gpplot, aes(weekNo, orderTotal, fill=status)) +
  geom_bar(stat="identity", position ="dodge") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) 

gp1 <- gp %>% 
  filter(OrderLastSubmittedDate != "NULL") %>%
  filter(NetFinalPrice > 5000) %>% 
  mutate(DaysN = as.integer(DaysDiff))

skim(gp1$DaysN)
