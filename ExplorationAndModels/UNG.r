#Builds a matrix of lagged differences for oil copper corn brlusd and bdi
library(Quandl)
library(tidyverse)
library(lubridate)


Quandl.api_key("nfJf-LHSL3gtCpXmyuX2")

#pull weekly data for each of the past 13 years 
startdate='2020-01-01'

UNG <- read_csv("R-Projects/ExplorationAndModels/UNG.csv")
UNG$Date <- as.Date(UNG$Date)
UNG$ungd1 <- c(0,diff(UNG$`Adj Close`))
v <- (UNG$`Adj Close` - lag(UNG$`Adj Close`)) / lag(UNG$`Adj Close`)
UNG$ungChange <- round(v, 3)

ng = Quandl("CHRIS/CME_NG1", collapse = "daily", start_date=startdate, order='asc')
ng$ngd1 <- c(0,diff(ng$Settle))
v <- (ng$Settle - lag(ng$Settle)) / lag(ng$Settle) 
ng$ngChange <- round(v, 3)

ng2 = Quandl("CHRIS/CME_NG2", collapse = "daily", start_date=startdate, order='asc')
ng2$ngd1 <- c(0,diff(ng2$Settle))
v <- (ng2$Settle - lag(ng2$Settle)) / lag(ng2$Settle) 
ng2$ng2Change <- round(v, 3)

View(UNG %>% filter(Date > '2020-12-31')  %>% 
  select(Date, ungChange) %>% 
  inner_join(select(ng, Date, ngChange), by="Date") %>% 
  inner_join(select(ng2, Date, ng2Change), by="Date")
)

d <- UNG %>% filter(Date > '2020-02-01' & day(Date) < 22)  %>% 
  select(Date, ungChange) %>% 
  inner_join(select(ng, Date, ngChange), by="Date") %>% 
  inner_join(select(ng2, Date, ng2Change), by="Date")

d$dayName <- weekdays(d$Date)
d$monthNo <- month(d$Date)

dmonth <- d %>% 
  group_by(monthNo) %>% 
  summarise(ng1Diff = sum(ungChange - ngChange), ng2Diff = sum(ungChange - ng2Change)) %>% 
  ungroup()

ddayName <- d %>% 
  group_by(dayName) %>% 
  summarise(ungSum = sum(ungChange),ng1Diff = sum(ungChange - ngChange), ng2Diff = sum(ungChange - ng2Change)) %>% 
  ungroup()

# > filter(w, weekNo < 5) %>% summarise(sum(week1Diff), sum(week2Diff))
# sum(week1Diff)` `sum(week2Diff)`
# 0.017            0.013s

# rolling NG contract at EOM produces strong movement in UNG 

x1 <- d %>% filter(day(Date) <= 10)
x11 <- d %>% filter(day(Date) < 22 & day(Date) > 10)

x1dayName <- x1 %>% 
  group_by(dayName) %>% 
  summarise(ungSum = sum(ungChange),ng1Diff = sum(ungChange - ngChange), ng2Diff = sum(ungChange - ng2Change)) %>% 
  ungroup()

x11dayName <- x11 %>% 
  group_by(dayName) %>% 
  summarise(ungSum = sum(ungChange),ng1Diff = sum(ungChange - ngChange), ng2Diff = sum(ungChange - ng2Change)) %>% 
  ungroup()

x21 <- UNG %>% filter(day(Date) >= 21) %>% 
  select(Date, Close, ungChange) %>% 
  inner_join(select(ng, Date, Settle, ngChange), by="Date") %>%
  inner_join(select(ng2, Date, Settle, ng2Change), by="Date")

colSums(select(d, -(c(Date))), na.rm = 'true')
colSums(select(x1, -(c(Date, weekNo))), na.rm = 'true')
colSums(select(x11, -(c(Date, weekNo))), na.rm = 'true')

View(filter(x, day(Date) %in% c(25, 26, 27,28,29,30,31,1,2)) %>% mutate(Day1 = weekdays(Date)))
     
plot(d$ungChange, d$ngChange)

write.csv(d, file='R-Projects/ExplorationAndModels/ung_output.csv')
