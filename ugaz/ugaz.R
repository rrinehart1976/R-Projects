# analyze ugaz 
# 1. Does UGAZ lose more value during rollover period?
library(skimr)
library(lubridate)
library(tidymodels)

#setwd("C:/Users/My Surface/Documents/R-SCRIPTS/ugaz")
setwd("C:/Users/aborst/R-SCRIPTS/ugaz")

period1 <- cbind(1,2,3,4,5,6,7,8,9,10)
period2 <- cbind(11,12,13,14,15,16,17,18,19,20)

#downloaded data from yahoo, daily for last year
a <- readr::read_csv('UGAZ.csv')
a2018 <- readr::read_csv('UGAZMarch2018-2019.csv')

b <- a %>%  
    mutate(closeDiff = (Close - lag(Close))/lag(Close), dayNo = day(Date)) %>% 
    select(Date, dayNo, closeDiff) %>% 
    na.omit() %>% 
    mutate(period_group = case_when(dayNo %in% period1 ~ "first",
                              dayNo %in% period2 ~ "second",
                              TRUE ~ "second"
                              )) %>% 
    mutate_if(is.character, as.factor)

 
  
  ggplot(filter(b,period_group=="first"), aes(groupDiff)) +
    geom_histogram()


b %>%  
  ggplot(aes(closeDiff)) +
  geom_histogram() +
  facet_wrap( ~ period_group)

skim(b)
ccc(b,closeDiff,dayNo)

b %>% t_test(closeDiff ~ period, order=c("first", "second"), alternative = "less")

lmmodel <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(closeDiff~period,data=b)
  
tidy(lmmodel)


b2018 <- a2018 %>%  
  mutate(closeDiff = (Close - lag(Close))/lag(Close), dayNo = day(Date)) %>%
  select(Date, dayNo, closeDiff) %>% 
  na.omit() %>% 
  mutate(period = case_when(dayNo %in% period1 ~ "first",
                            dayNo %in% period2 ~ "second",
                            TRUE ~ "third"))

g <- b %>% group_by(period)  %>% 
      summarize(summedDiff = sum(closeDiff), mean_diff = mean(closeDiff))
g

b2018 %>% group_by(period)  %>% 
  summarize(summedDiff = sum(closeDiff), mean_diff = mean(closeDiff))

union_all(b, b2018) %>% group_by(period)  %>% 
       summarize(summedDiff = sum(closeDiff), mean_diff = mean(closeDiff))



union_all(b, b2018) %>% filter(dayNo == 15)

