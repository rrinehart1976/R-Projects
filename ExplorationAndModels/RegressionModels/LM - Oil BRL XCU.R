#fit oil linear regression model with brlusd and copper as predictors 
library(Quandl)
library(dplyr)
library(reshape2)
library(lubridate)
library(tidyr)
library(ggplot2)
#montly diff
Quandl.api_key("")

source(file = "Shared/Common_functions.R")

startdate <-  "2015-09-01"

wti <-  Quandl("CHRIS/CME_CL1", collapse = "week", start_date=startdate)
wti$diff <-  c(wti$Settle[1:(nrow(wti)-1)] - wti$Settle[2:nrow(wti)],0)
wti$group <-  "wti"
#wtim = wtim[2:nrow(wtim),]

# brlusd = Quandl("CURRFX/BRLUSD", collapse = "week", start_date=startdate)
# brlusd$diff = c(brlusd$Rate[1:(nrow(brlusd)-1)] - brlusd$Rate[2:nrow(brlusd)],0)
# brlusd$group = "brlusd"

copper  <-  Quandl("CHRIS/CME_HG1", collapse = "week", start_date=startdate)
copper$diff  <-  c(copper$Settle[1:(nrow(copper)-1)] - copper$Settle[2:nrow(copper)],0)
copper$group <-  "copusd"
#copm = copm[2:nrow(copm),]

# bdi = Quandl("LLOYDS/BDI", collapse = "week", start_date=startdate)
# bdi$diff = c(bdi$Index[1:(nrow(bdi)-1)] - bdi$Index[2:nrow(bdi)],0)
# bdi$group = "bdi"


# wti = filter(wti, wti$Date %in% bdi$Date) 

#copm = copm[2:nrow(copm),]
m <- inner_join(wti, copper, by = "Date") %>% 
  select(Date, oil = Settle.x, copper = Settle.y)

#Plot the data to look for multivariate outliers, non-linear relationships etc
m %>% ggplot(aes(oil,copper)) +
    geom_point() +
    geom_jitter() +
    geom_smooth() +
    facet_wrap(~month(Date))

mnorm  <-  m %>%  mutate(oilnorm = normalize(oil), coppernorm = normalize(copper)) 

mnorm %>% 
  ggplot(aes(Date, oilnorm)) +
  geom_line() + 
  geom_point() +
  geom_line(data = mnorm, aes(Date,coppernorm), color = "red") +
  facet_wrap(~month(Date))
  
cor(select(m, oil, copper))
#fit oil model with copper as predictors (aka x-variable, explanatory variables)
oilmod = lm(oil ~ copper, data = m)
summary(oilmod)
#r^2 is 0.71 so around 70% of variation in the price of oil can be accounted for by brlusd and copper
#F-stat tests whether slope is zero 
oilpred = predict(oilmod)
gdata = melt(data.frame(data1$oil, wti$Date, oilpred),id="wti.Date")

ggplot(data=gdata,
       aes(x=wti.Date, y=value, colour=variable)) +
  geom_line(aes(linetype=variable)) +  
  geom_point()
# theme(panel.grid.minor = element_line(colour="blue", size=0.5)) + 
# scale_x_continuous(minor_breaks = seq(1, 10, 0.5))
# 
oilres = resid(oilmod)
plot(m$oil, oilres)
abline(0, 0)                  # the horizon
oilrst = rstandard(oilmod)
plot(m$oil,oilrst)
qqnorm(oilrst)
# 
# cor(data1$copper, data1$brlusd, method="pearson")
confint(oilmod, conf.level=0.95)
