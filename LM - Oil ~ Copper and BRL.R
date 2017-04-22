library(Quandl)
library(reshape2)
library(lubridate)
library(tidyr)
library(ggplot2)
??Quandl
#montly diff
startdate = "2015-09-01"
enddate = "2012-01-01"
wtim = Quandl("CHRIS/CME_CL1", collapse = "week", start_date=startdate)
wtim$diff = c(wtim$Settle[1:(nrow(wtim)-1)] - wtim$Settle[2:nrow(wtim)],0)
wtim$group = "wti"
#wtim = wtim[2:nrow(wtim),]

brlm = Quandl("CURRFX/BRLUSD", collapse = "week", start_date=startdate)
brlm$diff = c(brlm$Rate[1:(nrow(brlm)-1)] - brlm$Rate[2:nrow(brlm)],0)
brlm$group = "brlusd"
#brlm = brlm[2:nrow(brlm),]

copm = Quandl("CHRIS/CME_HG1", collapse = "week", start_date=startdate)
copm$diff = c(copm$Settle[1:(nrow(copm)-1)] - copm$Settle[2:nrow(copm)],0)
copm$group = "copusd"
#copm = copm[2:nrow(copm),]

data1 = data.frame(oil = wtim[,c("Settle")], brlusd = brlm[,c("Rate")], copper = copm[,c("Settle")] )
#Plot the data to look for multivariate outliers, non-linear relationships etc
plot(data1)
cor(data1)
#fit oil model with brlusd and copper as predictors (aka x-variable, explanatory variables)
oilmod = lm(oil ~ brlusd + copper, data = data1)
summary(oilmod)
oilbrl = lm(oil ~ brlusd, data = data1)
summary(oilbrl)
oilcop = lm(oil ~ copper, data = data1)
summary(oilcop)
#r^2 is 0.71 so around 70% of variation in the price of oil can be accounted for by brlusd and copper
#F-stat tests whether slope is zero 
# par(mfrow=c(2,2))
# plot(oilmod)
# par(mfrow=c(1,1))
oilcop = predict(oilcop)
gdata1 = melt(data.frame(data1$oil, wtim$Date, oilcop),id="wtim.Date")

oilbrl = predict(oilbrl)
gdata2 = melt(data.frame(data1$oil, wtim$Date, oilbrl),id="wtim.Date")

oilpred = predict(oilmod)
gdata3 = melt(data.frame(data1$oil, wtim$Date, oilpred),id="wtim.Date")
gdata = rbind(gdata1,gdata2,gdata3) 
ggplot(data=gdata,
       aes(x=wtim.Date, y=value, colour=variable)) +
  geom_line(aes(linetype=variable)) +  
  geom_point()
# theme(panel.grid.minor = element_line(colour="blue", size=0.5)) + 
# scale_x_continuous(minor_breaks = seq(1, 10, 0.5))
# 
# oilres = resid(oilmod)
# plot(data1$oil, oilres)
# abline(0, 0)                  # the horizon
# abline(oilres)
# # oilrst = rstandard(oilmod)
# # plot(data1$oil,oilrst)
# qqnorm(oilrst)
# 
# cor(data1$copper, data1$brlusd, method="pearson")
# confint(oilmod, conf.level=0.95)
