#Builds a matrix of lagged differences for oil copper corn brlusd and bdi
library(Quandl)
library(tidyverse)

Quandl.api_key("")

#pull weekly data for each of the past 13 years 
startdate='2005-01-01'

wti = Quandl("CHRIS/CME_CL1", collapse = "month", start_date=startdate, order='asc')
wti$lagd1 <- c(0,diff(wti$Settle))
wti$group = "wti"

brlusd = Quandl("CURRFX/BRLUSD", collapse = "month", start_date=startdate, order='asc')
brlusd$lagd1 <- c(0,diff(brlusd$Rate))
brlusd$group = "brlusd"

copper = Quandl("CHRIS/CME_HG1", collapse = "month", start_date=startdate, order='asc')
copper$lagd1 <- c(0,diff(copper$Settle))
copper$group = "copusd"
#copper = copper[2:nrow(copper),]

# bdi = Quandl("LLOYDS/BDI", collapse = "month", start_date=startdate, order='asc')
# bdi$lagd1 <- c(0,diff(bdi$Index))
# bdi$group = "bdi"
# 
corn = Quandl("CHRIS/CME_C1", collapse = "month", start_date=startdate, order='asc')
corn$lagd1 <- c(0,diff(corn$Settle))
corn$group = "corn"

wti$oil <- round(wti$lagd1 / lag(wti$Settle),2) * 100
brlusd$brl <- round(brlusd$lagd1 / lag(brlusd$Rate),2) * 100
copper$cop <- round(copper$lagd1 / lag(copper$Settle),2) * 100
#bdi$bdi <- round(bdi$lagd1 / lag(bdi$Index),2) * 100
corn$corn <- round(corn$lagd1 / lag(corn$Settle),2) * 100

d1 <- select(wti, Date, oil) %>% 
    # left_join(select(brlusd,Date,brl)) %>% 
    left_join(select(copper,Date,cop)) %>% 
    # left_join(select(bdi,Date,bdi)) %>% 
    left_join(select(corn,Date,corn)) %>% 
    filter(!is.na(oil))

x <- table(d1$oil)/length(d1$oil)

df2 <- as.data.frame(x) %>% mutate(event = as.numeric(levels(Var1)))

d1 %>%  ggplot(aes(oil)) +
  geom_bar() +
  labs(title = "Price of Nearest WTI Contract Month-on-month difference", subtitle = "Binomial Distribution") +
  xlab("Diff from previous month") +
  ylab("Frequency")

# Position

barplot(df2$Freq, names.arg = df2$event)

quantile(d1[,2])
class(table(d1$oil))
View(table(d1$oil))
filter(d1, oil == 88)
