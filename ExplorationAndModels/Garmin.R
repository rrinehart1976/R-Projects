library(tidyquant)
library(skimr)
library(tidyverse)

gp <- read_csv("C:/Users/aborst/Documents/R-Projects/CurrentBI/PrivateData/Garmin.csv")

p <- gp %>% 
  filter(year(Date) == 2020) %>% 
  mutate(weekNo = week(Date)) %>% 
  group_by(weekNo) %>% 
  summarise(mileage = sum(Distance)) %>% 
  ungroup()

ggplot(p, aes(weekNo,mileage)) +
  geom_line() +
  geom_ma(ma_fun = SMA, n=5, color = "red") + 
  theme_bw() +
  theme(panel.ontop = TRUE, panel.background = element_rect(color = NA, fill = NA))

