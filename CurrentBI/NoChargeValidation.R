library(tidyverse)
library(skimr)
prod <- read.csv(file = "PrivateData/NoChargeProd.csv")
test <- read.csv(file = "PrivateData/NoChargeTest20200816_20200825.csv")

sum(prod$Price) %>% scales::dollar(scale = 1e-3, accuracy = 0.1, suffix = "K")
count(test)
