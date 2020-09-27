library(tidyverse)
library(skimr)

prod <- read.csv(file = "PrivateData/NoChargeProd.csv")
test <- read.csv(file = "PrivateData/No ChargeTest_2.csv")

count(prod)
count(test)

#look at Trish changes for addresses in DSL
source(file = "Data_Access/database_functions.R")

soheader <- sql01_table("Corsi1App", "SOHeader")
soshipheader <- sql01_table("Corsi1App", "SOShipHeader")
# con <- dbConnect(RSQLite::SQLite(), "00_data/bikes_database.db")

header <- soheader %>% select(OrdNbr,ShipAttn,ShipAddr1,ShipZip, Crtd_DateTime) %>% filter(Crtd_DateTime > "2020-06-01")
shipheader <- soshipheader %>% select(OrdNbr,ShipAttn,ShipAddr1,ShipZip, Crtd_DateTime) %>% filter(OrdNbr %in% soheader$OrdNbr )

rpt <- inner_join(header, shipheader, by="OrdNbr") %>% 
    filter(ShipAddr1.x != ShipAddr1.y )

