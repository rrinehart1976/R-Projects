library(tidyverse)
library(lubridate)

setwd("I:/Documents/Andrew/Reports")
#this list was given to Mindy
df <- read.csv('SitelineRS_YTD.csv')



g1 <- df %>%  mutate(year = year(as.Date(ImportStartDT)), month = month(as.Date(ImportStartDT)))   %>% 
        group_by(Description.1) %>% 
        summarize(usd = sum(NetPrice)) 

write.table(as.data.frame(filter(g1, str_detect(Description.1,'Production'))), "clipboard", sep="\t")
write.table(as.data.frame(filter(g1, str_detect(Description.1,'Truck'))), "clipboard", sep="\t")
write.table(as.data.frame(filter(g1, str_detect(Description.1,'Material'))), "clipboard", sep="\t")
write.table(as.data.frame(filter(g1, str_detect(Description.1,'Engineer'))), "clipboard", sep="\t")
write.table(filter(df, str_detect(Description.1, 'Production Engineering')), "clipboard", sep="\t")

g <- df %>%  mutate(year = year(as.Date(ImportStartDT)), month = month(as.Date(ImportStartDT)))   %>% 
  group_by(Code, year, month) %>% 
  summarize(freqRS=n()) 

g <- df %>%  mutate(year = year(as.Date(ImportStartDT)), month = month(as.Date(ImportStartDT)))   %>% 
  group_by(SKU) %>% 
  summarize(usd = sum(NetPrice), freqN=n()) 

write.table(filter(df, SKU == 'DRSHW'), "clipboard", sep="\t", row.names = FALSE)
               
filter(g) %>% 
# filter(g, Code == 'RS-100-01') %>% 
  ggplot(aes(x=as.factor(month), y=freqRS)) + 
    geom_bar(stat="identity")
    + ggtitle("Siteline RS Reasons") 
    + xlab(label = "Month") 
    + ylab("Counts") 
