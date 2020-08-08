library(readr)
library(tidymodels)
library(glue)

report_20190314044100 <- read_delim("C:/Users/aborst/AppData/Roaming/OrderExpressDataExport/report - 20190314044100", 
                                       "\t", escape_double = FALSE, trim_ws = TRUE)
report_20200430083105 <- read_delim("C:/Users/aborst/AppData/Roaming/OrderExpressDataExport/report - 20200430083105", 
                                    "\t", escape_double = FALSE, trim_ws = TRUE)

a <- report_20200430083105 %>% 
    filter(Status=="Quoted")

prev <- report_20190314044100 %>% 
  filter(Status=="Quoted")

file_path <- glue("C:/Users/aborst/R-Scripts/PrivateData", "/OE_Quoted.csv")

write.csv(a, file=file_path)
