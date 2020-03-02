library(tidyverse)
options(digits=9)
summary(Sales)
colnames(Sales)
sales2 <- Sales %>% rename(rep = "Sales Rep", location = "Company ID", brand="product Line", 
                invoiceID = "Invoice Number", documentType = "Document Type",  ordNbr = "Order Number", 
                invoiceDate = "Invoice Date", 
                custID = "Customer ID", customerName = "Customer Name", saleAmount = "Sales $") %>% 
                mutate( salesAmount = str_replace(str_replace(saleAmount, "\\(", "-"),"\\)", "")) %>%             
                mutate( salesAmount = as.character(str_replace(salesAmount, ",", ""))) %>% 
                select(rep, location, brand, invoiceID, documentType, ordNbr, invoiceDate, custID, 
                customerName, salesAmount, Cases) 

sales2$salesAmount <- as.numeric(sales2$salesAmount)

saleSummary <- sales2 %>% group_by(rep, location, brand, invoiceID, documentType, ordNbr, invoiceDate, custID, 
                                  customerName) %>% 
                 summarise(salesTotal = round(sum(salesAmount, na.rm = TRUE), 2),
                                                  caseTotal = round(sum(Cases, na.rm = TRUE)) ) %>% 
                 filter(salesTotal != 0 )            

setwd("I:/Documents/Andrew/Reports")
write.csv(saleSummary,"saleSummary20191205.csv", row.names = FALSE)
View(saleSummary[4280:4282,])

       