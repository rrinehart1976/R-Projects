#January 2018 a new version of Siteline XML (salesperson renamed designer, requestedshipdate renamed ship_date. 
library(xml2)
library(dplyr)
library(stringr)
dir1 <- 'C:/Users/aborst/Documents/R/SL2018/'
lf <- list.files(dir1)
processed <-  lf[which(str_count(lf, '-P')==1)]
groupNumber <- str_remove(str_extract(processed, '-G\\d+'), '-G')
if(max(as.numeric(groupNumber))>26) {
  groups <- paste0("A", LETTERS[as.numeric(groupNumber)- 26]) 
} else {
  groups <- LETTERS[as.numeric(groupNumber)]  
}

reportNames <- c("OrderNumber", "OrderDate", "ProcessedDate", "Status", "Cases", "Dollars", "JobName", "Designer",  
              "ShipDate", "CustomerAccount", "CustomerName", "QuoteType", "Construction", "DoorStyle", "Material", "Finish",
              "SpecialInstructions", "FileName")

df <- data.frame(matrix(ncol = length(reportNames), nrow = 0))
colnames(df) <- reportNames
i <- 1
while(i<length(processed)){
#while(i<10) {
  x <- read_xml(paste0(dir1, processed[i]))
  df[i,]$FileName <- processed[i]  
  # df[i,j] <-
  totalGroups <- as.character(xml_contents(xml_find_all(x, paste0(".//", "Total_Groups"))))
  if(length(totalGroups)==0){ 
    totalGroups = "2" #so that group letter is appended for older files, it isn't right but better than trying to get 
    #order number to match what was used
  }
  if(totalGroups == "1") {
    orderNumber <- as.character(xml_contents(xml_find_all(x, paste0(".//", "FONumber"))))
  } 
  else {
    orderNumber <- paste0(as.character(xml_contents(xml_find_all(x, paste0(".//", "FONumber")))), groups[i])
  }
  df[i,]$OrderNumber <- orderNumber
  df[i,]$OrderDate <- as.character(xml_contents(xml_find_all(x, paste0(".//", "CreatedOn"))))
  df[i,]$ProcessedDate <- as.character(file.info(paste0(dir1, processed[i]))$mtime)
  df[i,]$Status <- as.character(xml_contents(xml_find_all(x, paste0(".//", "Status"))))
  df[i,]$Cases <- as.character(xml_contents(xml_find_all(x, paste0(".//", "CaseCount_Total"))))
  df[i,]$Dollars <- as.character(xml_contents(xml_find_all(x, paste0(".//", "Products_Sub_Total"))))
  df[i,]$JobName <- as.character(xml_contents(xml_find_all(x, paste0(".//", "Name"))))[1] #take first Name in XML
  designer <- as.character(xml_contents(xml_find_all(x, paste0(".//", "Designer"))))
  if(length(designer)==0){
    designer <- as.character(xml_contents(xml_find_all(x, paste0(".//", "SalesPerson"))))
  }
  if(length(designer)==0){
    designer <- as.character(xml_contents(xml_find_all(x, paste0(".//", "Salesperson"))))
  }
  df[i,]$Designer <- designer   
  
  optionList <- xml_attrs(xml_find_all(x, paste0(".//", "Option")))
  shipDate <- as.character(xml_contents(xml_find_all(x, paste0(".//", "Ship_Date"))))
  if(length(shipDate)==0){
    shipDate <- as.character(xml_contents(xml_find_all(x, paste0(".//", "RequestedShipDate"))))
  } 
  if(length(shipDate)==0){
    df[i,]$ShipDate <- "N/A"  
  } else{
    df[i,]$ShipDate <- shipDate  
  }
  df[i,]$CustomerAccount <- as.character(xml_contents(xml_find_all(x, paste0(".//", "Code"))))
  df[i,]$CustomerName <- as.character(xml_contents(xml_find_all(x, paste0(".//", "Name"))))[2] #take second Name in XML
  df[i,]$QuoteType <- unname(unlist(optionList[1]))[2] #Quote Type
  df[i,]$Construction <- unname(unlist(optionList[2]))[2] 
  df[i,]$DoorStyle <- unname(unlist(optionList[3]))[2] 
  df[i,]$Material <- unname(unlist(optionList[5]))[2] 
  df[i,]$Finish <- unname(unlist(optionList[6]))[2] 
  specialInstructions <- as.character(xml_contents(xml_find_all(x, paste0(".//", "Special_Instructions"))))
  if(length(specialInstructions)==0) {
    df[i,]$SpecialInstructions <-  "None"
  } else {
    df[i,]$SpecialInstructions <-  specialInstructions
  }
  
  i <- i + 1
  print(i)
}

df <- data.frame(lapply(df, function(x) {gsub("&amp;", "&", x)}))

View(df)

write.csv(df, file="Z:/OrderReport/SitelineOrders_2018_All.csv", row.names = FALSE)
