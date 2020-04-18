library(odbc)
library(rstudioapi)
library(dplyr)
con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 13 for SQL Server",
                 Server = "DBNAME",
                 Database = "MDEDICGPROD",
                 UID = "username",
                 PWD = rstudioapi::askForPassword("Database password"),
                 Port = 1433)

tlist <- dbGetQuery(con,"SELECT TABLE_NAME FROM MDNET2.INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE = 'BASE TABLE'")
filter(tlist, grepl("Ord", TABLE_NAME))
db <- "MDNET2.dbo."
client <- "binatone"
sprintf("SELECT * FROM %sOrdDelete WHERE client_id='%s'",db,client)
sql <- sprintf("SELECT * FROM shipload_ord WHERE client_id='%s' AND sendshipconfirm = 0",client)
oh <- dbGetQuery(con, "select top 100 * from shipload_ord")
sh <- dbGetQuery(con, sql) 
