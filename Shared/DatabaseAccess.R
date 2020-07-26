##  Common functions to access data.
##  Andrew Borst
##  Last Update 5/27/20
##:::::::::::::::::::::::::::::::::
sql01_view <- function(database_name, database_view)
{
    library(odbc)
    library(jsonlite)
    j <- read_json("PrivateData/configuration.json")
    login <- j[[1]]$login
    pwd <- j[[1]]$password
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "IN-SQL01",
                     Database = database_name,
                     UID = login,
                     PWD = pwd,
                     Port = 1433)
    dbGetQuery(con, glue::glue("SELECT * FROM ", database_view))
}