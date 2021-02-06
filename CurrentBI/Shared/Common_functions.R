normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#####
# The columns in the dashboard report tables are static. Need to fill 
# in missing columns after each filter. 
# Future task: Use ddfJobType table to indicate which lines to include in 
# dasboard reports. 

build_filler <- function() {
  #create data frame with 0 rows and 3 columns
  df_filler1 <- data.frame(
    Brand = c('Greenfield', 'Greenfield', 'Siteline'),
    ProductLine = c('Framed', 'Full Access', 'Full Access')
  )
  
  df_filler1$Plant <- 'Elkins'
  
  df_filler2 <- df_filler1   
  
  df_filler2$Plant <- 'Indy'
  
  df_filler <- union_all(df_filler1, df_filler2)  
  df_filler$Cases <- 0
  df_filler$Dollars <- 0
  
  df_filler
}

fill_missing_rows <- function (x) {
  filler <- build_filler()

  IDs <- unique(x$weekNo)

  for (i in 1:length(IDs)){ 
    temp <- x[x$ID==IDs[i],]
    
    fill_df <- filler %>% 
      mutate(weekNo = IDs[i]) %>% 
      anti_join(temp, by=c("Brand", "Plant", "ProductLine"))
    
    x <- union(x, fill_df)
  }

  x
  
}

#Function to replace anything less than 0 in the array
fixDayWeekMonthCounter <- function(xArray, startInt){
  if(sum(xArray < 1) > 0){
    
    indexCounter = 1
    
    for(item in xArray){
      if(item < 1){
        xArray <- replace(xArray, indexCounter, startInt)
        startInt <- startInt - 1
      }
      
      indexCounter <- indexCounter + 1
    }
  }
  return (xArray)
}
