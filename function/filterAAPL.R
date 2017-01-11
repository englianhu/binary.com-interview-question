filterAAPL <- function(mbase, startDate = NULL, endDate = NULL) {
  ## ==================== Load Packages ===================================
  library('BBmisc')
  library('readr')
  library('plyr')
  library('dplyr')
  library('stringr')
  library('magrittr')
  library('lubridate')
  library('formattable')
  library('quantmod')
  
  ## ==================== Data Validation ===================================
  mbaseDT <- mbase %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
    tbl_df %>% mutate(Date = ymd(Date), 
                      AAPL.Volume = formattable::digits(
                        AAPL.Volume, 0, format = 'd', big.mark = ','))
  dateID <- mbase$Date
  
  if(is.null(startDate) & is.null(endDate)) {
    startDate <- dateID[1]
    endDate <- tail(dateID, 1)
    
  } else if((startDate %in% dateID) & (endDate %in% dateID)) {
    startDate <- startDate
    endDate <- endDate
    
  } else {
    tryCatch(suppressAll(getSymbols('AAPL', start = startDate, end = endDate)), 
             error = function(e) getSymbols('AAPL'))
    mbase <- AAPL; rm(AAPL)
    mbaseDT <- mbase %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
      tbl_df %>% mutate(Date = ymd(Date), 
                        AAPL.Volume = formattable::digits(
                          AAPL.Volume, 0, format = 'd', big.mark = ','))
    dateID <- mbase$Date
    startDate <- dateID[1]
    endDate <- tail(dateID, 1)
    
    tmp <- list(fund = mbase, fundDT = mbaseDT)
    return(tmp)
  }
  
  ## ==================== Data Filtering ===================================
  mbase <- mbase[paste0(startDate, '/', endDate)]
  
  mbaseDT <- mbase %>% mutate(Date = ymd(Date), 
                    AAPL.Volume = formattable::digits(
                      AAPL.Volume, 0, format = 'd', big.mark = ','))
  
  tmp <- list(fund = mbase, fundDT = mbaseDT)
  return(tmp)
}