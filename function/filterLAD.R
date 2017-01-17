filterLAD <- memoise(function(startDate = NULL, endDate = NULL) {
  ## ==================== Load Packages ===================================
  suppressPackageStartupMessages(library('BBmisc'))
  suppressAll(library('readr'))
  suppressAll(library('plyr'))
  suppressAll(library('dplyr'))
  suppressAll(library('stringr'))
  suppressAll(library('magrittr'))
  suppressAll(library('lubridate'))
  suppressAll(library('formattable'))
  suppressAll(library('quantmod'))
  suppressAll(library('tidyquant'))
  
  ## ==================== Data Validation ===================================
  if(exists('LAD')) {
    mbase <- LAD
    
  } else {
    tryCatch({
      suppressAll(getSymbols('LAD', from = '2015-01-01'))
    }, error = function(e) stop('Kindly restart the shiny app.'))
    mbase <- LAD; rm(LAD)
  }
  
  mbaseDT <- mbase %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
    tbl_df %>% mutate(Date = ymd(Date), 
                      LAD.Volume = formattable::digits(
                        LAD.Volume, 0, format = 'd', big.mark = ','))
  dateID <- mbaseDT$Date
  
  if(is.null(startDate) & is.null(endDate)) {
    startDate <- dateID[1]
    endDate <- tail(dateID, 1)
    
  } else if((startDate %in% dateID) & (endDate %in% dateID)) {
    startDate <- startDate
    endDate <- endDate
    
  } else {
    tryCatch(suppressAll(getSymbols('LAD', from = startDate, to = endDate)), 
             error = function(e) getSymbols('LAD', from = '2015-01-01'))
    mbase <- LAD; rm(LAD)
    mbaseDT <- mbase %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
      tbl_df %>% mutate(Date = ymd(Date), 
                        LAD.Volume = formattable::digits(
                          LAD.Volume, 0, format = 'd', big.mark = ','))
    dateID <- mbaseDT$Date
    startDate <- dateID[1]
    endDate <- tail(dateID, 1)
    
    tmp <- list(fund = mbase, fundDT = mbaseDT)
    return(tmp)
  }
  
  ## ==================== Data Filtering ===================================
  mbase <- mbase[paste0(startDate, '/', endDate)]
  
  mbaseDT <- mbase %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
    tbl_df %>% mutate(Date = ymd(Date), 
                    LAD.Volume = formattable::digits(
                      LAD.Volume, 0, format = 'd', big.mark = ','))
  
  tmp <- list(fund = mbase, fundDT = mbaseDT)
  return(tmp)
})
