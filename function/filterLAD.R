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
  #'@ suppressAll(library('tidyquant'))
  suppressAll(source('./function/loadLAD.R'))
  
  ## ==================== Data Validation ===================================
  if(exists('LAD')) {
    mbase <- LAD
    
  } else {
    loadLAD()
    mbase <- LAD; rm(LAD)
    mbaseDT <- LADDT; rm(LADDT)
  }
  
  if(!exists('mbaseDT')) {
    mbaseDT <- LAD %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
      tbl_df %>% mutate(Date = ymd(Date)) %>% arrange(Date)
    # %>% mutate(LAD.Volume = formattable::digits(
    #       LAD.Volume, 0, format = 'd', big.mark = ','))
  }
  
  if(!exists('dateRange')) dateRange <- c(today() - 365, today())
  
  if(is.null(startDate) & is.null(endDate)) {
    startDate <- dateRange[1]
    endDate <- dateRange[2]
    
  } else if((startDate %in% dateRange) & (endDate %in% dateRange)) {
    startDate <- startDate
    endDate <- endDate
    
  } else {
    suppressAll(getSymbols('LAD', from = startDate, to = endDate))
    
    mbase <- LAD; rm(LAD)
    mbaseDT <- mbase %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
      tbl_df %>% mutate(Date = ymd(Date)) %>% arrange(Date)
    # %>% mutate(LAD.Volume = formattable::digits(
    #       LAD.Volume, 0, format = 'd', big.mark = ','))
    dateRange <- range(mbaseDT$Date)
    startDate <- dateRange[1]
    endDate <- dateRange[2]
    
    tmp <- list(fund = mbase, fundDT = mbaseDT)
    return(tmp)
  }
  
  ## ==================== Data Filtering ===================================
  mbase <- mbase[paste0(startDate, '/', endDate)]
  
  mbaseDT <- mbase %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
    tbl_df %>% mutate(Date = ymd(Date)) %>% arrange(Date)
  # %>% mutate(LAD.Volume = formattable::digits(
  #       LAD.Volume, 0, format = 'd', big.mark = ','))
  
  tmp <- list(fund = mbase, fundDT = mbaseDT)
  return(tmp)
})
