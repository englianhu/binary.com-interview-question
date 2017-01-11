getAAPL <- function(mbase, start = NULL, end = NULL){
  
  ## ==================== Load Packages ===================================
  library('BBmisc')
  library('readr')
  library('plyr')
  library('dplyr')
  library('stringr')
  library('magrittr')
  library('lubridate')
  library('quantmod')
  
  ## ==================== Data Validation ===================================
  AAPL <- tryCatch(suppressAll(read_rds(path = './data/AAPL.rds')), 
                   error = function(e) getSymbols('AAPL'))
  
  dateID <- AAPL %>% data.frame %>% rownames %>% ymd
  
  if(is.null(start)) {
    start <- dateID[1]
    
  } else if(start %in% dateID) {
    start <- start
    
  } else {
    AAPL <- getSymbols('AAPL')
  }
  
  if(file.exists('./data/AAPL.rds')) {
    AAPL <- suppressAll(read_rds(path = './data/AAPL.rds'))
    
  } else {
    getSymbols('AAPL')
  }
  
  
  end <- tail(AAPL, 1) %>% data.frame %>% rownames %>% ymd
  
  ## ==================== Load Packages ===================================
  
  
}

