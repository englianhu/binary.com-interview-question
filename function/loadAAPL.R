loadAAPL <- function() {
  suppressPackageStartupMessages(library("BBmisc"))
  suppressAll(library('devtools'))
  suppressAll(library('lubridate'))
  suppressAll(library('plyr'))
  suppressAll(library('stringr'))
  suppressAll(library('magrittr'))
  suppressAll(library('dplyr'))
  suppressAll(library('tidyr'))
  suppressAll(library('readr'))
  suppressAll(library('tidyverse')) #load c(dplyr, tidyr, stringr, readr) due to system doesn't work.
  suppressAll(library('quantmod'))
  
  tryCatch({
    suppressAll(getSymbols('AAPL', from = '2015-01-01'))
    if(exists('AAPL')) saveRDS(AAPL, file = './data/AAPL.rds')
  }, error = function(e) AAPL <- read_rds(path = './data/AAPL.rds'))
  
  if(!exists('AAPL')) AAPL <- read_rds(path = './data/AAPL.rds')
  
  AAPLDT <- AAPL %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
    tbl_df %>% mutate(Date = ymd(Date))#, 
  #AAPL.Volume = formattable::digits(
  #AAPL.Volume, 0, format = 'd', big.mark = ','))
  dateID <- AAPLDT$Date
  
  #'@ return(eval(parse(text = paste0('AAPL = AAPL; AAPLDT = AAPLDT; dateID = dateID'))))
  return(list(assign('AAPL', AAPL), assign('AAPLDT', AAPLDT), assign('dateID', dateID)))
}


