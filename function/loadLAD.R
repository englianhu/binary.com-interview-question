loadLAD <- function() {
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
  suppressAll(library('tidyquant'))
  
  ## check if the saved dataset is today's data? if previous day then need to scrap from website.
if(file.exists('./data/LAD.rds')) {
  if(readRDS('./data/LAD.rds') %>% attributes %>% .$updated %>% as.Date < today()) {
    #'@ tryCatch({
    #'@   suppressAll(getSymbols('LAD', from = '2015-01-01'))
    #'@ }, error = function(e) stop('Kindly restart the shiny app.'))
    suppressAll(getSymbols('LAD', from = '2015-01-01'))
    saveRDS(LAD, file = './data/LAD.rds')
    
  } else {
    LAD <- read_rds(path = './data/LAD.rds')
  }
} else {
  suppressAll(getSymbols('LAD', from = '2015-01-01'))
  saveRDS(LAD, file = './data/LAD.rds')
}

#'@ tryCatch({
#'@   suppressAll(getSymbols('LAD', from = '2015-01-01'))
#'@   if(exists('LAD')) saveRDS(LAD, file = './data/LAD.rds')
#'@   }, error = function(e) LAD <- read_rds(path = './data/LAD.rds'))

#'@ if(!exists('LAD')) LAD <- read_rds(path = './data/LAD.rds')

LADDT <- LAD %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
  tbl_df %>% mutate(Date = ymd(Date)) %>% arrange(Date)
                    #mutate(LAD.Volume = formattable::digits(
                    #       LAD.Volume, 0, format = 'd', big.mark = ','))
dateID <- LADDT$Date

  #'@ return(eval(parse(text = paste0('LAD = LAD; LADDT = LADDT; dateID = dateID'))))
  return(list(assign('LAD', LAD), assign('LADDT', LADDT), assign('dateID', dateID)))
}


