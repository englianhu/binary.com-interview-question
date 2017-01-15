## ========= Setup Options =================================
## Setup Options, Loading Required Libraries and Preparing Environment
## Loading the packages and setting adjustment
## 
## Search available packages.
## http://thecoatlessprofessor.com/programming/automatically-check-if-r-package-is-the-latest-version-on-package-load/
## 
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
suppressAll(library("shiny"))
suppressAll(library("shinyjs"))
suppressAll(library('shinyBS'))
suppressAll(library('DT'))
suppressAll(library('quantmod'))
suppressAll(library('formattable'))
suppressAll(library('highcharter'))
suppressAll(library('PerformanceAnalytics'))
suppressAll(library('memoise'))
source('./function/filterAAPL.R')
source('./function/plotChart2.R')

## ========= Read Data =================================
## check if the saved dataset is today's data? if previous day then need to scrap from website.
if(file.exists('./data/AAPL.rds')) {
  if(readRDS('./data/AAPL.rds') %>% attributes %>% .$updated %>% as.Date < today()) {
    suppressAll(getSymbols('AAPL', from = '2015-01-01'))
  } else {
    AAPL <- read_rds(path = './data/AAPL.rds')
  }
} else {
  suppressAll(getSymbols('AAPL', from = '2015-01-01'))
  saveRDS(AAPL, file = './data/AAPL.rds')
}

#'@ tryCatch({
#'@   suppressAll(getSymbols('AAPL', from = '2015-01-01'))
#'@   if(exists('AAPL')) saveRDS(AAPL, file = './data/AAPL.rds')
#'@   }, error = function(e) AAPL <- read_rds(path = './data/AAPL.rds'))

#'@ if(!exists('AAPL')) AAPL <- read_rds(path = './data/AAPL.rds')

AAPLDT <- AAPL %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
  tbl_df %>% mutate(Date = ymd(Date))#, 
                    #AAPL.Volume = formattable::digits(
                    #AAPL.Volume, 0, format = 'd', big.mark = ','))
dateID <- AAPLDT$Date

