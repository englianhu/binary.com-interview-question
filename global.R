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
suppressAll(source('./function/filterLAD.R'))
suppressAll(source('./function/plotChart2.R'))

## ========= Read Data =================================
## check if the saved dataset is today's data? if previous day then need to scrap from website.
if(file.exists('./data/LAD.rds')) {
  if(readRDS('./data/LAD.rds') %>% attributes %>% .$updated %>% as.Date < today()) {
    tryCatch({
      suppressAll(getSymbols('LAD', from = '2015-01-01'))
    }, error = function(e) stop('Kindly restart the shiny app.'))
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

