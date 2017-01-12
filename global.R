## ========= Setup Options =================================
## Setup Options, Loading Required Libraries and Preparing Environment
## Loading the packages and setting adjustment
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
tryCatch(suppressAll(getSymbols('AAPL')), 
         error = function(e) AAPL <- read_rds(path = './data/AAPL.rds'))

if(!exists('AAPL')) AAPL <- read_rds(path = './data/AAPL.rds')

AAPLDT <- AAPL %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
  tbl_df %>% mutate(Date = ymd(Date), 
                    AAPL.Volume = formattable::digits(
                      AAPL.Volume, 0, format = 'd', big.mark = ','))
dateID <- AAPLDT$Date

