## ========= Setup Options =================================
## Setup Options, Loading Required Libraries and Preparing Environment
## Loading the packages and setting adjustment
## 
## Search available packages.
## http://thecoatlessprofessor.com/programming/automatically-check-if-r-package-is-the-latest-version-on-package-load/
## 
## 
## require(devtools)
## install_version("ggplot2", version = "0.9.1", repos = "http://cran.us.r-project.org")
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
#'@ suppressAll(library('tidyquant'))
suppressAll(library("shiny"))
suppressAll(library("shinyjs"))
suppressAll(library('shinyBS'))
suppressAll(library('DT'))
suppressAll(library('quantmod'))
suppressAll(library('formattable'))
suppressAll(library('highcharter'))
suppressAll(library('PerformanceAnalytics'))
suppressAll(library('memoise'))
suppressAll(library('glmnet'))
suppressAll(library('forecast'))
suppressAll(library('RCurl'))

suppressAll(source('./shinyFunction/loadLAD.R'))
suppressAll(source('./shinyFunction/filterLAD.R'))
suppressAll(source('./shinyFunction/glmPrice.R'))
suppressAll(source('./shinyFunction/h.R'))
suppressAll(source('./shinyFunction/plotChart2.R'))

## ========= Read Data =================================
eval(parse(text = paste0('datam = loadLAD(); LAD = datam$LAD; LADDT = datam$LADDT; rm(datam)')))

## use 365 days dataset but using predict().
## need to modify... temporarily use since baseline * times the coef rates will be consider as a weighted models but need to test. 
tmpsumgs <- read_rds(path = './shinyData/tmpsumgs.rds') %>% tbl_df
#'@ tmptable <- read_rds(path = './data/tmptable.rds') %>% tbl_df
#'@ tmpgsfit <- read_rds(path = './data/tmpgsfit.rds') #file too big and heavily to load, need to only pick the best fit.
#'@ tmpgsform <- read_rds(path = './data/tmpgsform.rds')
fitgaum16.alpha08 <- read_rds(path = './shinyData/fitgaum16.alpha08.rds')

#'@ fitgaum193.alpha08 <- read_rds(path = './data/fitgaum193.alpha08.rds')

## Use 365 days dataset for time series model.
pre.mse1 <- read_rds(path = './shinyData/pre.mse1.rds')

## Read best fit model.
pre.gsform <- read_rds(path = './shinyData/pre.gsform.rds')







