## =================================== Basic Model ===========================================
## Draft simulate the function while I added-on some-more criterias and selections.
## Simulate 365 days dataset with Markov Chain.
##   using moving 365 days dataset and rerun the compStocks.
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
suppressAll(library('tidyquant'))
suppressAll(library("shiny"))
suppressAll(library("shinyjs"))
suppressAll(library('shinyBS'))
suppressAll(library('DT'))
suppressAll(library('quantmod'))
suppressAll(library('formattable'))
suppressAll(library('highcharter'))
suppressAll(library('PerformanceAnalytics'))
suppressAll(library('memoise'))
suppressAll(source('./function/loadLAD.R'))
suppressAll(source('./function/filterLAD.R'))
suppressAll(source('./function/compStocks.R'))
suppressAll(source('./function/plotChart2.R'))

## load once and saved.
#'@ getSymbols('LAD')
#'@ saveRDS(LAD, file = './data/LAD_full.rds')
LAD <- read_rds(path = './data/LAD_full.rds')
LADDT <- LAD %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
  tbl_df %>% mutate(Date = ymd(Date)) %>% arrange(Date)
#mutate(LAD.Volume = formattable::digits(
#       LAD.Volume, 0, format = 'd', big.mark = ','))

dateID0 <- ymd('2015-01-01')
LADDT %<>% filter(Date >= (dateID0 - years(1))) #dateID == ymd('2016-02-29') has error and will need to review the coding.
dateID <- unique(LADDT$Date)
dateID <- dateID[dateID >= dateID0]
#'@ dir.create(paste0('./data/', str_replace_all(dateID, '-', '')))

## list the cv.glmnet models.
families <- c('gaussian', 'binomial', 'poisson', 'multinomial', 'cox', 'mgaussian', 'all')

bsGSfit <- llply(dateID, function(dt) {
  
  ## create a folder to save all models.
  pth = paste0('./data/', str_replace_all(dt, '-', ''))
  if(!dir.exists(pth)) dir.create(pth)
  
  ## predict dateID onwards from data < dateID
  smp = filter(LADDT, Date < dt & Date >= (dt - years(1)))
  gsfit = compStocks(smp, family = families[1], xy.matrix = 'h2', yv.lm = c(TRUE, FALSE), 
                     yv = c('open1', 'open2', 'high1', 'high2', 'low1', 'low2', 'close1', 'close2', 'daily.mean1', 'daily.mean2', 'daily.mean3', 'mixed1', 'mixed2', 'mixed3'), 
                     pred.type = 'class', .print = TRUE, .save = TRUE, pth = pth)
  
  ## basic model weight is FALSE
  weight.date = FALSE
  weight.volume = FALSE
  
  ## save basic or weight models.
  if(weight.date != FALSE | weight.volume != FALSE) {
    wtfitgaum = gsfit; rm(gsfit)
    
    ## generates 224 models
    saveRDS(wtfitgaum, file = paste0(pth, '/wtfitgaum.rds'))
    
    ## saved best mean-squared error comparison.
    wtfitgaum.mse1 = ldply(wtfitgaum$fit, function(x) x$mse) %>% tbl_df %>% filter(mse == min(mse))
    saveRDS(wtfitgaum.mse1, file = paste0(pth, '/wtfitgaum.mse1.rds'))
    
    ## saved summary of all best models. (if more than 1)
    name514gs = unique(wtfitgaum.mse1$.id)
    wtfitgaum.sum = ldply(wtfitgaum$fit[name514gs], function(x) x$mse) %>% tbl_df
    saveRDS(wtfitgaum.sum, file = paste0(pth, '/wtfitgaum.sum.rds'))
    
    ## saved best model.
    wtfitgaum.best = wtfitgaum$fit[name514gs]
    saveRDS(wtfitgaum.best, file = paste0(pth, '/wtfitgaum.best.rds'))
    
    ## saved best model's formula.
    wtfitgaum.form = wtfitgaum$formula1[str_replace_all(name514gs, 'wtfitgaum', '') %>% as.numeric]
    saveRDS(wtfitgaum.form, file = paste0(pth, '/wtfitgaum.form.rds'))
    
  } else {
    fitgaum = gsfit; rm(gsfit)
    
    ## generates 224 models
    saveRDS(fitgaum, file = paste0(pth, '/fitgaum.rds'))
    
    ## saved best mean-squared error comparison.
    fitgaum.mse1 = ldply(fitgaum$fit, function(x) x$mse) %>% tbl_df %>% filter(mse == min(mse))
    saveRDS(fitgaum.mse1, file = paste0(pth, '/fitgaum.mse1.rds'))
    
    ## saved summary of all best models. (if more than 1)
    name514gs = unique(fitgaum.mse1$.id)
    fitgaum.sum = ldply(fitgaum$fit[name514gs], function(x) x$mse) %>% tbl_df
    saveRDS(fitgaum.sum, file = paste0(pth, '/fitgaum.sum.rds'))
    
    ## saved best model.
    fitgaum.best = fitgaum$fit[name514gs]
    saveRDS(fitgaum.best, file = paste0(pth, '/fitgaum.best.rds'))
    
    ## saved best model's formula.
    fitgaum.form = fitgaum$formula1[str_replace_all(name514gs, 'fitgaum', '') %>% as.numeric]
    saveRDS(fitgaum.form, file = paste0(pth, '/fitgaum.form.rds'))
  }
  })







## ================================= Weighted Model ===========================================




















