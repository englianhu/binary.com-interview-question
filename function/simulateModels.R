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
  ##   for example : in order to predict today price, I used the data from 
  ##   1 years (365 days or 366 days for leap years.) ago from yesterday.
  #'@ ymd("2016-2-29") %m-% years(1)
  ## http://stackoverflow.com/questions/8490799/how-to-account-for-leap-years
  
  smp = filter(LADDT, Date < dt & Date >= (dt %m-% years(1)))
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

## ================================= Save Basic Model ========================================
## Arrange the best fit and save the model summary.
files <- list.files('./data', pattern = '[0-9]{8}')
#'@ pth <- paste0('./data/', files, '/fitgaum.mse1.rds')

## list all daily minimum standard error models.
pre.mse1 <- ldply(files, function(x) {
  y = read_rds(path = paste0('./data/', x, '/fitgaum.mse1.rds'))
  y %>% data.frame(Date = x, .) %>% tbl_df %>% filter(mse == min(mse))
}) %>% tbl_df

## filter and only get first unique element since the mse is same.
#'@ unique(pre.mse1[c('Date', 'model', 'mse')])
pre.mse1 <- pre.mse1[!duplicated(pre.mse1$Date, pre.mse1$mse), ]
saveRDS(pre.mse1, file = './data/pre.mse1.rds')
#> pre.mse1
# A tibble: 517 × 4
#       Date        .id  model       mse
#     <fctr>      <chr> <fctr>     <dbl>
#1  20150102 fitgaum119   mse9 0.1209190
#2  20150105 fitgaum135   mse9 0.1211786
#3  20150106 fitgaum132   mse3 0.1210193
#4  20150107  fitgaum81   mse8 0.1221952
#5  20150108 fitgaum144   mse3 0.1220447
#6  20150109  fitgaum27   mse3 0.1210785
#7  20150112 fitgaum129   mse3 0.1211022
#8  20150113  fitgaum17   mse3 0.1214222
#9  20150114  fitgaum28   mse3 0.1215031
#10 20150115  fitgaum18   mse3 0.1208796

## list all daily minimum standard error models' formula.
pre.gsform <- ldply(files, function(x) {
  y = read_rds(path = paste0('./data/', x, '/fitgaum.form.rds'))
  z = paste0(substr(x, 1, 4), '-', substr(x, 5, 6), '-', substring(x, nchar(x) - 1)) %>% 
    ymd 
  data.frame(Date = z, form = y) %>% tbl_df
}) %>% tbl_df

## filter and only get first unique element since the mse is same.
pre.gsform <- pre.gsform[!duplicated(pre.gsform$Date), ]
saveRDS(pre.gsform, file = './data/pre.gsform.rds')

## ================================= Weighted Model ==========================================
## Application bayesian as weighted function and MCMC for prediction.

## Read summary of the best fit model
## From below table, we noted that the dynamic model required compare 
##   to using constant model across the days as we can know from column `.id`.
pre.mse1 <- read_rds(path = './data/pre.mse1.rds')
#> pre.mse1
# A tibble: 517 × 4
#       Date        .id  model       mse
#     <fctr>      <chr> <fctr>     <dbl>
#1  20150102 fitgaum119   mse9 0.1209190
#2  20150105 fitgaum135   mse9 0.1211786
#3  20150106 fitgaum132   mse3 0.1210193
#4  20150107  fitgaum81   mse8 0.1221952
#5  20150108 fitgaum144   mse3 0.1220447
#6  20150109  fitgaum27   mse3 0.1210785
#7  20150112 fitgaum129   mse3 0.1211022
#8  20150113  fitgaum17   mse3 0.1214222
#9  20150114  fitgaum28   mse3 0.1215031
#10 20150115  fitgaum18   mse3 0.1208796

## Read best fit model.
pre.gsform <- read_rds(path = './data/pre.gsform.rds')


## ============================== Save Weighted Model ========================================
## Arrange the best fit and save the data.
## 
llply(seq(nrow(pre.mse1)), function(i) {
  y = read_rds(path = paste0('./data/', pre.mse1$Date[i], '/', pre.mse1$.id[i], '.rds'))
  j = filter(y$mse, mse == pre.mse1$mse[i]) %>% .$model %>% str_replace_all('mse', '') %>% 
    as.numeric
  y$yhat[j] %>% unlist
  })

files <- list.files('./data/20150102/', pattern = 'fitgaum+[0-9]{1,}.rds$')

wt <- ldply(files, function(x) {
  y = read_rds(path = paste0('./data/', x, '/', x))
  y %>% data.frame(Date = x, .) %>% tbl_df %>% filter(mse == min(mse))
}) %>% tbl_df



## ==================================== MCMC Model ===========================================
## Application of MCMC to simulate the Profit and Loss of opt.Kelly() and optimal.f() 
##   staking model.












