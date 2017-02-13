simcompS <- function(mbase, family, weight.dist = 'none') {
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
  suppressAll(library('quantmod'))
  suppressAll(source('./function/compStocks.R'))
  
  if(is.xts(mbase)) {
    mbase <- mbase %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
      tbl_df %>% mutate(Date = ymd(Date)) %>% arrange(Date)
    #mutate(LAD.Volume = formattable::digits(
    #       LAD.Volume, 0, format = 'd', big.mark = ','))
  }
  
  ## predicted date start from 2015-01-01.
  dateID0 <- ymd('2015-01-01')
  mbase %<>% filter(Date >= (dateID0 - years(1))) #dateID == ymd('2016-02-29') has error and will need to review the coding.
  dateID <- unique(mbase$Date)
  dateID <- dateID[dateID >= dateID0]
  #'@ dir.create(paste0('./data/', str_replace_all(dateID, '-', '')))
  
  ## list the cv.glmnet models.
  families <- c('gaussian', 'binomial', 'poisson', 'multinomial', 'cox', 'mgaussian', 'all')
  
  if(!family %in% families) {
    stop('Kindly set family within c("gaussian", "binomial", "poisson", "multinomial", "cox", "mgaussian", "all").')
  }
  
  fitm <- llply(dateID, function(dt) {
    
    ## create a folder to save all models.
    pth = paste0('./data/', str_replace_all(dt, '-', ''))
    if(!dir.exists(pth)) dir.create(pth)
    
    smp = filter(mbase, Date < dt & Date >= (dt %m-% years(1)))
    
    ## basic model weight is 
    if(weight.dist == 'pnorm') {
      
      ## predict dateID onwards from data < dateID
      ##   for example : in order to predict today price, I used the data from 
      ##   1 years (365 days or 366 days for leap years.) ago from yesterday.
      #'@ ymd("2016-2-29") %m-% years(1)
      ## http://stackoverflow.com/questions/8490799/how-to-account-for-leap-years
      fld = str_replace_all(dt, '-', '')
      fitnum = list.files(paste0('./data/', fld), pattern = '^fitgaum+[0-9]{1,}.rds$') %>% 
        str_extract_all('([0-9]{1,})') %>% unlist %>% as.numeric %>% sort
      
      fl = ldply(seq(fitnum), function(x) {
        read_rds(path = paste0('./data/', fld, '/wt.fitgaum', x, '.rds'))
      }) %>% tbl_df
      #'@ files <- list.files(paste0('./data/', fld), pattern = 'wt.fitgaum+[0-9]{1,}.rds$')
      #'@ paste0(substr(x, 1, 4), '-', substr(x, 5, 6), '-', 
      #'@       substring(x, nchar(x) - 1)) %>% ymd
      
      ## sqrt to emphasized the recent stock price more heavily. (based on the decay rate to lighten
      ##   previous stock price effects.)
      wtdt = exp(-log(as.numeric(difftime(dt, smp$Date))^2))
      ## I temporary to use `weight.volume` as the weight function on stock price to avoid 
      ##   troublesome to modify existing function. However the weight function doesn't take 
      ##   the effect of trade volume.
      wtpc = fl[, grep('P[0-9]', names(fl), value = TRUE)]
      
      #`preset.weight = TRUE` inside compStocks() is the temporarily set argument for testing 224 models.
      wt.pnorm.fitgaum = compStocks(smp, family = family, xy.matrix = 'h2', yv.lm = c(TRUE, FALSE), 
                         weight.date = wtdt, weight.volume = wtpc, fordate = dt, preset.weight = TRUE, 
                         yv = c('open1', 'open2', 'high1', 'high2', 'low1', 'low2', 'close1', 'close2', 'daily.mean1', 'daily.mean2', 'daily.mean3', 'mixed1', 'mixed2', 'mixed3'), 
                         pred.type = 'class', .print = TRUE, .save = TRUE, pth = pth, weight.dist = weight.dist)
      
      ## generates 224 models
      saveRDS(wt.pnorm.fitgaum, file = paste0(pth, '/wt.pnorm.fitgaum.rds'))
      
      ## saved best mean-squared error comparison.
      wt.pnorm.fitgaum.mse1 = ldply(wt.pnorm.fitgaum$fit, function(x) x$mse) %>% tbl_df %>% 
        filter(mse == min(mse))
      saveRDS(wt.pnorm.fitgaum.mse1, file = paste0(pth, '/wt.pnorm.fitgaum.mse1.rds'))
      
      ## saved summary of all best models. (if more than 1)
      name514gs = unique(wt.pnorm.fitgaum.mse1$.id)
      wt.pnorm.fitgaum.sum = ldply(wt.pnorm.fitgaum$fit[name514gs], function(x) x$mse) %>% tbl_df
      saveRDS(wt.pnorm.fitgaum.sum, file = paste0(pth, '/wt.pnorm.fitgaum.sum.rds'))
      
      ## saved best model.
      wt.pnorm.fitgaum.best = wt.pnorm.fitgaum$fit[name514gs]
      saveRDS(wt.pnorm.fitgaum.best, file = paste0(pth, '/wt.pnorm.fitgaum.best.rds'))
      
      ## saved best model's formula.
      wt.pnorm.fitgaum.form = wt.pnorm.fitgaum$formula1[str_replace_all(
        name514gs, 'wt.pnorm.fitgaum', '') %>% as.numeric]
      saveRDS(wt.pnorm.fitgaum.form, file = paste0(pth, '/wt.pnorm.fitgaum.form.rds'))
      rm(fld, fitnum, fl, wtdt, wtpc, wt.pnorm.fitgaum, wt.pnorm.fitgaum.mse1, wt.pnorm.fitgaum.best, wt.pnorm.fitgaum.sum, wt.pnorm.fitgaum.form)

    } else if(weight.dist == 'phalfnorm') {
      
      ## predict dateID onwards from data < dateID
      ##   for example : in order to predict today price, I used the data from 
      ##   1 years (365 days or 366 days for leap years.) ago from yesterday.
      #'@ ymd("2016-2-29") %m-% years(1)
      ## http://stackoverflow.com/questions/8490799/how-to-account-for-leap-years
      fld = str_replace_all(dt, '-', '')
      fitnum = list.files(paste0('./data/', fld), pattern = '^fitgaum+[0-9]{1,}.rds$') %>% 
        str_extract_all('([0-9]{1,})') %>% unlist %>% as.numeric %>% sort
      
      fl = ldply(seq(fitnum), function(x) {
        read_rds(path = paste0('./data/', fld, '/wt.fitgaum', x, '.rds'))
      }) %>% tbl_df
      #'@ files <- list.files(paste0('./data/', fld), pattern = 'wt.fitgaum+[0-9]{1,}.rds$')
      #'@ paste0(substr(x, 1, 4), '-', substr(x, 5, 6), '-', 
      #'@       substring(x, nchar(x) - 1)) %>% ymd
      
      ## sqrt to emphasized the recent stock price more heavily. (based on the decay rate to lighten
      ##   previous stock price effects.)
      wtdt = exp(-log(as.numeric(difftime(dt, smp$Date))^2))
      ## I temporary to use `weight.volume` as the weight function on stock price to avoid 
      ##   troublesome to modify existing function. However the weight function doesn't take 
      ##   the effect of trade volume.
      wtpc = fl[, grep('PH[0-9]', names(fl), value = TRUE)]
      
      #`preset.weight = TRUE` inside compStocks() is the temporarily set argument for testing 224 models.
      wt.phalfnorm.fitgaum = compStocks(smp, family = family, xy.matrix = 'h2', yv.lm = c(TRUE, FALSE), 
                                    weight.date = wtdt, weight.volume = wtpc, fordate = dt, preset.weight = TRUE, 
                                    yv = c('open1', 'open2', 'high1', 'high2', 'low1', 'low2', 'close1', 'close2', 'daily.mean1', 'daily.mean2', 'daily.mean3', 'mixed1', 'mixed2', 'mixed3'), 
                                    pred.type = 'class', .print = TRUE, .save = TRUE, pth = pth, weight.dist = weight.dist)
      
      ## generates 224 models
      saveRDS(wt.phalfnorm.fitgaum, file = paste0(pth, '/wt.phalfnorm.fitgaum.rds'))
      
      ## saved best mean-squared error comparison.
      wt.phalfnorm.fitgaum.mse1 = ldply(wt.phalfnorm.fitgaum$fit, function(x) x$mse) %>% tbl_df %>% 
        filter(mse == min(mse))
      saveRDS(wt.phalfnorm.fitgaum.mse1, file = paste0(pth, '/wt.phalfnorm.fitgaum.mse1.rds'))
      
      ## saved summary of all best models. (if more than 1)
      name514gs = unique(wt.phalfnorm.fitgaum.mse1$.id)
      wt.phalfnorm.fitgaum.sum = ldply(wt.phalfnorm.fitgaum$fit[name514gs], function(x) x$mse) %>% tbl_df
      saveRDS(wt.phalfnorm.fitgaum.sum, file = paste0(pth, '/wt.phalfnorm.fitgaum.sum.rds'))
      
      ## saved best model.
      wt.phalfnorm.fitgaum.best = wt.phalfnorm.fitgaum$fit[name514gs]
      saveRDS(wt.phalfnorm.fitgaum.best, file = paste0(pth, '/wt.phalfnorm.fitgaum.best.rds'))
      
      ## saved best model's formula.
      wt.phalfnorm.fitgaum.form = wt.phalfnorm.fitgaum$formula1[str_replace_all(
        name514gs, 'wt.phalfnorm.fitgaum', '') %>% as.numeric]
      saveRDS(wt.phalfnorm.fitgaum.form, file = paste0(pth, '/wt.phalfnorm.fitgaum.form.rds'))
      rm(fld, fitnum, fl, wtdt, wtpc, wt.phalfnorm.fitgaum, wt.phalfnorm.fitgaum.mse1, wt.phalfnorm.fitgaum.best, wt.phalfnorm.fitgaum.sum, wt.phalfnorm.fitgaum.form)

    } else if(weight.dist == 'none') {
      
      fitgaum = compStocks(smp, family = family, xy.matrix = 'h2', yv.lm = c(TRUE, FALSE), preset.weight = TRUE, 
                         yv = c('open1', 'open2', 'high1', 'high2', 'low1', 'low2', 'close1', 'close2', 'daily.mean1', 'daily.mean2', 'daily.mean3', 'mixed1', 'mixed2', 'mixed3'), 
                         pred.type = 'class', .print = TRUE, .save = TRUE, pth = pth)
      
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
      rm(fld, fitnum, fl, wtdt, wtpc, fitgaum, fitgaum.mse1, fitgaum.best, fitgaum.sum, fitgaum.form)

    } else {
      stop('Kindly select weight.dist = "pnorm" or weight.dist = "phalfnorm" or weight.dist = "none".')
    }
    })

  #'@ rm(list = ls(all.names = TRUE))
  return(fitm)
  }

