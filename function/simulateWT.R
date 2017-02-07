simulateWT <- function(mbase, settledPrice = 'Close', .parallel = TRUE, .save = TRUE, .print = FALSE) {
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
  suppressAll(library('LaplacesDemon'))
  suppressAll(library('parallel'))
  suppressAll(library('doParallel'))
  suppressAll(library('foreach'))
  suppressAll(library('fdrtool'))
  suppressAll(source('./function/phalfnorm.R'))
  
  settledPrice = substitute(settledPrice)
  
  #'@ doParallel::registerDoParallel(cores = parallel::detectCores())
  
  ## https://github.com/tobigithub/R-parallel/wiki/R-parallel-Errors
  ### Register parallel backend
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  getDoParWorkers()
  
  folder <- list.files('./data', pattern = '[0-9]{8}')
  #'@ files <- list.files(paste0('./data/', folder), pattern = 'fitgaum+[0-9]{1,}.rds$')
  
  mbase <- mbase[order(mbase$Date, decreasing = FALSE), ]
  
  smbase <- llply(folder, function(x) {
    ldply(list.files(paste0('./data/', x), pattern = 'fitgaum+[0-9]{1,}.rds$'), 
          function(y) {
      xy = read_rds(path = paste0('./data/', x, '/', y))
      z = paste0(substr(x, 1, 4), '-', substr(x, 5, 6), '-', 
                 substring(x, nchar(x) - 1)) %>% ymd
      yy = filter(LADDT, Date < z & Date >= (z %m-% years(1)))
      
      all.yhat = data.frame(Date = yy$Date, 
                            #'@ Diff = as.numeric(difftime(yy$Date[1], yy$Date, units = 'days')), 
                            Y = yy[grep(settledPrice, names(yy), value = TRUE)] %>% 
                              unlist, xy$yhat) %>% tbl_df
      #'@ all.yhat[paste0(c('Y', 'X1', paste0('X1.', 1:10)))] %<>% mutate_each(funs(log))
      
      all.yhat %<>% 
        mutate(P1 = pnorm(X1, Y), 
               P1.1 = pnorm(X1.1, Y), 
               P1.2 = pnorm(X1.2, Y), 
               P1.3 = pnorm(X1.3, Y), 
               P1.4 = pnorm(X1.4, Y), 
               P1.5 = pnorm(X1.5, Y), 
               P1.6 = pnorm(X1.6, Y), 
               P1.7 = pnorm(X1.7, Y), 
               P1.8 = pnorm(X1.8, Y), 
               P1.9 = pnorm(X1.9, Y), 
               P1.10 = pnorm(X1.10, Y), 
               PH1 = phalfnorm(X1, Y), 
               PH1.1 = phalfnorm(X1.1, Y), 
               PH1.2 = phalfnorm(X1.2, Y), 
               PH1.3 = phalfnorm(X1.3, Y), 
               PH1.4 = phalfnorm(X1.4, Y), 
               PH1.5 = phalfnorm(X1.5, Y), 
               PH1.6 = phalfnorm(X1.6, Y), 
               PH1.7 = phalfnorm(X1.7, Y), 
               PH1.8 = phalfnorm(X1.8, Y), 
               PH1.9 = phalfnorm(X1.9, Y), 
               PH1.10 = phalfnorm(X1.10, Y))
        
      all.yhat[paste0(c('Y', 'P1', paste0('P1.', 1:10), 'PH1', paste0('PH1.', 1:10)))] %<>% mutate_each(funs(log))
      
      wtdf <- data.frame(Date = z, Model = y, b0 = first(all.yhat$Date), 
                         t(colMeans(all.yhat[paste0(c('Y', 'P1', paste0('P1.', 1:10), 'PH1', paste0('PH1.', 1:10)))]))) %>% tbl_df
      ## Need to refer to nicky 2001 again for weighted function. 
      ##   Either exp(log((T_{i} - T_{i-1}) * x_{i})) or while i is the time point at time_i, 365 days will have 365 observations crossprod().
      ##   exp(log((T_{latest_i} - T_{latest_i-0}) * mean(x_{latest_i}))) while i is the time point at time_i and only latest T_{i} - T_{0}.
      
      if(.save == TRUE) {
        saveRDS(wtdf, file = paste0('./data/', x, '/wt.', y))
        if(.print == TRUE) cat(paste0('./data/', x, '/wt.', y, ' had saved.\n'))
      } else {
        if(.print == TRUE) cat(paste0('./data/', x, '/wt.', y, ' had calculated.\n'))
      }
      wtdf
    }) %>% tbl_df
  })
  
  return(smbase)
}