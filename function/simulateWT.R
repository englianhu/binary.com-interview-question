simulateWT <- function(mbase, settledPrice = 'Close', weight.dist = 'pnorm', 
                       .parallel = TRUE, .save = TRUE, .print = FALSE) {
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
  
  if(weight.dist == 'pnorm') {
    weight.dist = pnorm
  } else if (weight.dist == 'phalfnorm') {
    weight.dist = phalfnorm
  } else {
    stop('Kindly select weight.dist = "pnorm" or weight.dist = "phalfnorm".')
  }
  
  folder <- list.files('./data', pattern = '[0-9]{8}')
  #'@ files <- list.files(paste0('./data/', folder), pattern = 'fitgaum+[0-9]{1,}.rds$')
  
  smbase <- llply(folder[1], function(x) {
    ldply(list.files(paste0('./data/', x), pattern = 'fitgaum+[0-9]{1,}.rds$')[1], 
          function(y) {
      xy = read_rds(path = paste0('./data/', x, '/', y))
      z = paste0(substr(x, 1, 4), '-', substr(x, 5, 6), '-', 
                 substring(x, nchar(x) - 1)) %>% ymd
      yy = filter(LADDT, Date < z & Date >= (z %m-% years(1)))
      
      all.yhat = data.frame(Date = yy$Date, 
                            #'@ Diff = as.numeric(difftime(yy$Date[1], yy$Date, units = 'days')), 
                            Y = yy[grep(settledPrice, names(yy), value = TRUE)] %>% 
                              unlist, xy$yhat) %>% tbl_df
      all.yhat[paste0(c('Y', 'X1', paste0('X1.', 1:10)))] %<>% mutate_each(funs(log))
      
      if(.save == TRUE) {
        saveRDS(all.yhat, file = paste0('./data/', x, '/yhat', y))
        if(.print == TRUE) cat(paste0('./data/', x, '/yhat.', y, ' had saved.\n'))
      } else {
        if(.print == TRUE) cat(paste0('./data/', x, '/yhat.', y, ' had calculated.\n'))
      }
      
      all.yhat %<>% 
        mutate(X1 = weight.dist(X1, Y), 
               X1.1 = weight.dist(X1.1, Y), 
               X1.2 = weight.dist(X1.2, Y), 
               X1.3 = weight.dist(X1.3, Y), 
               X1.4 = weight.dist(X1.4, Y), 
               X1.5 = weight.dist(X1.5, Y), 
               X1.6 = weight.dist(X1.6, Y), 
               X1.7 = weight.dist(X1.7, Y), 
               X1.8 = weight.dist(X1.8, Y), 
               X1.9 = weight.dist(X1.9, Y), 
               X1.10 = weight.dist(X1.10, Y))#'@ %>% 
        #'@ 
        #'@ mutate(B1 = ifelse(X1 < 0.5, X1, 0), 
        #'@        B1.1 = ifelse(X1.1 < 0.5, X1.1, 0), 
        #'@        B1.2 = ifelse(X1.2 < 0.5, X1.2, 0), 
        #'@        B1.3 = ifelse(X1.3 < 0.5, X1.3, 0), 
        #'@        B1.4 = ifelse(X1.4 < 0.5, X1.4, 0), 
        #'@        B1.5 = ifelse(X1.5 < 0.5, X1.5, 0), 
        #'@        B1.6 = ifelse(X1.6 < 0.5, X1.6, 0), 
        #'@        B1.7 = ifelse(X1.7 < 0.5, X1.7, 0), 
        #'@        B1.8 = ifelse(X1.8 < 0.5, X1.8, 0), 
        #'@        B1.9 = ifelse(X1.9 < 0.5, X1.9, 0), 
        #'@        B1.10 = ifelse(X1.10 < 0.5, X1.10, 0)) %>% 
        #'@ 
        #'@ mutate(S1 = ifelse(X1 > 0.5, X1, 0), 
        #'@        S1.1 = ifelse(X1.1 > 0.5, X1.1, 0), 
        #'@        S1.2 = ifelse(X1.2 > 0.5, X1.2, 0), 
        #'@        S1.3 = ifelse(X1.3 > 0.5, X1.3, 0), 
        #'@        S1.4 = ifelse(X1.4 > 0.5, X1.4, 0), 
        #'@        S1.5 = ifelse(X1.5 > 0.5, X1.5, 0), 
        #'@        S1.6 = ifelse(X1.6 > 0.5, X1.6, 0), 
        #'@        S1.7 = ifelse(X1.7 > 0.5, X1.7, 0), 
        #'@        S1.8 = ifelse(X1.8 > 0.5, X1.8, 0), 
        #'@        S1.9 = ifelse(X1.9 > 0.5, X1.9, 0), 
        #'@        S1.10 = ifelse(X1.10 > 0.5, X1.10, 0)) %>% 
        #'@ 
        #'@ select(Date, Y, Diff, 
        #'@        B1, B1.1, B1.2, B1.3, B1.4, B1.5, B1.6, B1.7, B1.8, B1.9, B1.10, 
        #'@        S1, S1.1, S1.2, S1.3, S1.4, S1.5, S1.6, S1.7, S1.8, S1.9, S1.10) %>% 
        #'@ 
        #'@ mutate(WT1 = B1 + S1, WT1.1 = B1.1 + S1.1, WT1.2 = B1.2 + S1.2, 
        #'@        WT1.3 = B1.3 + S1.3, WT1.4 = B1.4 + S1.4, WT1.5 = B1.5 + S1.5, 
        #'@        WT1.6 = B1.6 + S1.6, WT1.7 = B1.7 + S1.7, WT1.8 = B1.8 + S1.8, 
        #'@        WT1.9 = B1.9 + S1.9, WT1.10 = B1.10 + S1.10)
        
      all.yhat[paste0(c('Y', 'X1', paste0('X1.', 1:10)))] %<>% mutate_each(funs(log))
      data.frame(Date = z, Model = y, t(colMeans(all.yhat[paste0(c('Y', 'X1', paste0('X1.', 1:10)))]))) %>% tbl_df
      ## Need to refer to nicky 2001 again for weighted function. 
      ##   Either exp(log((T_{i} - T_{i-1}) * x_{i})) or while i is the time point at time_i, 365 days will have 365 observations crossprod().
      ##   exp(log((T_{latest_i} - T_{latest_i-0}) * mean(x_{latest_i}))) while i is the time point at time_i and only latest T_{i} - T_{0}.
      
    }) %>% tbl_df
  })
  
  return(smbase)
}