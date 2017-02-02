simulateWT <- function(mbase, .save = TRUE, .print = FALSE) {
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
  
  
  test <- llply(folder[1], function(x) {
    ldply(files[1], function(y) {
      xy = read_rds(path = paste0('./data/', x, '/', y))
      z = paste0(substr(x, 1, 4), '-', substr(x, 5, 6), '-', 
                 substring(x, nchar(x) - 1)) %>% ymd
      yy = filter(LADDT, Date < z & Date >= (z %m-% years(1)))
      all.yhat = data.frame(Date = yy$Date, 
                            Y = yy[grep('.Close', names(yy), value = TRUE)] %>% 
                              unlist, xy$yhat) %>% tbl_df
      if(.save == TRUE) {
        saveRDS(all.yhat, file = paste0('./data/', x, '/yhat', y))
        if(.print == TRUE) cat(paste0('./data/', x, '/yhat.', y, ' had saved.\n'))
      } else {
        if(.print == TRUE) cat(paste0('./data/', x, '/yhat.', y, ' had calculated.\n'))
      }
      
      wt.buy = all.yhat %>% mutate(B1 = ifelse(X1 < Y, X1, 0), 
                                   B1.1 = ifelse(X1.1 < Y, X1.1, 0), 
                                   B1.2 = ifelse(X1.2 < Y, X1.2, 0), 
                                   B1.3 = ifelse(X1.3 < Y, X1.3, 0), 
                                   B1.4 = ifelse(X1.4 < Y, X1.4, 0), 
                                   B1.5 = ifelse(X1.5 < Y, X1.5, 0), 
                                   B1.6 = ifelse(X1.6 < Y, X1.6, 0), 
                                   B1.7 = ifelse(X1.7 < Y, X1.7, 0), 
                                   B1.8 = ifelse(X1.8 < Y, X1.8, 0), 
                                   B1.9 = ifelse(X1.9 < Y, X1.9, 0), 
                                   B1.10 = ifelse(X1.10 < Y, X1.10, 0)) %>% 
        select(Date, B1, B1.1, B1.2, B1.3, B1.4, B1.5, B1.6, B1.7, B1.8, B1.9, B1.10)
      
      wt.sell = all.yhat %>% mutate(S1 = ifelse(X1 > Y, X1, 0), 
                                    S1.1 = ifelse(X1.1 > Y, X1.1, 0), 
                                    S1.2 = ifelse(X1.2 > Y, X1.2, 0), 
                                    S1.3 = ifelse(X1.3 > Y, X1.3, 0), 
                                    S1.4 = ifelse(X1.4 > Y, X1.4, 0), 
                                    S1.5 = ifelse(X1.5 > Y, X1.5, 0), 
                                    S1.6 = ifelse(X1.6 > Y, X1.6, 0), 
                                    S1.7 = ifelse(X1.7 > Y, X1.7, 0), 
                                    S1.8 = ifelse(X1.8 > Y, X1.8, 0), 
                                    S1.9 = ifelse(X1.9 > Y, X1.9, 0), 
                                    S1.10 = ifelse(X1.10 > Y, X1.10, 0)) %>% 
        select(Date, S1, S1.1, S1.2, S1.3, S1.4, S1.5, S1.6, S1.7, S1.8, S1.9, S1.10)
      
      wt.sum = suppressAll(join(wt.buy, wt.sell)) %>% tbl_df %>% rename(Diff = B1) %>% 
        mutate(Diff = c(0, diff(Date))) %>% .[-1] %>% mutate_each(funs(log))
      ## Need to refer to nicky 2001 again for weighted function. 
      ##   Either exp(log((T_{i} - T_{i-1}) * x_{i})) or while i is the time point at time_i, 365 days will have 365 observations crossprod().
      ##   exp(log((T_{latest_i} - T_{latest_i-0}) * mean(x_{latest_i}))) while i is the time point at time_i and only latest T_{i} - T_{0}.
      wt.sum <- data.frame(wt.buy$Date, wt.sum) %>% tbl_df
      return(wt.sum)
    })
  })
  
  return()
}