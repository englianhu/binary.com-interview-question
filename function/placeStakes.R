placeStakes <- function(newx, fit = NULL, choose = c('Back', 'Lay'), prob, stakes = 10) {
  ## test the returns of the staking model.
  ## ================================ Load Package =====================================
  suppressPackageStartupMessages(library('BBmisc'))
  suppressAll(library('plyr'))
  suppressAll(library('tidyverse'))
  suppressAll(library('lubridate'))
  
  suppressAll(source('./function/optKelly.R'))
  suppressAll(source('./function/optF.R'))
  
  ## ============================= Data Validation =====================================
  if(is.null(fit)) {
    fit <- read_rds(path = './data/fitgaum16.alpha08.rds')
  }
  
  pd <- predict(fitgaum16.alpha08, newx = xy$x, pred.type = 'class') %>% 
    data.frame %>% tbl_df %>% rename(Pred.Close = X1) %>% 
    mutate(Pred.Close = round(Pred.Close, 2))
  
  dfm <- data.frame(fundDT, pd) %>% tbl_df
  
  res <- dfm[c('Date', 'LAD.Close', 'Pred.Close')] %>% 
    mutate(
      Diff = round(Pred.Close - LAD.Close, 2), 
      Back = ifelse(Pred.Close > LAD.Close, 1, 
             ifelse(Pred.Close == LAD.Close, 0, -1)), 
      Lay = ifelse(Pred.Close < LAD.Close, 1, 
            ifelse(Pred.Close == LAD.Close, 1, -1))) %>% 
    mutate(Return.Back = ifelse(Back == 1, (Diff * Back * stakes), 
                         ifelse(Back == 0, (Back * stakes), 
                         ifelse(Back == -1, (Back * stakes), 0))), 
           Return.Lay = ifelse(Lay == 1, (Diff * Lay * stakes), 
                        ifelse(Lay == 0, (Lay * stakes), 
                        ifelse(Lay == -1, (Lay * stakes), 0))))
  
  return()
}