placeStakes <- function(newx, fit = NULL, choose = c('Back', 'Lay'), prob, stakes = 10, 
                        bet.price = 'Predict', settled.price = 'Close') {
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
  
  fundDT %<>% mutate(HL.Mean = (LAD.High + LAD.Low) / 2)
  
  bet.price.list <- c('Predict', 'Open', 'High', 'Low', 'Close')
  if(!bet.price %in% bet.price.list) {
    stop('Kindly select a price that you would like to place an order.')
  } else {
    bet.price.list <- bet.price.list
  }
  
  settled.price.list <- c('Predict', 'Open', 'High', 'Low', 'Close')
  if(!settled.price %in% settled.price.list) {
    stop('Kindly select a price that you would like to place an order.')
  } else {
    settled.price <- settled.price
  }
  
  ## ============================= Prediction model =====================================
  pd <- predict(fitgaum16.alpha08, newx = xy$x, pred.type = 'class') %>% 
    data.frame %>% tbl_df %>% rename(Pred = X1) %>% 
    mutate(Pred = round(Pred, 2))
  
  dfm <- data.frame(fundDT, pd) %>% tbl_df
  
  if(bet.price == 'Predict') {
    dfm %<>% mutate(betPrice = Pred)
  }
  if(bet.price == 'Open') {
    dfm %<>% mutate(betPrice = LAD.Open)
  }
  if(bet.price == 'High') {
    dfm %<>% mutate(betPrice = LAD.High)
  }
  if(bet.price == 'Low') {
    dfm %<>% mutate(betPrice = LAD.Low)
  }
  if(bet.price == 'Close') {
    dfm %<>% mutate(betPrice = LAD.Close)
  }
  
  if(settled.price == 'Predict') {
    dfm %<>% mutate(settledPrice = Pred)
  }
  if(settled.price == 'Open') {
    dfm %<>% mutate(settledPrice = LAD.Open)
  }
  if(settled.price == 'High') {
    dfm %<>% mutate(settledPrice = LAD.High)
  }
  if(settled.price == 'Low') {
    dfm %<>% mutate(settledPrice = LAD.Low)
  }
  if(settled.price == 'Close') {
    dfm %<>% mutate(settledPrice = LAD.Close)
  }
  
  ## ============================= Staking model =====================================
  ## http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:relative_strength_index_rsi
  ## The article brief the RSI taking closing price and the formula which is 
  ##   average gain - average loss within 14 days.
  
  ## Lets assume that the bets will be only valid until Closed market, therefore 
  ##   Close price as daily settlement price as default.
  res <- dfm[c('Date', 'HL.Mean', 'betPrice', 'settledPrice')] %>% mutate(
      Diff = round(betPrice - settledPrice, 2), 
      Back = ifelse(betPrice > settledPrice, 1, 0), 
      Lay = ifelse(betPrice < settledPrice, 1, 0)) %>% 
    mutate(prob = pnorm(betPrice, mean = HL.Mean, sd = sd(HL.Mean))) %>% 
    mutate(Return.Back = ifelse(prob > 0.5, Diff * Back * stakes, 0), 
           Return.Lay = ifelse(probr > 0.5, Diff * Lay * stakes, 0))
  
  ## convert the bet.price into probabilities based on daily.mean2.
  res %<>% mutate(prob = pnorm(betPrice, mean = HL.Mean, sd = sd(HL.Mean))) %>% 
    mutate(optKellyR = optKelly(win = (Diff * Back * stakes), 
                                loss = (Diff * Back * stakes), 
                                p = prob, obs = nrow(.), lev = stakes))
  
  res %<>% mutate(optKelly)
  
  return()
}