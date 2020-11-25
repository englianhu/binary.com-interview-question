suppressWarnings(require('BBmisc'))
suppressWarnings(require('shiny'))
suppressWarnings(require('httr'))
suppressWarnings(require('memoise'))
suppressWarnings(require('stringr'))
suppressWarnings(require('xts'))
suppressWarnings(require('TFX'))
suppressWarnings(require('quantmod'))
suppressWarnings(require('rugarch'))
suppressWarnings(require('lubridate'))
suppressWarnings(require('ggplot2'))
suppressWarnings(require('highcharter'))
suppressWarnings(require('formattable'))
suppressWarnings(require('magrittr'))
suppressWarnings(require('plyr'))
suppressWarnings(require('dplyr'))
suppressWarnings(require('pryr'))
suppressWarnings(require('tidyr'))
suppressWarnings(require('purrr'))
suppressWarnings(require('microbenchmark'))
suppressWarnings(require('stringr'))
suppressWarnings(require('forecast'))
suppressWarnings(require('PerformanceAnalytics'))
suppressWarnings(require('later'))

if(Sys.info()[1] == 'Linux') {
  lib('chronR')
} else if(Sys.info()[1] == 'Windows') {
  lib('taskscheduleR')
} else {
  
}

forecastData <- function(price = 'Cl', ahead = 1) {
  forC.EURUSD <- calc_fx(`EURUSD=X`, 'EURUSD=X', price = price, ahead = ahead)
  forC.USDJPY <- calc_fx(`JPY=X`, 'JPY=X', price = price, ahead = ahead)
  forC.GBPUSD <- calc_fx(`GBPUSD=X`, 'GBPUSD=X', price = price, ahead = ahead)
  forC.USDCHF <- calc_fx(`CHF=X`, 'CHF=X', price = price, ahead = ahead)
  forC.USDCAD <- calc_fx(`CAD=X`, 'CAD=X', price = price, ahead = ahead)
  forC.AUDUSD <- calc_fx(`AUDUSD=X`, 'AUDUSD=X', price = price, ahead = ahead)
  
  fxC <- ldply(list(EURUSD = forC.EURUSD, 
                    USDJPY = forC.USDJPY, 
                    GBPUSD = forC.GBPUSD, 
                    USDCHF = forC.USDCHF, 
                    USDCAD = forC.USDCAD, 
                    AUDUSD = forC.AUDUSD), function(x) 
                      data.frame(x$forecastPrice)) %>% 
    unite(., Price.T1, EURUSD:AUDUSD) %>% 
    mutate(Price.T1 = as.numeric(str_replace_all(Price.T1, 'NA|_', '')))
  
  if(price == 'Hi') names(fxC)[2] <- 'Price.T1.Hi'
  if(price == 'Lo') names(fxC)[2] <- 'Price.T1.Lo'
  
  return(fxC)
}



