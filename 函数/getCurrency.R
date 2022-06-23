getCurrency <- function(symbol, freq, period) {
  source('./function/quantitative-finance.R')
  
  currencies <- c('USDJPY', 'USDEUR', 'USDGBP', 'USDAUD', 'USDCAD', 'USDCHF')
  
  ## The function only able get maximum 50 days data from now.
  curr.minute <- llply(paste0(2:50, period), function(pd) { 
    f.get.google.intraday('USDJPY', freq, pd)
    }) %>% do.call(rbind, .)
  
  mbase$High <- mbase$Open
  mbase$Low <- mbase$Open
  mbase$Close <- mbase$Open
  
  saveRDS(mbase, './data/USDJPY.Minute.rds')
  USDJPY.Minute3 <- to.minutes3(USDJPY.Minute)
  
  
  
  return(cur)
  }