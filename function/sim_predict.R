sim_predict <- function(mbase, .getFX = NULL, .from = NULL, .to = NULL, 
                        timeID0 = ymd(index(mbase)[100]), 
                        .preCat = 'Op', .preCat2 = NULL, .setPrice = 'Cl', 
                        currency = 'JPY=X', ahead = 1) {
  suppressWarnings(require('pryr', quietly = TRUE))
  source('function/forecastUSDJPYHL.R')
  
  curr <- c('AUDUSD=X', 'EURUSD=X', 'GBPUSD=X', 'CHF=X', 'CAD=X', 
            'CNY=X', 'JPY=X')
  
  if (!is.null(.getFX)) {

    ## Get data.
    if (.getFX %in% curr) {
      .getFX <- .getFX
      
      if (!is.null(.from)) {
        .from = ymd(.from)
        
      } else {
        stop('Kindly entry the start date of your dataset.')
      }
      
      if (!is.null(.to)) {
        .to = ymd(.to)
        
      } else {
        stop('Kindly entry the end date of your dataset.')
      }
      
      if(ymd(.to) < .from) stop('End date cannot old than Start date.')
      
      mbase <- getSymbols(.getFX, src = 'yahoo', from = .from, 
                           to = .to, auto.assign = FALSE)
    }
  }
  
  if(!is.xts(mbase)) mbase <- xts(mbase[, -1], order.by = mbase$Date)
  if(nrow(mbase) < 100) 
    warning('observation number suggest to at least 100 to increse the accuracy of prediction.')
  
  ## dateID
  dateID <- index(mbase)
  timeID0 <- ymd(timeID0)
  dateID <- dateID[dateID > timeID0]
  obs.data <- mbase[index(mbase) > timeID0]
  
  #start <- seq(1, 652, 109)
  #stop <- start - 1
  #stop <- c(stop[-1], length(dateID))
  #data.frame(dateID = paste0('dateID = dateID[', start, ':', stop, ']'))
  
  ## Now we try to use the daily mean value which is (Hi + Lo) / 2.
  pred.data <- ldply(dateID, function(dt) {
    smp = mbase
    dtr = last(index(smp[index(smp) < dt]))
    smp = smp[paste0(dtr %m-% years(1), '/', dtr)]
    frd = as.numeric(difftime(dt, dtr), units = 'days')
    fit = suppressAll(forecastUSDJPYHL(
      smp, .preCat = .preCat, .preCat2 = .preCat2, .setPrice = .setPrice, 
      currency = currency, ahead = ahead))
    cat(paste('Latest Date (GMT):', fit$LatestDate.GMT, 'done!\n'))
    
    fit %>% tbl_df
  }, .parallel = FALSE) %>% tbl_df
  
  rm(obs.data)
  
  return(pred.data)
}

