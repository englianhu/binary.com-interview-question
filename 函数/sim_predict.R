sim_predict <- function(mbase, .getFX = NULL, .from = NULL, .to = NULL, 
                        timeID = index(mbase), timeID0 = index(mbase)[100], 
                        .preCat = 'Op', .preCat2 = NULL, .preCat3 = NULL, 
						.preCat4 = NULL, .save = FALSE, currency = 'JPY=X', 
						ahead = 1) {

  suppressWarnings(require('pryr', quietly = TRUE))
  source('function/forecastUSDJPYHL.R')
  
  cr_code <- c('AUDUSD=X', 'EURUSD=X', 'GBPUSD=X', 'CHF=X', 'CAD=X', 
               'CNY=X', 'JPY=X')
  
  cr_name <- c('AUDUSD', 'EURUSD', 'GBPUSD', 'USDCHF', 'USDCAD', 
               'USDCNY', 'USDJPY')
  
  names(cr_code) <- cr_name
  names(cr_name) <- cr_code
  
  if (!is.null(.getFX)) {
  
    ## Get data.
    if (.getFX %in% cr_code) {
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
  
  if(!is.xts(mbase)) mbase <- xts(
    dpyr::select(mbase, -Date), order.by = mbase$Date)
  if(nrow(mbase) < 100) 
    warning('observation number suggest to at least 100 to increse the accuracy of prediction.')
  
  ## timeID or dateID
  timeID <- ymd(timeID)
  timeID0 <- ymd(timeID0)
  timeID <- timeID[timeID > timeID0]
  obs.data <- mbase[timeID > timeID0]
  
  #start <- seq(1, 652, 109)
  #stop <- start - 1
  #stop <- c(stop[-1], length(index(mbase)))
  #data.frame(timeID = paste0('timeID', seq(start), ' = index(mbase)[', start, ':', stop, ']'))
  #rm(start, stop)
  #                           timeID
  #1   timeID1 = index(mbase)[1:109]
  #2 timeID2 = index(mbase)[110:218]
  #3 timeID3 = index(mbase)[219:327]
  #4 timeID4 = index(mbase)[328:436]
  #5 timeID5 = index(mbase)[437:545]
  #6 timeID6 = index(mbase)[546:911]
  
  ## Now we try to use the daily mean value which is (Hi + Lo) / 2.
  pred.data <- ldply(timeID, function(dt) {
    smp = mbase
    dtr = xts::last(index(smp[index(smp) < dt]), 1) #tail(..., 1)
    smp = smp[paste0(dtr %m-% years(1), '/', dtr)]
    frd = as.numeric(difftime(dt, dtr), units = 'days')
    fit = suppressAll(forecastUSDJPYHL(
      smp, .preCat = .preCat, .preCat2 = .preCat2, .preCat3 = .preCat3, 
	  .preCat4 = .preCat4, currency = currency, ahead = ahead))
    cat(paste('Latest Date (GMT):', fit$LatestDate.GMT, 'done!\n'))
	
	if(.save == TRUE) {
		saveRDS(fit, paste0('data/USDJPY/pred.', fit$LatestDate.GMT, '.rds'))
		cat(paste('Latest Date (GMT):', fit$LatestDate.GMT, 'saved!\n\n'))
	}
    
  fit %>% tbl_df
  }, .parallel = FALSE) %>% tbl_df
  
  rm(obs.data)
  
  return(pred.data)
}

