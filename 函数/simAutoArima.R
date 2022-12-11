## Now we try to use the daily mean value which is (Hi + Lo) / 2.
## Hi for predict daily highest price. (selling daytrade)
## Lo for predict daily lowest price. (buying daytrade)
simAutoArima <- function(mbase, .prCat = 'Mn', .baseDate = ymd('2015-01-01'), armaOrder = FALSE, 
                         .maPeriod = 'years', .unit = 1, .verbose = FALSE, .parallel = FALSE) {
  ## Auto Arima model will auto choose optimal p and q among ARIMA(0,0) to ARIMA(5,5), therefore 
  ##   no need to set value p and value q.
  ## https://stackoverflow.com/questions/19483952/how-to-extract-integration-order-d-from-auto-arima
  
  if(!is.xts(mbase)) mbase <- xts(mbase[, -1], order.by = mbase$Date)
  
  ## dateID
  dateID <- index(mbase)
  if(!is.Date(.baseDate)) {
    dateID0 <- ymd(.baseDate); rm(.baseDate)
  } else {
    dateID0 <- .baseDate; rm(.baseDate)
  }
  dateID <- dateID[dateID >= dateID0]
  
  ## Set as our daily settlement price.
  obs.data <- mbase[index(mbase) > dateID0]
  price.category <- c('Op', 'Hi', 'Mn', 'Lo', 'Cl')
  
  if(.prCat %in% price.category) {
    if(.prCat == 'Op') {
      obs.data2 <- Op(mbase)
	  
    } else if(.prCat == 'Hi') {
      obs.data2 <- Hi(mbase)
      
    } else if(.prCat == 'Mn') { #mean of highest and lowest
      obs.data2 <- cbind(Hi(mbase), Lo(mbase), 
                         USDJPY.Mn = rowMeans(cbind(Hi(mbase), Lo(mbase))))[,-c(1:2)]
      
    } else if(.prCat == 'Lo') {
      obs.data2 <- Lo(mbase)
      
    } else if(.prCat == 'Cl') {
      obs.data2 <- Cl(mbase)
      
    } else {
      stop('Kindly choose .prCat = "Op", .prCat = "Hi", .prCat = "Mn", .prCat = "Lo" or .prCat = "Cl".')
    }
  } else {
    stop('Kindly choose .prCat = "Op", .prCat = "Hi", .prCat = "Mn", .prCat = "Lo" or .prCat = "Cl".')
  }
  
  ## Forecast simulation on the ets models.
  pred.data <- ldply(dateID, function(dt) {
    smp = obs.data2
    dtr = xts::last(index(smp[index(smp) < dt]))
    
    if(.maPeriod == 'months') {
      smp = smp[paste0(dtr %m-% months(.unit), '/', dtr)]
    }
    if(.maPeriod == 'years') {
      smp = smp[paste0(dtr %m-% years(.unit), '/', dtr)]
    }
    frd = as.numeric(difftime(dt, dtr, units = 'days'))
	fit = auto.arima(smp) #exponential smoothing model.
	if(frd > 1) dt = seq(dt - days(frd), dt, by = 'days')[-1]
    if(.verbose == TRUE) cat(paste('frd=', frd, ';dt=', dt, '\n'))
    data.frame(Date = dt, forecast(fit, h = frd)) %>% tbl_df
  }, .parallel = .parallel) %>% tbl_df
  
  cmp.data <- xts(pred.data[, -1], order.by = pred.data$Date)
  cmp.data <- cbind(cmp.data, obs.data)
  rm(obs.data, pred.data)
  
  return(cmp.data)
}