## Now we try to use the daily mean value which is (Hi + Lo) / 2.
## Hi for predict daily highest price. (selling daytrade)
## Lo for predict daily lowest price. (buying daytrade)
simAutoArima2 <- function(mbase, .prCat = 'Mn', 
                          .baseDate = as.POSIXct(strptime('2015-01-01', "%Y-%m-%d %H:%M:%S")), 
                         .maPeriod = 'years', .unit = 1, .difftime = 'days', 
                         .verbose = FALSE, .parallel = FALSE) {
  #' Auto Arima model
  #' 
  
  if(!is.xts(mbase)) mbase <- xts(mbase[, -1], order.by = mbase$Date)
  
  ## dateID
  dateID <- index(mbase)
  dateID <- as.POSIXct(strptime(dateID, "%Y-%m-%d %H:%M:%S")) %>% sort
  
  if(!is.Date(.baseDate)) {
    #'@ dateID0 <- ymd(.baseDate); rm(.baseDate)
    dateID0 <- as.POSIXct(strptime(.baseDate, "%Y-%m-%d %H:%M:%S")); rm(.baseDate)
  } else {
    dateID0 <- as.POSIXct(strptime(.baseDate, "%Y-%m-%d %H:%M:%S")); rm(.baseDate)
  }
  dateID <- dateID[dateID >= dateID0]
  
  ## Set as our daily settlement price.
  obs.data <- mbase[index(mbase) > dateID0]
  price.category <- c('Op', 'Hi', 'Mn', 'Lo', 'Cl')
  maPeriods <- c('secs', 'mins', 'hours', 'days', 'weeks', 'months', 'years')
  
  if(!is.numeric(.unit)) stop('.unit is a numeric parameter.')
  
  if(!.maPeriod %in% maPeriods) stop(paste0('Kindly choose .maPeriod among c(\'', 
                                            paste(maPeriods, collapse = ', '), '\').'))
  
  if(!.difftime %in% maPeriods) stop(paste0('Kindly choose .maPeriod among c(\'', 
                                            paste(maPeriods, collapse = ', '), '\').'))
  
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
    dtr = last(index(smp[index(smp) < dt]))
    
    if(.maPeriod == 'mins') {
      if(.difftime == 'mins') {
        smp = smp[paste0(dtr %m-% minutes(.unit), '/', dtr)]
        frd = as.numeric(difftime(dt, dtr), units = .difftime)
        fit = auto.arima(smp) #Auto Arima model.
        if(frd > 1) dt = seq(dt - minutes(frd), dt, by = .difftime)[-1]
        if(.verbose == TRUE) cat(paste('frd=', frd, ';dt=', dt, '\n'))
      }
      
    } else if(.maPeriod == 'hours') {
      if(.difftime == 'mins') {
        smp = smp[paste0(dtr %m-% hours(.unit), '/', dtr)]
        frd = as.numeric(difftime(dt, dtr), units = .difftime)
        fit = auto.arima(smp) #Auto Arima model.
        if(frd > 1) dt = seq(dt - minutes(frd), dt, by = .difftime)[-1]
        if(.verbose == TRUE) cat(paste('frd=', frd, ';dt=', dt, '\n'))
      }
      
    } else if(.maPeriod == 'days') {
      if(.difftime == 'mins') {
        smp = smp[paste0(dtr %m-% days(.unit), '/', dtr)]
        frd = as.numeric(difftime(dt, dtr), units = .difftime)
        fit = auto.arima(smp) #Auto Arima model.
        if(frd > 1) dt = seq(dt - minutes(frd), dt, by = .difftime)[-1]
        if(.verbose == TRUE) cat(paste('frd=', frd, ';dt=', dt, '\n'))
      }
      
    } else if(.maPeriod == 'weeks') {
      if(.difftime == 'hours') {
        smp = smp[paste0(dtr %m-% weeks(.unit), '/', dtr)]
        frd = as.numeric(difftime(dt, dtr), units = .difftime)
        fit = auto.arima(smp) #Auto Arima model.
        if(frd > 1) dt = seq(dt - hours(frd), dt, by = .difftime)[-1]
        if(.verbose == TRUE) cat(paste('frd=', frd, ';dt=', dt, '\n'))
      }

    } else if(.maPeriod == 'months') {
      if(.difftime == 'days') {
        smp = smp[paste0(dtr %m-% months(.unit), '/', dtr)]
        frd = as.numeric(difftime(dt, dtr), units = .difftime)
        fit = auto.arima(smp) #Auto Arima model.
        if(frd > 1) dt = seq(dt - days(frd), dt, by = .difftime)[-1]
        if(.verbose == TRUE) cat(paste('frd=', frd, ';dt=', dt, '\n'))
      }
      
      if(.difftime == 'hours') {
        smp = smp[paste0(dtr %m-% weeks(.unit), '/', dtr)]
        frd = as.numeric(difftime(dt, dtr), units = .difftime)
        fit = auto.arima(smp) #Auto Arima model.
        if(frd > 1) dt = seq(dt - hours(frd), dt, by = .difftime)[-1]
        if(.verbose == TRUE) cat(paste('frd=', frd, ';dt=', dt, '\n'))
      }

    } else if(.maPeriod == 'years') {
      if(.difftime == 'days') {
        smp = smp[paste0(dtr %m-% years(.unit), '/', dtr)]
        frd = as.numeric(difftime(dt, dtr), units = .difftime)
        fit = auto.arima(smp) #Auto Arima model.
        if(frd > 1) dt = seq(dt - days(frd), dt, by = .difftime)[-1]
        if(.verbose == TRUE) cat(paste('frd=', frd, ';dt=', dt, '\n'))
      }
      
    } else {
      stop('Kindly choose .maPeriod and .difftime among c("mins", "hours", "days", "weeks", "months", "years").')
    }
    
    data.frame(Date = dt, forecast(fit, h = frd)) %>% tbl_df
  }, .parallel = .parallel) %>% tbl_df
  
  cmp.data <- xts(pred.data[, -1], order.by = pred.data$Date)
  cmp.data <- cbind(cmp.data, obs.data)
  rm(obs.data, pred.data)
  
  return(na.omit(cmp.data))
  
  }

