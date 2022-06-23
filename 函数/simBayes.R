simBayes <- function(mbase, .method = 'bayesglm', .family = 'gaussian', .form = 1, 
                     .prCat = 'Mn', .baseDate = ymd('2015-01-01'), 
                     .parallel = FALSE, .progress = 'none'){
  
  require('arm', quietly  = TRUE)
  require('MCMCpack', quietly  = TRUE)
  require('quantmod', quietly  = TRUE)
  source('./function/Mn.R')
  source('./function/has.Mn.R')
  
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
  
  ## Forecast simulation on the Garch models.
  pred.data <- suppressAll(ldply(dateID, function(dt) {
    smp = cbind(obs.data2, mbase)
    dtr = last(index(smp[index(smp) < dt]))
    smp = smp[paste0(dtr %m-% years(1), '/', dtr)]
    frd = as.numeric(difftime(dt, dtr), units = 'days')
	
	if(.method == 'bayesglm') {
	
	  if(.form == 1) {
	    form = formula(paste(names(obs.data2), '~ USDJPY.High + USDJPY.Low - USDJPY.Open'))
	    fit = bayesglm(form, data = smp)
	  } else if(.form == 2) {
	    form = formula(paste(names(obs.data2), '~ USDJPY.High * USDJPY.Low - USDJPY.Open'))
	    fit = bayesglm(form, data = smp)
	  } else if(.form == 3) {
	    form = formula(paste(names(obs.data2), '~ USDJPY.High:USDJPY.Low - USDJPY.Open'))
	    fit = bayesglm(form, data = smp)
	  } else {
	    stop('Kindly choose .form = 1, 2 or 3.')
	  }
      if(frd > 1) dt = seq(dt - days(frd), dt, by = 'days')[-1]
        data.frame(Date = dt, predict(fit, h = frd)) %>% tbl_df
	
	} else if(.form == 'MCMCregress') {
	  
	  if(.form == 1) {
	    form = formula(paste(names(obs.data2), '~ USDJPY.High + USDJPY.Low - USDJPY.Open'))
	    fit = MCMCregress(form, data = smp)
	  } else if(.form == 2) {
	    form = formula(paste(names(obs.data2), '~ USDJPY.High * USDJPY.Low - USDJPY.Open'))
	    fit = MCMCregress(form, data = smp)
	  } else if(.form == 3) {
	    form = formula(paste(names(obs.data2), '~ USDJPY.High:USDJPY.Low - USDJPY.Open'))
	    fit = MCMCregress(form, data = smp)
	  } else {
	    stop('Kindly choose .form = 1, 2 or 3.')
	  }
      if(frd > 1) dt = seq(dt - days(frd), dt, by = 'days')[-1]
        data.frame(Date = dt, predict(fit, h = frd)) %>% tbl_df
	}
  }, .parallel = .parallel, .progress = .progress)) %>% tbl_df
  
  cmp.data <- xts(pred.data[, -1], order.by = pred.data$Date)
  cmp.data <- cbind(cmp.data, obs.data)
  rm(obs.data, pred.data)
  
  return(cmp.data)
}