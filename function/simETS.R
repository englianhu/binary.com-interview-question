## Now we try to use the daily mean value which is (Hi + Lo) / 2.
## Hi for predict daily highest price. (selling daytrade)
## Lo for predict daily lowest price. (buying daytrade)
simETS <- function(mbase, .model = 'ZZZ', .damped = NULL, .additive.only = FALSE, 
                     .prCat = 'Mn', .baseDate = ymd('2015-01-01'), .parallel = FALSE, 
                     .simulate = FALSE, .bootstrap = FALSE, .npaths = 5000) {
  #' Exponential smoothing state space model
  #' 
  #' Returns ets model applied to \code{y}.
  #' 
  #' Based on the classification of methods as described in Hyndman et al (2008).
  #' 
  #' The methodology is fully automatic. The only required argument for ets is
  #' the time series. The model is chosen automatically if not specified. This
  #' methodology performed extremely well on the M3-competition data. (See
  #' Hyndman, et al, 2002, below.)
  #' 
  #'@ aliases print.ets summary.ets as.character.ets coef.ets tsdiag.ets
  #' 
  #'@ param y a numeric vector or time series of class \code{ts}
  #'@ param model Usually a three-character string identifying method using the
  #' framework terminology of Hyndman et al. (2002) and Hyndman et al. (2008).
  #' The first letter denotes the error type ("A", "M" or "Z"); the second letter
  #' denotes the trend type ("N","A","M" or "Z"); and the third letter denotes
  #' the season type ("N","A","M" or "Z"). In all cases, "N"=none, "A"=additive,
  #' "M"=multiplicative and "Z"=automatically selected. So, for example, "ANN" is
  #' simple exponential smoothing with additive errors, "MAM" is multiplicative
  #' Holt-Winters' method with multiplicative errors, and so on.
  
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
  
  if(!is.character(.model)) {
    stop('Kindly insert 3 characters only. First character must within c("A", "M", "Z"), c("N", "A", "M", "Z") and c("N", "A", "M", "Z").')
  }
  if(nchar(.model) != 3) {
    stop('Kindly insert 3 characters only. First character must within c("A", "M", "Z"), c("N", "A", "M", "Z") and c("N", "A", "M", "Z").')
  }
  
  errortype <- substr(.model, 1, 1)
  trendtype <- substr(.model, 2, 2)
  seasontype <- substr(.model, 3, 3)
  
  ##> microbenchmark::microbenchmark(!is.element(errortype, c('A', 'M', 'Z')))
  ##Unit: microseconds
  ##                                     expr   min   lq    mean median    uq    max neval
  ## !is.element(errortype, c("A", "M", "Z")) 1.026 1.54 3.64893  2.053 2.053 96.479   100
  ##> microbenchmark::microbenchmark(errortype %in% c('A', 'M', 'Z'))
  ##Unit: microseconds
  ##                            expr   min   lq    mean median    uq    max neval
  ## errortype %in% c("A", "M", "Z") 1.027 1.54 3.35126 2.0525 2.053 89.294   100
  
  if(!errortype %in% c('A', 'M', 'Z')) 
    stop('Invalid error type')
  if(!trendtype %in% c('N', 'A', 'M', 'Z')) 
    stop('Invalid trend type')
  if(!seasontype %in% c('N', 'A', 'M', 'Z')) 
    stop('Invalid season type')
  
  ## Forecast simulation on the ets models.
  pred.data <- ldply(dateID, function(dt) {
    smp = obs.data2
    dtr = last(index(smp[index(smp) < dt]))
    smp = smp[paste0(dtr %m-% years(1), '/', dtr)]
    frd = as.numeric(difftime(dt, dtr), units = 'days')
    fit = ets(smp, model = .model, 
              damped = .damped, additive.only = .additive.only) #exponential smoothing model.
    if(frd > 1) dt = seq(dt - days(frd), dt, by = 'days')[-1]
    data.frame(Date = dt, forecast(fit, h = frd, simulate = .simulate, 
                                   bootstrap = .bootstrap, npaths = .npaths)) %>% tbl_df
  }, .parallel = .parallel) %>% tbl_df
  
  cmp.data <- xts(pred.data[, -1], order.by = pred.data$Date)
  cmp.data <- cbind(cmp.data, obs.data)
  rm(obs.data, pred.data)
  
  return(cmp.data)
}

