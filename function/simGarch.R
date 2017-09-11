simGarch <- function(mbase, .solver = 'hybrid', .prCat = 'Mn', .baseDate = ymd('2015-01-01'), 
                     .parallel = FALSE, .progress = 'none', .method = 'CSS-ML', .realizedVol = Ad(mbase), 
                     .variance.model = list(model = 'sGARCH', garchOrder = c(1, 1), 
                                            submodel = NULL, external.regressors = NULL, 
                                            variance.targeting = FALSE), 
                     .mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, archm = FALSE, 
                                       archpow = 1, arfima = FALSE, external.regressors = NULL, 
                                       archex = FALSE), 
                     .dist.model = 'norm', start.pars = list(), fixed.pars = list()){
  
  #'@ source('./function/armaSearch.R', local = TRUE)
  source('./function/armaSearch.R')
  
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
      .mean.model$armaOrder <- suppressWarnings(armaSearch(obs.data2, .method = .method))
      .mean.model$armaOrder %<>% dplyr::filter(AIC==min(AIC)) %>% .[c('p', 'q')] %>% unlist
      
    } else if(.prCat == 'Hi') {
      obs.data2 <- Hi(mbase)
      .mean.model$armaOrder <- suppressWarnings(armaSearch(obs.data2, .method = .method))
      .mean.model$armaOrder %<>% dplyr::filter(AIC==min(AIC)) %>% .[c('p', 'q')] %>% unlist
      
    } else if(.prCat == 'Mn') { #mean of highest and lowest
      obs.data2 <- cbind(Hi(mbase), Lo(mbase), 
                         USDJPY.Mn = rowMeans(cbind(Hi(mbase), Lo(mbase))))[,-c(1:2)]
      .mean.model$armaOrder <- suppressWarnings(armaSearch(obs.data2, .method = .method))
      .mean.model$armaOrder %<>% dplyr::filter(AIC==min(AIC)) %>% .[c('p', 'q')] %>% unlist
      
    } else if(.prCat == 'Lo') {
      obs.data2 <- Lo(mbase)
      .mean.model$armaOrder <- suppressWarnings(armaSearch(obs.data2, .method = .method))
      .mean.model$armaOrder %<>% dplyr::filter(AIC==min(AIC)) %>% .[c('p', 'q')] %>% unlist
      
    } else if(.prCat == 'Cl') {
      obs.data2 <- Cl(mbase)
      .mean.model$armaOrder <- suppressWarnings(armaSearch(obs.data2, .method = .method))
      .mean.model$armaOrder %<>% dplyr::filter(AIC==min(AIC)) %>% .[c('p', 'q')] %>% unlist
      
    } else {
      stop('Kindly choose .prCat = "Op", .prCat = "Hi", .prCat = "Mn", .prCat = "Lo" or .prCat = "Cl".')
    }
  } else {
    stop('Kindly choose .prCat = "Op", .prCat = "Hi", .prCat = "Mn", .prCat = "Lo" or .prCat = "Cl".')
  }
  
  ## Multiple Garch models inside `rugarch` package.
  .variance.models <- c('sGARCH', 'fGARCH', 'eGARCH', 'gjrGARCH', 
                       'apARCH', 'iGARCH', 'csGARCH', 'realGARCH')
  
  ## do not execute since use `acf()` and `pacf()` can get the best fit p and q values.`
  #'@ .garchOrders <- expand.grid(0:5, 0:5, KEEP.OUT.ATTRS = FALSE) %>% 
  #'@   mutate(PP = paste(Var1, Var2)) %>% .$PP %>% str_split(' ') %>% llply(as.numeric)
  
  .solvers <- c('hybrid', 'solnp', 'nlminb', 'gosolnp', 'nloptr', 'lbfgs')
  
  .sub.fGarchs <- c('GARCH', 'TGARCH', 'AVGARCH', 'NGARCH', 'NAGARCH', 
                   'APARCH', 'GJRGARCH', 'ALLGARCH')
  
  .dist.models <- c('norm', 'snorm', 'std', 'sstd', 'ged', 'sged', 'nig', 'ghyp', 'jsu')
  
  if(!.variance.model$model %in% .variance.models) {
    stop(paste0('kindly choose .variance.model$model among ', 
                paste0('\'', .variance.models, '\'', collapse = ','), '.'))
  } else {
    
    if(.variance.model$model == 'fGARCH') {
      if(length(.variance.model$submodel) == 0)
      stop(paste0('kindly choose .variance.model$submodel among ', 
                  paste0('\'', .sub.fGarchs, '\'', collapse = ','), '.'))
    } else {
      if(length(.variance.model$submodel) > 0)
        stop('kindly choose .variance.model$submodel = NULL.')
    }
  }
  
  ## Error
  #'@ if(!all(.variance.model$garchOrder %in% .garchOrders)) 
  #'@   stop(paste0('kindly choose .variance.model$garchOrder among ', 
  #'@               paste0('\'', .garchOrders, '\'', collapse = ','), '.'))
  
  ## 
  ## Wrong solver will cause error!!!
  ## 
  ## The “hybrid” strategy solver first tries the “solnp” solver, in failing to converge 
  ##   then tries then “nlminb”, the “gosolnp” and finally the “nloptr” solvers.
  ## https://quant.stackexchange.com/questions/7260/r-arma-garch-rugarch-package-doesnt-always-converge?answertab=votes#tab-top
  if(!.solver %in% .solvers) 
    stop(paste0('kindly choose .solver among ', 
                paste0('\'', .solvers, '\'', collapse = ','), '.'))
  
  if(!.dist.model %in% .dist.models) 
    stop(paste0('kindly choose .dist.model among ', 
                paste0('\'', .dist.models, '\'', collapse = ','), '.'))
  
  ## Forecast simulation on the Garch models.
  pred.data <- suppressAll(ldply(dateID, function(dt) {
    if(.variance.model$model == 'realGARCH') {
      smp = obs.data2
      dtr = last(index(smp[index(smp) < dt]))
      smp = smp[paste0(dtr %m-% years(1), '/', dtr)]
      frd = as.numeric(difftime(dt, dtr), units = 'days')
      .realizedVol = Ad(mbase[paste0(dtr %m-% years(1), '/', dtr)])
      
      spec = ugarchspec(variance.model = .variance.model, 
                        mean.model = .mean.model, #realizedVol = .realizedVol, 
                        distribution.model = .dist.model)
      fit = ugarchfit(spec, smp, solver = .solver[1], realizedVol = .realizedVol)
      if(frd > 1) dt = seq(dt - days(frd), dt, by = 'days')[-1]
      fc = ugarchforecast(fit, n.ahead = frd, realizedVol = .realizedVol)
      
    } else {
      
      smp = obs.data2
      dtr = last(index(smp[index(smp) < dt]))
      smp = smp[paste0(dtr %m-% years(1), '/', dtr)]
      frd = as.numeric(difftime(dt, dtr), units = 'days')
      
      spec = ugarchspec(variance.model = .variance.model, 
                        mean.model = .mean.model, 
                        distribution.model = .dist.model)
      fit = ugarchfit(spec, smp, solver = .solver[1])
      if(frd > 1) dt = seq(dt - days(frd), dt, by = 'days')[-1]
      fc = ugarchforecast(fit, n.ahead = frd)
    }
    
    data.frame(Date = dt, Point.Forecast = attributes(fc)[[1]][['seriesFor']][1])
  }, .parallel = .parallel, .progress = .progress)) %>% tbl_df
  
  cmp.data <- xts(pred.data[, -1], order.by = pred.data$Date)
  cmp.data <- cbind(cmp.data, obs.data)
  rm(obs.data, pred.data)
  
  return(cmp.data)
}