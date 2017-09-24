simGarch <- function(mbase, .solver = 'hybrid', .prCat = 'Mn', .baseDate = ymd('2015-01-01'), 
                     .maPeriod = 'years', .unit = 1, .verbose = FALSE, 
                     .parallel = FALSE, .progress = 'none', .method = 'CSS-ML', .realizedVol = 'Ad', 
                     .variance.model = list(model = 'sGARCH', garchOrder = c(1, 1), 
                                            submodel = NULL, external.regressors = NULL, 
                                            variance.targeting = FALSE), 
                     .mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, archm = FALSE, 
                                        archpow = 1, arfima = FALSE, external.regressors = NULL, 
                                        archex = FALSE), 
                     .dist.model = 'norm', start.pars = list(), fixed.pars = list()){
  
  #'@ source('./function/armaSearch.R', local = TRUE)
  library('zoo')
  library('quantmod')
  source('./function/armaSearch.R')
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
  price.category <- c('Op', 'Hi', 'Mn', 'Lo', 'Cl', 'Ad')
  
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
      
    } else if(.prCat == 'Ad') {
      obs.data2 <- Ad(mbase)
      .mean.model$armaOrder <- suppressWarnings(armaSearch(obs.data2, .method = .method))
      .mean.model$armaOrder %<>% dplyr::filter(AIC==min(AIC)) %>% .[c('p', 'q')] %>% unlist
      
    } else {
      stop('Kindly choose .prCat = "Op", .prCat = "Hi", .prCat = "Mn", .prCat = "Lo", .prCat = "Cl" or .prCat = "Ad".')
    }
  } else {
    stop('Kindly choose .prCat = "Op", .prCat = "Hi", .prCat = "Mn", .prCat = "Lo", .prCat = "Cl" or .prCat = "Ad".')
  }
  
  if(!.realizedVol %in% c('Op', 'Hi', 'Mn', 'Lo', 'Cl', 'Ad', 'Vo')) {
    stop('Kindly choose .realizedVol = "Op", "Hi", "Mn", "Lo", "Cl", "Ad" or "Vo".')
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
      
      if(.maPeriod == 'months') {
        smp = smp[paste0(dtr %m-% months(.unit), '/', dtr)]
      }
      if(.maPeriod == 'years') {
        smp = smp[paste0(dtr %m-% years(.unit), '/', dtr)]
      }
      frd = as.numeric(difftime(dt, dtr, units = 'days'))
      
      spec = ugarchspec(variance.model = .variance.model, 
                        mean.model = .mean.model, #realizedVol = .rVol, 
                        distribution.model = .dist.model)
      if(.realizedVol == 'Op') {
        rVol = Delt(Op(mbase))[paste0(dtr %m-% years(1), '/', dtr)]
        rVol[1] = 1
        names(rVol) = names(Op(mbase)) %>% str_replace_all('.Open', '') %>% paste0(., '.Ret')
        fit = ugarchfit(spec, smp, solver = .solver[1], realizedVol = rVol)
        if(frd > 1) dt = seq(dt - days(frd), dt, by = 'days')[-1]
        if(.verbose == TRUE) cat(paste('frd=', frd, ';dt=', dt, '\n'))
        fc = ugarchforecast(fit, n.ahead = frd, realizedVol = rVol)
        
      } else if(.realizedVol == 'Hi') {
        rVol = Delt(Hi(mbase))[paste0(dtr %m-% years(1), '/', dtr)]
        rVol[1] = 1
        names(rVol) = names(Hi(mbase)) %>% str_replace_all('.High', '') %>% paste0(., '.Ret')
        fit = ugarchfit(spec, smp, solver = .solver[1], realizedVol = rVol)
        if(frd > 1) dt = seq(dt - days(frd), dt, by = 'days')[-1]
        if(.verbose == TRUE) cat(paste('frd=', frd, ';dt=', dt, '\n'))
        fc = ugarchforecast(fit, n.ahead = frd, realizedVol = rVol)
        
      } else if(.realizedVol == 'Mn') {
        rVol = Delt(Mn(mbase))[paste0(dtr %m-% years(1), '/', dtr)]
        rVol[1] = 1
        names(rVol) = names(Mn(mbase)) %>% str_replace_all('.Mean', '') %>% paste0(., '.Ret')
        fit = ugarchfit(spec, smp, solver = .solver[1], realizedVol = rVol)
        if(frd > 1) dt = seq(dt - days(frd), dt, by = 'days')[-1]
        if(.verbose == TRUE) cat(paste('frd=', frd, ';dt=', dt, '\n'))
        fc = ugarchforecast(fit, n.ahead = frd, realizedVol = rVol)
        
      } else if(.realizedVol == 'Lo') {
        rVol = Delt(Lo(mbase))[paste0(dtr %m-% years(1), '/', dtr)]
        rVol[1] = 1
        names(rVol) = names(Lo(mbase)) %>% str_replace_all('.Low', '') %>% paste0(., '.Ret')
        fit = ugarchfit(spec, smp, solver = .solver[1], realizedVol = rVol)
        if(frd > 1) dt = seq(dt - days(frd), dt, by = 'days')[-1]
        if(.verbose == TRUE) cat(paste('frd=', frd, ';dt=', dt, '\n'))
        fc = ugarchforecast(fit, n.ahead = frd, realizedVol = rVol)
        
      } else if(.realizedVol == 'Cl') {
        rVol = Delt(Cl(mbase))[paste0(dtr %m-% years(1), '/', dtr)]
        rVol[1] = 1
        names(rVol) = names(Cl(mbase)) %>% str_replace_all('.Close', '') %>% paste0(., '.Ret')
        fit = ugarchfit(spec, smp, solver = .solver[1], realizedVol = rVol)
        if(frd > 1) dt = seq(dt - days(frd), dt, by = 'days')[-1]
        if(.verbose == TRUE) cat(paste('frd=', frd, ';dt=', dt, '\n'))
        fc = ugarchforecast(fit, n.ahead = frd, realizedVol = rVol)
        
      } else if(.realizedVol == 'Ad') {
        rVol = Delt(Ad(mbase))[paste0(dtr %m-% years(1), '/', dtr)]
        rVol[1] = 1
        names(rVol) = names(Ad(mbase)) %>% str_replace_all('.Adjusted', '') %>% paste0(., '.Ret')
        fit = ugarchfit(spec, smp, solver = .solver[1], realizedVol = rVol)
        if(frd > 1) dt = seq(dt - days(frd), dt, by = 'days')[-1]
        if(.verbose == TRUE) cat(paste('frd=', frd, ';dt=', dt, '\n'))
        fc = ugarchforecast(fit, n.ahead = frd, realizedVol = rVol)
        
      } else if(.realizedVol == 'Vo') {
        rVol = Delt(Vo(mbase))[paste0(dtr %m-% years(1), '/', dtr)]
        rVol[1] = 1
        names(rVol) = names(Vo(mbase)) %>% str_replace_all('.Volume', '') %>% paste0(., '.Ret')
        fit = ugarchfit(spec, smp, solver = .solver[1], realizedVol = rVol)
        if(frd > 1) dt = seq(dt - days(frd), dt, by = 'days')[-1]
        if(.verbose == TRUE) cat(paste('frd=', frd, ';dt=', dt, '\n'))
        fc = ugarchforecast(fit, n.ahead = frd, realizedVol = rVol)
        
      } else {
        stop('Kindly choose .realizedVol = "Op", "Hi", "Mn", "Lo", "Cl", "Ad" or "Vo".')
      }
      
      
    } else {
      
      smp = obs.data2
      dtr = last(index(smp[index(smp) < dt]))
      smp = smp[paste0(dtr %m-% years(1), '/', dtr)]
      frd = as.numeric(difftime(dt, dtr, units = 'days'))
      
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