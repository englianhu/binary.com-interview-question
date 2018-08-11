mv_fx <- memoise(function(mbase, .mv.model = 'dcc', .model = 'aDCC', 
                          .dist.model = 'mvnorm', .currency = 'JPY=X', 
                          .ahead = 1, .include.Op = TRUE, .Cl.only = FALSE, 
                          .solver = 'solnp') {
  
  require(plyr, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  require(quantmod, quietly = TRUE)
  
  funs <- c('filterFX.R', 'opt_arma.R', 'filter_spec.R')
  l_ply(funs, function(x) source(paste0('function/', x)))
  
  if (!is.xts(mbase)) mbase <- xts(mbase[, -1], order.by = mbase$Date)
  mbase %<>% na.omit
  
  ## Here I compare the efficiency of filtering dataset.
  ## 
  #> microbenchmark(cbind(Op(mbase[[1]]), Hi(mbase[[1]]), Lo(mbase[[1]]), Cl(mbase[[1]])))
  #Unit: milliseconds
  #expr      min       lq
  #cbind(Op(mbase[[1]]), Hi(mbase[[1]]), Lo(mbase[[1]]), Cl(mbase[[1]])) 1.557214 1.629989
  #mean   median       uq      max neval
  #1.907246 1.748483 1.981272 5.213727   100
  #> microbenchmark(mbase[[1]][,grep('Open|High|Low|Close', names(mbase[[1]]))])
  #Unit: microseconds
  #expr     min       lq     mean
  #mbase[[1]][, grep("Open|High|Low|Close", names(mbase[[1]]))] 256.581 264.9785 297.6247
  #median     uq      max neval
  #270.577 280.84 1008.597   100
  
  if (.include.Op == TRUE & .Cl.only == FALSE) { 
    mbase <- cbind(Op(mbase), Hi(mbase), Lo(mbase), Cl(mbase))
    
  } else if (.include.Op == FALSE & .Cl.only == FALSE) {
    base <- cbind(Hi(mbase), Lo(mbase), Cl(mbase))
    
  } else if ((.include.Op == TRUE & .Cl.only == TRUE)|
             (.include.Op == FALSE & .Cl.only == TRUE)) {
    mbase <- Cl(mbase)
    mbase %<>% na.omit
    
  } else {
    stop(".Cl.only = TRUE will strictly only get closing price.")
  }
  
  if (.mv.model == 'dcc') {
    
    sv <- c('solnp', 'nlminb', 'lbfgs', 'gosolnp')
    if (!.solver %in% sv) {
      stop(".solver must be %in% c('solnp', 'nlminb', 'lbfgs', 'gosolnp')")
    } else {
      .solver <- .solver
    }
    
    md <- c('DCC', 'aDCC', 'FDCC')
    if (!.model %in% md) {
      stop(".model must be %in% c('DCC', 'aDCC', 'FDCC')")
    } else {
      .model <- .model
    }
    
    ## .dist.model = 'mvt' since mvt produced most accurate outcome.
    speclist <- filter_spec(mbase, .currency = .currency, 
                            .include.Op = .include.Op, .Cl.only = .Cl.only)
    mspec <- multispec(speclist)
    
    dccSpec <- dccspec(
      mspec, VAR = TRUE, lag = 1, 
      lag.criterion = c('AIC', 'HQ', 'SC', 'FPE'), 
      external.regressors = NULL, #external.regressors = VAREXO, 
      dccOrder = c(1, 1), model = .model, 
      distribution = 'mvt') # Below article compares distribution model and 
                            #   concludes that the 'mvt' is the best.
                            # http://www.unstarched.net/2013/01/03/the-garch-dcc-model-and-2-stage-dccmvt-estimation/
    
    fit <- dccfit(dccSpec, data = mbase, solver = .solver)
    fc <- dccforecast(fit, n.ahead = .ahead)
    
  } else if (.mv.model == 'go-GARCH') {
    
    armaOrder <- opt_arma(mbase)
    
    md <- c('constant', 'AR', 'VAR')
    if (!.model %in% md) {
      stop(".model must be %in% c('constant', 'AR', 'VAR')")
    } else {
      .model <- .model
    }
    
    .dist.models <- c('mvnorm', 'manig', 'magh')
    if (.dist.model %in% .dist.models) {
      .dist.model <- .dist.model
    } else {
      stop(".dist.model must %in% c('mvnorm', 'manig', 'magh').")
    }
    
    spec <- gogarchspec(
      variance.model = list(
        model = 'gjrGARCH', garchOrder = c(1, 1),    # Univariate Garch 2012 powerpoint.pdf
        submodel = NULL, external.regressors = NULL, #   compares the garchOrder and 
        variance.targeting = FALSE),                 #   concludes garch(1,1) is the best fit.
      mean.model = list(
        model = .model, robust = FALSE, 
        lag = 1, lag.max = NULL, lag.criterion = c('AIC', 'HQ', 'SC', 'FPE'), 
        external.regressors = NULL, 
        robust.control = list('gamma' = 0.25, 'delta' = 0.01, 
                              'nc' = 10, 'ns' = 500)), 
      distribution.model = .dist.model)
    
    fit <- gogarchfit(spec, mbase, solver = 'hybrid')
    fc <- gogarchforecast(fit, n.ahead = .ahead)
    
  } else if (.mv.model == 'copula-GARCH') {
    ...
  } else {
    stop("Kindly set .mv.model as 'dcc', 'go-GARCH' or 'copula-GARCH'.")
  }
  
  res = fitted(fc)
  colnames(res) = names(mbase)
  latestPrice = tail(mbase, 1)
  
  #rownames(res) <- as.character(forDate)
  latestPrice <- xts(latestPrice)
  #res <- as.xts(res)
  
  tmp = list(latestPrice = latestPrice, forecastPrice = res, 
             fit = fit, forecast = fc, AIC = infocriteria(fit))
  return(tmp)
})


