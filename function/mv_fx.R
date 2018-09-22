mv_fx <- memoise(function(mbase, .mv.model = 'dcc', .model = 'DCC', .tram = 'parametric', 
                          .VAR = FALSE, .dist.model = 'mvnorm', .currency = 'JPY=X', 
                          .ahead = 1, .price_type = 'OHLC', .solver = 'solnp', 
                          .var.target = FALSE, .roll = FALSE, .cluster = FALSE) {
  
  require('BBmisc', quietly = TRUE)
  suppressAll(lib('plyr', 'dplyr', 'matrixcalc', 'quantmod', 'MASS'))
  
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
  
  .price_types <- c('OHLC', 'HLC', 'HL', 'C')
  
  if(!.price_type %in% .price_types) {
    stop(paste0('.price_type must be in \'', paste(.price_types, collapse = ', '), '\'.'))
  }
  
  if (.price_type == 'OHLC') {
    mbase <- cbind(Op(mbase), Hi(mbase), Lo(mbase), Cl(mbase))
    mbase %<>% na.omit
    
  } else if (.price_type == 'HLC') {
    mbase <- cbind(Hi(mbase), Lo(mbase), Cl(mbase))
    mbase %<>% na.omit
    
  } else if (.price_type == 'HL') {
    mbase <- cbind(Hi(mbase), Lo(mbase))
    mbase %<>% na.omit
    
  } else if (.price_type == 'C') {
    mbase <- Cl(mbase)
    mbase %<>% na.omit
    
  } else {
    stop(paste0('.price_type must be in \'', paste(.price_types, collapse = ', '), '\'.'))
  }
  
  if (.cluster == TRUE) {
    cl <- makePSOCKcluster(ncol(mbase))
  } else {
    cl <- NULL
  }
  
  if (.mv.model == 'dcc') {
    # --------------------- DCC -----------------------------
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
    speclist <- filter_spec(mbase, .currency = .currency, .price_type = .price_type)
    mspec <- multispec(speclist)
    
    dccSpec <- dccspec(
      mspec, VAR = .VAR, lag = 1, 
      lag.criterion = c('AIC', 'HQ', 'SC', 'FPE'), 
      external.regressors = NULL, #external.regressors = VAREXO, 
      dccOrder = c(1, 1), model = .model, 
      distribution = .dist.model) # Below article compares distribution model and 
    #   concludes that the 'mvt' is the best.
    # http://www.unstarched.net/2013/01/03/the-garch-dcc-model-and-2-stage-dccmvt-estimation/
    
    if (.roll == TRUE) {
      mod = dccroll(dccSpec, data = mbase, solver = .solver, 
                    forecast.length = nrow(mbase), cluster = cl)
      cat('step 1/1 dccroll done!\n')
      
    } else {
      
      ## No need multifit()
      #'@ multf <- multifit(mspec, data = mbase, cluster = cl)
      #'@ cat('step 1/3 multifit done!\n')
      
      #'@ fit <- dccfit(dccSpec, data = mbase, solver = .solver, fit = multf, 
      #'@               cluster = cl)
      #'@ cat('step 2/3 dccfit done!\n')
      
      ## http://r.789695.n4.nabble.com/how-to-test-significance-of-VAR-coefficients-in-DCC-GARCH-Fit-td4472274.html
      if (.VAR == TRUE) {
        vfit = varxfit(X = mbase, p = 1, exogen = NULL, robust = FALSE, 
                       gamma = 0.25, delta = 0.01, nc = 10, ns = 500, 
                       postpad = 'constant')
      } else {
        vfit <- NULL
      }
      
      fit <- dccfit(dccSpec, data = mbase, solver = .solver, cluster = cl, 
                    VAR.fit = vfit)
      cat('step 1/2 dccfit done!\n')
      
      fc <- dccforecast(fit, n.ahead = .ahead, cluster = cl)
      #'@ cat('step 3/3 dccforecast done!\n')
      cat('step 2/2 dccforecast done!\n')
    }
    
  } else if (.mv.model == 'go-GARCH') {
    # -------------------go-GARCH ---------------------------
    
    ## I simply use the mean value of multivariate and round.
    armaOrder <- ldply(mbase, opt_arma) %>% 
      .[,-1] %>% colMeans %>% round(0)
    
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
    ## http://r.789695.n4.nabble.com/how-to-test-significance-of-VAR-coefficients-in-DCC-GARCH-Fit-td4472274.html
    if (.VAR == TRUE) {
      vfit = varxfit(X = mbase, p = 1, exogen = NULL, robust = FALSE, 
                     gamma = 0.25, delta = 0.01, nc = 10, ns = 500, 
                     postpad = 'constant')
    } else {
      vfit <- NULL
    }
    
    spec <- gogarchspec(
      variance.model = list(
        model = 'gjrGARCH', garchOrder = c(1, 1),    # Univariate Garch 2012 powerpoint.pdf
        submodel = NULL, external.regressors = NULL, #   compares the garchOrder and 
        variance.targeting = FALSE),                 #   concludes garch(1,1) is the best fit.
      mean.model = list(
        model = .model, robust = FALSE), 
      distribution.model = .dist.model)
    
    fit <- gogarchfit(spec, mbase, solver = 'hybrid', VAR.fit = vfit, 
                      cluster = cl)
    cat('step 1/2 gogarchfit done!\n')
    
    if (.roll == TRUE) {
      mod = gogarchroll(spec, data = mbase, solver = .solver, cluster = cl)
      cat('step 2/1 gogarchroll done!\n')
      
    } else {
      fc <- gogarchforecast(fit, n.ahead = .ahead, cluster = cl)
      cat('step 2/2 gogarchforecast done!\n')
    }
    
  } else if (.mv.model == 'mv-goGARCH') {
    # --------------- mv-goGARCH ---------------------------------
    
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
    ## http://r.789695.n4.nabble.com/how-to-test-significance-of-VAR-coefficients-in-DCC-GARCH-Fit-td4472274.html
    if (.VAR == TRUE) {
      vfit = varxfit(X = mbase, p = 1, exogen = NULL, robust = FALSE, 
                     gamma = 0.25, delta = 0.01, nc = 10, ns = 500, 
                     postpad = 'constant')
    } else {
      vfit <- NULL
    }
    
    ## .dist.model = 'mvt' since mvt produced most accurate outcome.
    speclist <- filter_spec(mbase, .currency = .currency, .price_type = .price_type)
    mspec <- multispec(speclist)
    
    goSpec <- gogarchspec(mspec, 
                          variance.model = list(
                            model = 'gjrGARCH', garchOrder = c(1, 1),    # Univariate Garch 2012 powerpoint.pdf
                            submodel = NULL, external.regressors = NULL, #   compares the garchOrder and 
                            variance.targeting = FALSE),                 #   concludes garch(1,1) is the best fit.
                          mean.model = list(
                            model = .model, robust = FALSE), 
                          distribution.model = .dist.model)
    
    if (.roll == TRUE) {
      mod = gogarchroll(goSpec, data = mbase, solver = .solver, 
                        forecast.length = nrow(mbase), cluster = cl)
      cat('step 1/1 gogarchroll done!\n')
      
    } else {
      
      ## http://r.789695.n4.nabble.com/how-to-test-significance-of-VAR-coefficients-in-DCC-GARCH-Fit-td4472274.html
      if (.VAR == TRUE) {
        vfit = varxfit(X = mbase, p = 1, exogen = NULL, robust = FALSE, 
                       gamma = 0.25, delta = 0.01, nc = 10, ns = 500, 
                       postpad = 'constant')
      } else {
        vfit <- NULL
      }
      
      fit <- gogarchfit(dccSpec, data = mbase, solver = 'hybrid', cluster = cl, 
                        VAR.fit = vfit)
      cat('step 1/2 gogarchfit done!\n')
      
      fc <- gogarchforecast(fit, n.ahead = .ahead, cluster = cl)
      cat('step 2/2 gogarchforecast done!\n')
    }
    
  } else if (.mv.model == 'copula-GARCH') {
    # --------------- copula-GARCH ------------------------------
    
    sv <- c('solnp', 'nlminb', 'lbfgs', 'gosolnp')
    if (!.solver %in% sv) {
      stop(".solver must be %in% c('solnp', 'nlminb', 'lbfgs', 'gosolnp')")
    } else {
      .solver <- .solver
    }
    
    md <- c('Kendall', 'ML')
    if (!.model %in% md) {
      stop(".model must be %in% c('Kendall', 'ML')")
    } else {
      .model <- .model
    }
    
    trams <- c('parametric', 'empirical', 'spd')
    if (!.tram %in% trams) {
      stop(".model must be %in% c('parametric', 'empirical', 'spd')")
    } else {
      .tram <- trams
    }
    
    ## .dist.model = 'mvt' since mvt produced most accurate outcome.
    speclist <- filter_spec(mbase, .currency = .currency, .price_type = .price_type, 
                            var.target = .var.target)
    mspec <- multispec(speclist)
    
    cSpec <- cgarchspec(
      mspec, VAR = .VAR, lag = 1, 
      lag.criterion = c('AIC', 'HQ', 'SC', 'FPE'), 
      external.regressors = NULL, #external.regressors = VAREXO, 
      dccOrder = c(1, 1), asymmetric = FALSE, ##wether use `aDCC` or normal `DCC` model.
      distribution.model = list(
        time.varying = TRUE, copula = .dist.model, 
        method = .model, transformation = .tram), 
      start.pars = list(), fixed.pars = list())
    # Below article compares distribution model and 
    #   concludes that the 'mvt' is the best.
    # http://www.unstarched.net/2013/01/03/the-garch-dcc-model-and-2-stage-dccmvt-estimation/
    
    if (.roll == TRUE) {
      mod = dccroll(cSpec, data = mbase, solver = .solver, 
                    forecast.length = nrow(mbase), cluster = cl)
      cat('step 1/1 dccroll done!\n')
      
    } else {
      
      ## http://r.789695.n4.nabble.com/how-to-test-significance-of-VAR-coefficients-in-DCC-GARCH-Fit-td4472274.html
      if (.VAR == TRUE) {
        vfit = varxfit(X = mbase, p = 1, exogen = NULL, robust = FALSE, 
                       gamma = 0.25, delta = 0.01, nc = 10, ns = 500, 
                       postpad = 'constant')
        
        fit <- cgarchfit(cSpec, data = mbase, solver = .solver, cluster = cl, 
                         VAR.fit = vfit)
        cat('step 1/2 cgarchfit done!\n')
        
        ## https://rdrr.io/rforge/rgarch/man/varxfilter.html
        ## https://rdrr.io/rforge/rmgarch/src/R/rmgarch-tests.R
        ## https://github.com/cran/rmgarch/blob/master/R/copula-postestimation.R
        fc <- varxforecast(X = mbase, Bcoef = fit@mfit$stdresid, p = 4, 
                           out.sample = 0, n.ahead = .ahead, n.roll = 0, mregfor = NULL)
        cat('step 2/2 varxforecast done!\n')
      } else {
        vfit <- NULL
        
        fit <- cgarchfit(cSpec, data = mbase, solver = .solver, cluster = cl, 
                         VAR.fit = vfit)
        cat('step 1/2 cgarchfit done!\n')
        
        ## https://stackoverflow.com/questions/34855831/forecasting-for-dcc-copula-garch-model-in-r
        ## http://r.789695.n4.nabble.com/copula-with-rmgarch-td4616138.html
        fc <- cgarchsim(fit, n.sim = ncol(mbase), n.start = 0, m.sim = .ahead, 
                        presigma = tail(sigma(fit), 1), startMethod = 'sample', 
                        preR = rcor(fit), preQ = fit@mfit$Qt[[length(fit@mfit$Qt)]], 
                        preZ = tail(fit@mfit$Z, 1), prereturns = tail(mbase, 1), 
                        preresiduals = tail(fit@mfit$stdresid, 1), cluster = cl)#, rseed = 1)
        
        res <- matrix(fc@msim$simZ, nr = 1)# + sim1@msim$simX[[1]]
        cat('step 2/2 cgarchsim done!\n')
        
        ## retrieve the VaR value for forecast n.ahead = 1
        VaR <- ldply(
          list(T1.VaR_01 = qnorm(0.01) * as.data.frame(sigma(fc)) + as.data.frame(fitted(fc)), 
               T1.VaR_05 = qnorm(0.05) * as.data.frame(sigma(fc)) + as.data.frame(fitted(fc)), 
               T1.VaR_95 = qnorm(0.95) * as.data.frame(sigma(fc)) + as.data.frame(fitted(fc)), 
               T1.VaR_99 = qnorm(0.99) * as.data.frame(sigma(fc)) + as.data.frame(fitted(fc)))) %>% 
          mutate(mshape = attributes(fit)$mfit$coef['[Joint]mshape'])
        
        vm <- names(VaR) %>% 
          grep('Open|High|Low|Close', ., value=TRUE) %>% 
          substr(1, nchar(.) - 11)
        names(VaR[,2:(ncol(VaR)-1)]) <- vm
        
        tmp = list(latestPrice = latestPrice, forecastPrice = res, 
                   variance = sigma(fc), forecastVaR = VaR, 
                   fit = fit, forecast = fc, AIC = AIC)
        return(tmp)
      }
      
    }
    
  } else {
    stop("Kindly set .mv.model as 'dcc', 'go-GARCH', 'mv-goGARCH' or 'copula-GARCH'.")
  }
  
  if (.roll == TRUE) {
    return(report(mod, type = 'fpm'))
    
  } else {
    
    res = fitted(fc)
    colnames(res) = names(mbase)
    latestPrice = tail(mbase, 1)
    
    #rownames(res) <- as.character(forDate)
    latestPrice <- xts(latestPrice)
    #res <- as.xts(res)
    
    if (.mv.model == 'dcc') {
      AIC = infocriteria(fit)
      
    } else if (.mv.model == 'go-GARCH') {
      AIC = llply(attributes(attributes(fit)$mfit$ufit)$fit, infocriteria)
      names(AIC) <- attributes(fit)$model$modeldata$asset.names
      
    } else if (.mv.model == 'mv-goGARCH') {
      AIC = infocriteria(fit)
      
    }else if (.mv.model == 'copula-GARCH') {
      AIC = infocriteria(fit)
    }
    
    ## retrieve the VaR value for forecast n.ahead = 1
    VaR <- ldply(
      list(T1.VaR_01 = qnorm(0.01) * as.data.frame(sigma(fc)) + as.data.frame(fitted(fc)), 
           T1.VaR_05 = qnorm(0.05) * as.data.frame(sigma(fc)) + as.data.frame(fitted(fc)), 
           T1.VaR_95 = qnorm(0.95) * as.data.frame(sigma(fc)) + as.data.frame(fitted(fc)), 
           T1.VaR_99 = qnorm(0.99) * as.data.frame(sigma(fc)) + as.data.frame(fitted(fc)))) %>% 
      mutate(mshape = attributes(fit)$mfit$coef['[Joint]mshape'])
    
    vm <- names(VaR) %>% 
      grep('Open|High|Low|Close', ., value=TRUE) %>% 
      substr(1, nchar(.) - 11)
    names(VaR[,2:(ncol(VaR)-1)]) <- vm
    
    tmp = list(latestPrice = latestPrice, forecastPrice = res, 
               variance = sigma(fc), forecastVaR = VaR, 
               fit = fit, forecast = fc, AIC = AIC)
    return(tmp)
  }
})

