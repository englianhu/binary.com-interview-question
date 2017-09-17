## Exponential Weighted Moving Average model - EWMA fixed parameters
ewma.spec.fixed <- llply(pp, function(y) {
  z = simStakesGarch(mbase, .solver = .solver.par[1], .prCat = y[1], 
                     .prCat.method = 'CSS-ML', .baseDate = ymd('2015-01-01'), 
                     .parallel = FALSE, .progress = 'text', 
                     .setPrice = y[2], .setPrice.method = 'CSS-ML', 
                     .initialFundSize = 1000, .fundLeverageLog = FALSE, 
                     .filterBets = FALSE, .variance.model = list(
                       model = .variance.model.par[2], garchOrder = c(1, 1), 
                       submodel = 'GJRGARCH', external.regressors = NULL, 
                       variance.targeting = FALSE), 
                     .mean.model = list(armaOrder = c(1, 1), 
                                        include.mean = TRUE, 
                                        archm = FALSE, archpow = 1, 
                                        arfima = FALSE, 
                                        external.regressors = NULL, 
                                        archex = FALSE), 
                     .dist.model = .dist.model.par[2], start.pars = list(), 
                     fixed.pars = list(alpha1 = 1 - 0.94, omega = 0))
  
  txt1 <- paste0('saveRDS(z', ', file = \'./data/', 
                 .variance.model.par[2], '.GJRGARCH', '.EWMA.fixed.', 
                 y[1], '.', y[2], '.', .dist.model.par[2], '.', 
                 .solver.par[1], '.rds\')')
  eval(parse(text = txt1))
  cat(paste0(txt1, ' done!', '\n'))
  rm(z)
})

## Exponential Weighted Moving Average model - EWMA estimated parameters
ewma.spec.est <- llply(pp, function(y) {
  z = simStakesGarch(mbase, .solver = .solver.par[1], .prCat = y[1], 
                     .prCat.method = 'CSS-ML', .baseDate = ymd('2015-01-01'), 
                     .parallel = FALSE, .progress = 'text', 
                     .setPrice = y[2], .setPrice.method = 'CSS-ML', 
                     .initialFundSize = 1000, .fundLeverageLog = FALSE, 
                     .filterBets = FALSE, .variance.model = list(
                       model = .variance.model.par[2], garchOrder = c(1, 1), 
                       submodel = 'GJRGARCH', external.regressors = NULL, 
                       variance.targeting = FALSE), 
                     .mean.model = list(armaOrder = c(1, 1), 
                                        include.mean = TRUE, 
                                        archm = FALSE, archpow = 1, 
                                        arfima = FALSE, 
                                        external.regressors = NULL, 
                                        archex = FALSE), 
                     .dist.model = .dist.model.par[2], start.pars = list(), 
                     fixed.pars = list(omega = 0))
  
  txt1 <- paste0('saveRDS(z', ', file = \'./data/', 
                 .variance.model.par[2], '.GJRGARCH', '.EWMA.est.', 
                 y[1], '.', y[2], '.', .dist.model.par[2], '.', 
                 .solver.par[1], '.rds\')')
  eval(parse(text = txt1))
  cat(paste0(txt1, ' done!', '\n'))
  rm(z)
})

## itegration Garch model - iGarch
igarch.spec <- llply(pp, function(y) {
  z = simStakesGarch(mbase, .solver = .solver.par[1], .prCat = y[1], 
                     .prCat.method = 'CSS-ML', .baseDate = ymd('2015-01-01'), 
                     .parallel = FALSE, .progress = 'text', 
                     .setPrice = y[2], .setPrice.method = 'CSS-ML', 
                     .initialFundSize = 1000, .fundLeverageLog = FALSE, 
                     .filterBets = FALSE, .variance.model = list(
                       model = .variance.model.par[2], garchOrder = c(1, 1), 
                       submodel = 'GJRGARCH', external.regressors = NULL, 
                       variance.targeting = FALSE), 
                     .mean.model = list(armaOrder = c(1, 1), 
                                        include.mean = TRUE, 
                                        archm = FALSE, archpow = 1, 
                                        arfima = FALSE, 
                                        external.regressors = NULL, 
                                        archex = FALSE), 
                     .dist.model = .dist.model.par[2], start.pars = list(), 
                     fixed.pars = list())
  
  txt1 <- paste0('saveRDS(z', ', file = \'./data/', 
                 .variance.model.par[2], '.GJRGARCH', '.', y[1], '.', y[2], '.', 
                 .dist.model.par[2], '.', .solver.par[1], '.rds\')')
  eval(parse(text = txt1))
  cat(paste0(txt1, ' done!', '\n'))
  rm(z)
})
