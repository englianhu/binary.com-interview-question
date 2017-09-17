setwd('./GitHub/englianhu/binary.com-interview-question')

### 4 gjrGARCH ===========================================================
## Now I try to build ohlc price for best mode - gjrGarch.

gjrGarch <- llply(pp, function(y) {
  z = simStakesGarch(mbase, .solver = .solver.par[1], .prCat = y[1], 
                   .prCat.method = 'CSS-ML', .baseDate = ymd('2015-01-01'), 
                   .parallel = FALSE, .progress = 'text', .realizedVol = Ad(mbase), 
                   .setPrice = y[2], .setPrice.method = 'CSS-ML', 
                   .initialFundSize = 1000, .fundLeverageLog = FALSE, 
                   .filterBets = FALSE, .variance.model = list(
                     model = .variance.model.par[8], garchOrder = c(1, 1), 
                     submodel = NULL, external.regressors = NULL, 
                     variance.targeting = FALSE), 
                   .mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, 
                                      archm = FALSE, archpow = 1, arfima = FALSE, 
                                      external.regressors = NULL, archex = FALSE), 
                   .dist.model = .dist.model.par[2], start.pars = list(), 
                   fixed.pars = list())
  
  txt1 <- paste0('saveRDS(z', ', file = \'./data/', .variance.model.par[8], '.', y[1], '.', y[2], '.', .dist.model.par[2], '.', .solver.par[1], '.rds\')')
  eval(parse(text = txt1))
  cat(paste0(txt1, ' done!', '\n'))
  rm(z)
 })

## Multiple Garch models inside `rugarch` package.
.variance.model.par <- c('sGARCH', 'fGARCH', 'eGARCH', 'gjrGARCH', 'apARCH', 'iGARCH', 'csGARCH', 'realGARCH')

## http://blog.csdn.net/desilting/article/details/39013825
## 接下来需要选择合适的ARIMA模型，即确定ARIMA(p,d,q)中合适的 p、q 值，我们通过R中的“acf()”和“pacf”函数来做判断。
#'@ .garchOrder.par <- expand.grid(0:2, 0:2, KEEP.OUT.ATTRS = FALSE) %>% mutate(PP = paste(Var1, Var2))
#'@ .garchOrder.par %<>% .$PP %>% str_split(' ') %>% llply(as.numeric)

.solver.par <- c('hybrid', 'solnp', 'nlminb', 'gosolnp', 'nloptr', 'lbfgs')

.sub.fGarch.par <- c('GARCH', 'TGARCH', 'AVGARCH', 'NGARCH', 'NAGARCH', 'APARCH', 'GJRGARCH', 'ALLGARCH')

.dist.model.par <- c('norm', 'snorm', 'std', 'sstd', 'ged', 'sged', 'nig', 'ghyp', 'jsu')

pp <- expand.grid(c('Op', 'Hi', 'Mn', 'Lo', 'Cl'), c('Op', 'Hi', 'Mn', 'Lo', 'Cl')) %>% mutate(PP = paste(Var1, Var2)) %>% .$PP %>% str_split(' ')
pp <- llply(pp, function(x) x[x[1]!=x[2]][!is.null(x)])
pp <- pp[!is.na(pp)]

## ============================= Not yet debug =============================
#Error in names(x) <- value : 
#  'names' attribute [3] must be the same length as the vector [2]
pp <- list(c('Hi', 'Lo'), c('Lo', 'Hi'))
##  ========================================================================

source('./function/armaSearch.R')

fitGM.mn <- suppressAll(simGarch(USDJPY, .prCat = 'Mn')) #will take a minute
saveRDS(fitGM.mn, './data/fitGM.mn.rds')

fitGM.lo <- suppressAll(simGarch(USDJPY, .prCat = 'Lo')) #will take a minute
saveRDS(fitGM.lo, './data/fitGM.lo.rds')


fGarch.GJRGARCH <- llply(pp, function(y) {
  z = simStakesGarch(mbase, .solver = .solver.par[1], .prCat = y[1], 
                     .prCat.method = 'CSS-ML', .baseDate = ymd('2015-01-01'), 
                     .parallel = FALSE, .progress = 'text', 
                     .setPrice = y[2], .setPrice.method = 'CSS-ML', 
                     .initialFundSize = 1000, .fundLeverageLog = FALSE, 
                     .filterBets = FALSE, .variance.model = list(
                       model = .variance.model.par[2], garchOrder = c(1, 1), 
                       submodel = 'GJRGARCH', external.regressors = NULL, 
                       variance.targeting = FALSE), 
                     .mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, 
                                        archm = FALSE, archpow = 1, arfima = FALSE, 
                                        external.regressors = NULL, archex = FALSE), 
                     .dist.model = .dist.model.par[1], start.pars = list(), 
                     fixed.pars = list())
  
  txt1 <- paste0('saveRDS(z', ', file = \'./data/', .variance.model.par[2], '.GJRGARCH.', 
                 y[1], '.', y[2], '.', .dist.model.par[1], '.', .solver.par[1], '.rds\')')
  eval(parse(text = txt1))
  cat(paste0(txt1, ' done!', '\n'))
  rm(z)
})

### 4 GARCH ===========================================================
## Now I try to build ohlc price for best mode - gjrGarch.

realGARCH <- llply(pp, function(y) {
  z = simStakesGarch(mbase, .solver = .solver.par[1], .prCat = y[1], 
                   .prCat.method = 'CSS-ML', .baseDate = ymd('2015-01-01'), 
                   .parallel = FALSE, .progress = 'text', .realizedVol = 'Ad', 
                   .setPrice = y[2], .setPrice.method = 'CSS-ML', 
                   .initialFundSize = 1000, .fundLeverageLog = FALSE, 
                   .filterBets = FALSE, .variance.model = list(
                     model = .variance.model.par[8], garchOrder = c(1, 1), 
                     submodel = NULL, external.regressors = NULL, 
                     variance.targeting = FALSE), 
                   .mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, 
                                      archm = FALSE, archpow = 1, arfima = FALSE, 
                                      external.regressors = NULL, archex = FALSE), 
                   .dist.model = .dist.model.par[2], start.pars = list(), 
                   fixed.pars = list())
  
  txt1 <- paste0('saveRDS(z', ', file = \'./data/', .variance.model.par[8], '.', 
                 y[1], '.', y[2], '.', .dist.model.par[2], '.', .solver.par[1], '.rds\')')
  eval(parse(text = txt1))
  cat(paste0(txt1, ' done!', '\n'))
  rm(z)
 })

