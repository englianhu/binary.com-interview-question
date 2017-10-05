setwd('./GitHub/englianhu/binary.com-interview-question')


#'@ rm(list = ls(all = TRUE))

suppressPackageStartupMessages(library('BBmisc'))
pkgs <- c('knitr', 'kableExtra', 'tint', 'devtools', 'lubridate', 'data.table', 'plyr', 'stringr', 'magrittr', 'dplyr', 'tidyr', 'tidyverse', 'ff', 'ffbase', 'tidyquant', 'sparklyr', 'readr', 'quantmod', 'htmltools', 'highcharter', 'googleVis', 'formattable', 'ggfortify', 'DT', 'forecast', 'Mcomp', 'MCMCpack', 'PerformanceAnalytics', 'broom', 'microbenchmark', 'doParallel', 'Boruta', 'fBasics', 'fPortfolio', 'rugarch', 'parma', 'rmgarch')

suppressAll(lib(pkgs))
#'@ install.packages(pkgs, lib = 'C:/Program Files/R/R-3.4.0/library')

## load chunks `saveQ1E`.
#'@ load('./Q1E.RData')
load('./Q1E-read-data3.RData')

suppressAll(l_ply(c('last.R', 'Mn.R', 'has.Mn.R', 'simAutoArima.R', 'simAutoArima2.R', 'simGarch.R', 'simGarch2.R', 'plotChart2.R', 'armaSearch.R', 'simGarch.R', 'simGarch2.R', 'quantitative-finance.R'), function(pkg) source(paste0('./function/', pkg))))

## Set option to below if you want to plot an independent webpage with graph 
#'@ op <- options(gvis.plot.tag=NULL)
op <- options(gvis.plot.tag = 'chart')
options(gvis.plot.tag = 'chart', warn = -1)
#'@ options(rpubs.upload.method = 'internal')

## R: llply fully reproducible results in parallel
## https://stackoverflow.com/questions/34946177/r-llply-fully-reproducible-results-in-parallel
cl <- makeCluster(detectCores())
registerDoParallel(cl)

## Create a cluster object to be used for rugarcgh and rmgarch models.
cluster = makePSOCKcluster(15)

## Connect to Spark.
#'@ spark_install()
#'@ sc <- spark_connect(master = 'local')

rm(pkgs)


##  1 day
mbase_day1 <- llply(mbase, function(x) {
  #'@ z %<>% mutate(Date = as.POSIXct(strptime(Date, "%Y-%m-%d %H")))
  z = to.daily(x)
  nn = names(x) %>% str_replace_all('.Close', '')
  names(z) = names(z) %>% str_replace_all('x', nn)
  z})

#'@ fx <- ldply(mbase, dim)
#'@ names(fx) <- c('Currency', 'Rows.all', 'Cols.all')
rm(mbase)

########################################################################
## set all models provided by Garch function.

## Multiple Garch models inside `rugarch` package.
.variance.model.par <- c('sGARCH', 'fGARCH', 'eGARCH', 'gjrGARCH', 'apARCH', 'iGARCH', 'csGARCH', 'realGARCH')

.solver.par <- c('hybrid', 'solnp', 'nlminb', 'gosolnp', 'nloptr', 'lbfgs')

.sub.fGarch.par <- c('GARCH', 'TGARCH', 'AVGARCH', 'NGARCH', 'NAGARCH', 'APARCH', 'GJRGARCH', 'ALLGARCH')

.dist.model.par <- c('norm', 'snorm', 'std', 'sstd', 'ged', 'sged', 'nig', 'ghyp', 'jsu')

## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'
.baseDate = ymd(ymd_hms(.baseDate))

Garch.m <- .variance.model.par

## 3 months
res <- llply(Garch.m[4], function(x) {
  llply(mbase_day1, function(y) {
    z = simGarch(y, .prCat = 'Cl', .baseDate = .baseDate, .dist.model = 'snorm', 
	             .variance.model = list(model = x, garchOrder = c(1, 1), 
                                        submodel = NULL, external.regressors = NULL, 
                                        variance.targeting = FALSE), 
                 .maPeriod = 'months', .unit = 3, .verbose = TRUE)#, 
                 #.bootstrap = TRUE, .boot.method = 'Partial', .boot.solver = 'solnp')
	nm = names(Cl(z)) %>% str_replace_all('.Close', '')
    txt1 <- paste0('saveRDS(z', ', file = \'./data/', nm, '.', x, '.Cl.snorm.hybrid.d3m.rds\'); rm(z)')
    eval(parse(text = txt1))
    cat(paste0(txt1, ' done!', '\n'))
  })
  cat(paste(x, 'done', '\n'))
})


## 6 months
res <- llply(Garch.m[4], function(x) {
  llply(mbase_day1[3:7], function(y) {
    z = simGarch(y, .prCat = 'Cl', .baseDate = .baseDate, .dist.model = 'snorm', 
	             .variance.model = list(model = x, garchOrder = c(1, 1), 
                                        submodel = NULL, external.regressors = NULL, 
                                        variance.targeting = FALSE), 
                 .maPeriod = 'months', .unit = 6, .verbose = TRUE)#, 
                 #.bootstrap = TRUE, .boot.method = 'Partial', .boot.solver = 'solnp')
    nm = names(Cl(z)) %>% str_replace_all('.Close', '')
	txt1 <- paste0('saveRDS(z', ', file = \'./data/', nm, '.', x, '.Cl.snorm.hybrid.d6m.rds\'); rm(z)')
    eval(parse(text = txt1))
    cat(paste0(txt1, ' done!', '\n'))
  })
  cat(paste(x, 'done', '\n'))
})


## 1 years
res <- llply(Garch.m[2], function(x) {
  llply(mbase_day1, function(y) {
    z = simGarch(y, .prCat = 'Cl', .baseDate = .baseDate, .dist.model = 'snorm', 
	             .variance.model = list(model = x, garchOrder = c(1, 1), 
                                        submodel = 'GARCH', external.regressors = NULL, 
                                        variance.targeting = FALSE), 
                 .maPeriod = 'years', .unit = 1, .verbose = TRUE)#, 
                 #.bootstrap = TRUE, .boot.method = 'Partial', .boot.solver = 'solnp')
    nm = names(Cl(z)) %>% str_replace_all('.Close', '')
	txt1 <- paste0('saveRDS(z', ', file = \'./data/', nm, '.', x, '.GARCH.Cl.snorm.hybrid.d1y.rds\'); rm(z)')
    eval(parse(text = txt1))
    cat(paste0(txt1, ' done!', '\n'))
  })
  cat(paste(x, 'done', '\n'))
})

## =========================================

mbase = mbase_day1[[1]]; .prCat = 'Cl'; 
.maPeriod = 'years'; .unit = 1; .verbose = TRUE; .parallel = FALSE
.bootstrap = FALSE

## Multiple Garch models inside `rugarch` package.
.variance.model.par <- c('sGARCH', 'fGARCH', 'eGARCH', 'gjrGARCH', 'apARCH', 'iGARCH', 'csGARCH', 'realGARCH')

.solver.par <- c('hybrid', 'solnp', 'nlminb', 'gosolnp', 'nloptr', 'lbfgs')

.sub.fGarch.par <- c('GARCH', 'TGARCH', 'AVGARCH', 'NGARCH', 'NAGARCH', 'APARCH', 'GJRGARCH', 'ALLGARCH')

.dist.model.par <- c('norm', 'snorm', 'std', 'sstd', 'ged', 'sged', 'nig', 'ghyp', 'jsu')


## Exponential Weighted Moving Average model - EWMA fixed parameters
ewma.spec.fixed <- llply(mbase_day1, function(x) {
  z = simGarch(x, .solver = .solver.par[1], .prCat = 'Cl', .verbose = TRUE, 
                     .baseDate = ymd('2014-01-01'), .maPeriod = 'years', 
                     .unit = 1, .parallel = FALSE, .progress = 'text', 
                     .variance.model = list(
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
  nm = names(Cl(z)) %>% str_replace_all('.Close', '')
  txt1 <- paste0('saveRDS(z', ', file = \'./data/', nm, '.', 
                 .variance.model.par[2], '.GJRGARCH', '.EWMA.fixed.Cl.', 
                 .dist.model.par[2], '.', .solver.par[1], 'd1y.rds\')')
  eval(parse(text = txt1))
  cat(paste0(txt1, ' done!', '\n'))
  rm(z)
})

## Exponential Weighted Moving Average model - EWMA estimated parameters
ewma.spec.est <- llply(mbase_day1, function(x) {
  z = simGarch(x, .solver = .solver.par[1], .prCat = 'Cl', .verbose = TRUE, 
                     .baseDate = ymd('2014-01-01'), .maPeriod = 'years', 
                     .unit = 1, .parallel = FALSE, .progress = 'text',                      
                     .variance.model = list(
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
  nm = names(Cl(z)) %>% str_replace_all('.Close', '')
  txt1 <- paste0('saveRDS(z', ', file = \'./data/',  nm, '.', 
                 .variance.model.par[2], '.GJRGARCH', '.EWMA.est.', 
                 y[1], '.', y[2], '.', .dist.model.par[2], '.', 
                 .solver.par[1], 'd1y.rds\')')
  eval(parse(text = txt1))
  cat(paste0(txt1, ' done!', '\n'))
  rm(z)
})

## itegration Garch model - iGarch
igarch.spec <- llply(mbase_day1, function(x) {
  z = simGarch(x, .solver = .solver.par[1], .prCat = 'Cl', .verbose = TRUE, 
                     .baseDate = ymd('2014-01-01'), .maPeriod = 'years', 
                     .unit = 1, .parallel = FALSE, .progress = 'text', 
                     .variance.model = list(
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
  nm = names(Cl(z)) %>% str_replace_all('.Close', '')
  txt1 <- paste0('saveRDS(z', ', file = \'./data/',  nm, '.', 
                 .variance.model.par[2], '.GJRGARCH', '.', y[1], '.', y[2], '.', 
                 .dist.model.par[2], '.', .solver.par[1], 'd1y.rds\')')
  eval(parse(text = txt1))
  cat(paste0(txt1, ' done!', '\n'))
  rm(z)
})

# ===========================================================================
## norm distribution generates highest ROI compare to snorm.
flsEWMA <- dir('./data', pattern = '.hybrid.Cl.')


fundList <- llply(flsEWMA, function(dt) {
  fl = paste0('./data/', dt)
  if(file.size(fl) > 100) {
    cbind(Model = str_replace_all(dt, '.rds', ''), 
        readRDS(file = fl)) %>% tbl_df
  }})

# ===========================================================================
res <- simGarch(mbase_day1[[1]], .solver = .solver.par[1], .prCat = 'Cl', .baseDate = ymd('2014-01-01'), 
                .maPeriod = 'months', .unit = 3, .verbose = FALSE, .bootstrap = FALSE, 
                .boot.method = 'Partial', .boot.solver = 'solnp', .n.bootfit = 10000, .n.bootpred = 10000, 
                .parallel = FALSE, .progress = 'text', .method = 'CSS-ML', .realizedVol = 'Ad', 
                .variance.model = list(model = .variance.model.par[1], garchOrder = c(1, 1), 
                                       submodel = 'GARCH', external.regressors = NULL, 
                                       variance.targeting = FALSE), 
                .mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, archm = FALSE, 
                                   archpow = 1, arfima = FALSE, external.regressors = NULL, 
                                   archex = FALSE), 
                .dist.model = .dist.model.par[2], start.pars = list(), fixed.pars = list())
nm = names(Cl(res)) %>% str_replace_all('.Close', '')
txt1 <- paste0('saveRDS(res', ', file = \'./data/',  nm, '.', 
                 .variance.model.par[1], '.GARCH.Cl.', 
                 .dist.model.par[1], '.', .solver.par[1], '.d3m.rds\')')
eval(parse(text = txt1))
cat(paste0(txt1, ' done!', '\n'))
rm(res)


## Exponential Weighted Moving Average model - EWMA fixed parameters
ewma.spec.fixed <- llply(mbase_day1, function(x) {
  z = simGarch(x, .solver = .solver.par[1], .prCat = 'Cl', .baseDate = ymd('2014-01-01'), 
               .maPeriod = 'months', .unit = 3, .verbose = FALSE, .bootstrap = FALSE, 
               .boot.method = 'Partial', .boot.solver = 'solnp', .n.bootfit = 10000, .n.bootpred = 10000, 
               .parallel = FALSE, .progress = 'text', .method = 'CSS-ML', .realizedVol = 'Ad', 
               .variance.model = list(model = .variance.model.par[2], garchOrder = c(1, 1), 
                                      submodel = 'GARCH', external.regressors = NULL, 
                                      variance.targeting = FALSE), 
               .mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, archm = FALSE, 
                                  archpow = 1, arfima = FALSE, external.regressors = NULL, 
                                  archex = FALSE), 
               .dist.model = .dist.model.par[2], start.pars = list(), 
			   fixed.pars = list(alpha1 = 1 - 0.94, omega = 0))
  nm = names(Cl(z)) %>% str_replace_all('.Close', '')
  txt1 <- paste0('saveRDS(z', ', file = \'./data/', nm, '.', 
                 .variance.model.par[2], '.GARCH.EWMA.fixed.Cl.', 
                 .dist.model.par[2], '.', .solver.par[1], '.d3m.rds\')')
  eval(parse(text = txt1))
  cat(paste0(txt1, ' done!', '\n'))
  rm(z)
})

## Exponential Weighted Moving Average model - EWMA estimated parameters
ewma.spec.est <- llply(mbase_day1, function(x) {
  z = simGarch(x, .solver = .solver.par[1], .prCat = 'Cl', .baseDate = ymd('2014-01-01'), 
               .maPeriod = 'months', .unit = 3, .verbose = FALSE, .bootstrap = FALSE, 
               .boot.method = 'Partial', .boot.solver = 'solnp', .n.bootfit = 10000, .n.bootpred = 10000, 
               .parallel = FALSE, .progress = 'text', .method = 'CSS-ML', .realizedVol = 'Ad', 
               .variance.model = list(model = .variance.model.par[2], garchOrder = c(1, 1), 
                                      submodel = 'GARCH', external.regressors = NULL, 
                                      variance.targeting = FALSE), 
               .mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, archm = FALSE, 
                                  archpow = 1, arfima = FALSE, external.regressors = NULL, 
                                  archex = FALSE), 
               .dist.model = .dist.model.par[2], start.pars = list(), fixed.pars = list(omega = 0))
  nm = names(Cl(z)) %>% str_replace_all('.Close', '')
  txt1 <- paste0('saveRDS(z', ', file = \'./data/',  nm, '.', 
                 .variance.model.par[2], '.GARCH.EWMA.est.Cl.', 
                 .dist.model.par[2], '.', .solver.par[1], '.d3m.rds\')')
  eval(parse(text = txt1))
  cat(paste0(txt1, ' done!', '\n'))
  rm(z)
})

## itegration Garch model - iGarch
igarch.spec <- llply(mbase_day1, function(x) {
  z = simGarch(x, .solver = .solver.par[1], .prCat = 'Cl', .baseDate = ymd('2014-01-01'), 
               .maPeriod = 'months', .unit = 3, .verbose = FALSE, .bootstrap = FALSE, 
               .boot.method = 'Partial', .boot.solver = 'solnp', .n.bootfit = 10000, .n.bootpred = 10000, 
               .parallel = FALSE, .progress = 'text', .method = 'CSS-ML', .realizedVol = 'Ad', 
               .variance.model = list(model = .variance.model.par[2], garchOrder = c(1, 1), 
                                      submodel = 'GARCH', external.regressors = NULL, 
                                      variance.targeting = FALSE), 
               .mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, archm = FALSE, 
                                  archpow = 1, arfima = FALSE, external.regressors = NULL, 
                                  archex = FALSE), 
               .dist.model = .dist.model.par[2], start.pars = list(), fixed.pars = list())
  nm = names(Cl(z)) %>% str_replace_all('.Close', '')
  txt1 <- paste0('saveRDS(z', ', file = \'./data/',  nm, '.', 
                 .variance.model.par[2], '.GARCH.Cl.', 
                 .dist.model.par[2], '.', .solver.par[1], 'd3m.rds\')')
  eval(parse(text = txt1))
  cat(paste0(txt1, ' done!', '\n'))
  rm(z)
})


