setwd('./GitHub/englianhu/binary.com-interview-question')


#'@ rm(list = ls(all = TRUE))

suppressPackageStartupMessages(library('BBmisc'))
pkgs <- c('knitr', 'kableExtra', 'tint', 'devtools', 'lubridate', 'data.table', 'plyr', 'stringr', 'magrittr', 'dplyr', 'tidyr', 'tidyverse', 'ff', 'ffbase', 'tidyquant', 'sparklyr', 'readr', 'quantmod', 'htmltools', 'highcharter', 'googleVis', 'formattable', 'ggfortify', 'DT', 'forecast', 'Mcomp', 'MCMCpack', 'PerformanceAnalytics', 'broom', 'microbenchmark', 'doParallel', 'Boruta', 'fBasics', 'fPortfolio', 'rugarch', 'parma', 'rmgarch')

suppressAll(lib(pkgs))
#'@ install.packages(pkgs, lib = 'C:/Program Files/R/R-3.4.0/library')

suppressAll(l_ply(c('last.R', 'Mn.R', 'has.Mn.R', 'simAutoArima.R', 'simAutoArima2.R', 'simGarch.R', 'simGarch2.R', 'plotChart2.R', 'armaSearch.R', 'simGarch.R', 'simGarch2.R', 'quantitative-finance.R'), function(pkg) source(paste0('./function/', pkg))))

## load chunks `saveQ1E`.
#'@ load('./Q1E.RData')
load('./Q1E-read-data3.RData')

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

## set all models provided by Garch function.
#'@ source('./function/armaSearch.R', local = TRUE)
  library('zoo')
  library('quantmod')
  source('./function/armaSearch.R')
  source('./function/Mn.R')
  source('./function/has.Mn.R')
  
## Multiple Garch models inside `rugarch` package.
.variance.model.par <- c('sGARCH', 'fGARCH', 'eGARCH', 'gjrGARCH', 'apARCH', 'iGARCH', 'csGARCH', 'realGARCH')

.solver.par <- c('hybrid', 'solnp', 'nlminb', 'gosolnp', 'nloptr', 'lbfgs')

.sub.fGarch.par <- c('GARCH', 'TGARCH', 'AVGARCH', 'NGARCH', 'NAGARCH', 'APARCH', 'GJRGARCH', 'ALLGARCH')

.dist.model.par <- c('norm', 'snorm', 'std', 'sstd', 'ged', 'sged', 'nig', 'ghyp', 'jsu')

## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'
.baseDate = ymd(ymd_hms(.baseDate))

Garch.m <- .variance.model.par

mbase = mbase_day1[[1]]; .solver = .solver.par[1]; .prCat = 'Cl'; 
.maPeriod = 'years'; .unit = 1; .verbose = TRUE; .parallel = FALSE; .progress = 'text'
.bootstrap = FALSE; .baseDate = ymd('2014-01-01'); .method = 'CSS-ML'; .realizedVol = 'Ad'
.boot.method = 'Partial'; .boot.solver = 'solnp'; .n.bootfit = 10000; .n.bootpred = 10000; 
.variance.model = list(model = .variance.model.par[4], garchOrder = c(1, 1), 
                       submodel = NULL, external.regressors = NULL, 
                       variance.targeting = FALSE); 
.mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, archm = FALSE, 
                   archpow = 1, arfima = FALSE, external.regressors = NULL, 
                   archex = FALSE); 
.dist.model = .dist.model.par[2]; start.pars = list(); fixed.pars = list()

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
  
obs.data2 <- Cl(mbase)
.mean.model$armaOrder <- suppressWarnings(armaSearch(obs.data2, .method = .method))
.mean.model$armaOrder %<>% dplyr::filter(AIC==min(AIC)) %>% .[c('p', 'q')] %>% unlist


## Forecast simulation on the Garch models.
  pred.data <- suppressAll(

    ldply(dateID, function(dt) {

      smp = obs.data2
      dtr = xts::last(index(smp[index(smp) < dt]))
	  	  
      if(.maPeriod == 'months') {
        smp = smp[paste0(dtr %m-% months(.unit), '/', dtr)]
      }
      if(.maPeriod == 'years') {
        smp = smp[paste0(dtr %m-% years(.unit), '/', dtr)]
      }
      
      frd = as.numeric(difftime(dt, dtr, units = 'days'))      
      spec = ugarchspec(variance.model = .variance.model, 
                        mean.model = .mean.model, 
                        distribution.model = .dist.model)
      fit = ugarchfit(spec, smp, solver = 'hybrid')
      if(frd > 1) dt = seq(dt - days(frd), dt, by = 'days')[-1]
      if(.verbose == TRUE) cat(paste('frd=', frd, ';dt=', dt, '\n'))
      if(.bootstrap == TRUE) {
          fc = ugarchboot(fit, n.ahead = frd, method = .boot.method, solver = .boot.solver, 
                          n.bootfit = .n.bootfit, n.bootpred = .n.bootpred)
        } else {
          fc = ugarchforecast(fit, n.ahead = frd)
      }
	  if(is.atomic(fc)) {
	    fcc = fc 
	  } else {
	    fcc = attributes(fc)$forecast$seriesFor
	  }
    yy = data.frame(Date = dt, Point.Forecast = fcc)	
	names(yy) = c('Date', 'Point.Forecast')
	yy
  }, .parallel = .parallel, .progress = .progress)) %>% tbl_df
    
  cmp.data <- xts(pred.data[, -1], order.by = pred.data$Date)
  cmp.data <- cbind(cmp.data, obs.data)
  rm(obs.data, pred.data)
  cmp.data
  
  