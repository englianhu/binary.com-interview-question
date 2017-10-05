setwd('./GitHub/englianhu/binary.com-interview-question')


#'@ rm(list = ls(all = TRUE))

suppressPackageStartupMessages(library('BBmisc'))
pkgs <- c('knitr', 'kableExtra', 'tint', 'devtools', 'lubridate', 'data.table', 'plyr', 'stringr', 'magrittr', 'dplyr', 'tidyr', 'tidyverse', 'ff', 'ffbase', 'tidyquant', 'sparklyr', 'readr', 'quantmod', 'htmltools', 'highcharter', 'googleVis', 'formattable', 'ggfortify', 'DT', 'forecast', 'Mcomp', 'MCMCpack', 'PerformanceAnalytics', 'broom', 'microbenchmark', 'doParallel', 'Boruta', 'fBasics', 'fPortfolio', 'rugarch', 'parma', 'rmgarch')
 
suppressAll(lib(pkgs))
#'@ install.packages(pkgs, lib = 'C:/Program Files/R/R-3.4.0/library')

suppressAll(l_ply(c('last.R', 'Mn.R', 'has.Mn.R', 'simAutoArima.R', 'simAutoArima2.R', 'simETS.R', 'simETS2.R', 'plotChart2.R', 'armaSearch.R', 'simGarch.R', 'simGarch2.R', 'quantitative-finance.R'), function(pkg) source(paste0('./function/', pkg))))

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

########################################################################
## set all models provided by ets function.
ets.m1 <- c('A', 'M', 'Z')
ets.m2 <- c('N', 'A', 'M', 'Z')
ets.m3 <- c('N', 'A', 'M', 'Z')
ets.m <- do.call(paste0, expand.grid(ets.m1, ets.m2, ets.m3))
rm(ets.m1, ets.m2, ets.m3)

## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'
.baseDate = ymd(ymd_hms(.baseDate))

## 3 months
res <- llply(ets.m, function(x) {
  llply(mbase_day1, function(y) {
    z = simETS(y, .model = x, .prCat = 'Cl', .baseDate = .baseDate, 
               .maPeriod = 'months', .unit = 3, .verbose = TRUE, 
               .simulate = TRUE, .bootstrap = TRUE)
    nm = names(Cl(z)) %>% str_replace_all('.Close', '')
    txt1 = paste0('saveRDS(z', ', file = \'./data/', nm, '.', x, '.Cl.d3m.rds\'); rm(z)')
    eval(parse(text = txt1))
    cat(paste0(txt1, ' done!', '\n'))
  })
  cat(paste(x, 'done', '\n'))
})

## 6 months
res <- llply(ets.m, function(x) {
  llply(mbase_day1, function(y) {
    z = simETS(y, .model = x, .prCat = 'Cl', .baseDate = .baseDate, 
               .maPeriod = 'months', .unit = 6, .verbose = TRUE, 
               .simulate = TRUE, .bootstrap = TRUE)
    nm = names(Cl(z)) %>% str_replace_all('.Close', '')
    txt1 = paste0('saveRDS(z', ', file = \'./data/', nm, '.', x, '.Cl.d6m.rds\'); rm(z)')
    eval(parse(text = txt1))
    cat(paste0(txt1, ' done!', '\n'))
  })
  cat(paste(x, 'done', '\n'))
})

## 1 year
res <- llply(ets.m, function(x) {
  llply(mbase_day1, function(y) {
    z = simETS(y, .model = x, .prCat = 'Cl', .baseDate = .baseDate, 
               .maPeriod = 'years', .unit = 1, .verbose = TRUE, 
               .simulate = TRUE, .bootstrap = TRUE)
    nm = names(Cl(z)) %>% str_replace_all('.Close', '')
    txt1 = paste0('saveRDS(z', ', file = \'./data/', nm, '.', x, '.Cl.d1y.rds\'); rm(z)')
    eval(parse(text = txt1))
    cat(paste0(txt1, ' done!', '\n'))
  })
  cat(paste(x, 'done', '\n'))
})

## =========================================

mbase = mbase_day1[[1]]; .model = 'ZZZ'; .damped = NULL; .additive.only = FALSE
.prCat = 'Cl'; 
.maPeriod = 'years'; .unit = 1; .verbose = TRUE; .parallel = FALSE
.simulate = FALSE; .bootstrap = FALSE; .npaths = 5000


