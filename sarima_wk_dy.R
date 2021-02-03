rm(list = ls())

setwd('C:/Users/User/Documents/GitHub/binary.com-interview-question')

if(!suppressPackageStartupMessages(require('BBmisc'))) {
  install.packages('BBmisc', dependencies = TRUE, INSTALL_opts = '--no-lock')
}
suppressPackageStartupMessages(require('BBmisc'))
# suppressPackageStartupMessages(require('rmsfuns'))

pkgs <- c('devtools', 'knitr', 'kableExtra', 'tint', 'furrr', 'tidyr', 
          'devtools','readr', 'lubridate', 'data.table', 'reprex', 
          'feather', 'purrr', 'quantmod', 'tidyquant', 'tibbletime', 
          'timetk', 'plyr', 'dplyr', 'stringr', 'magrittr', 'tdplyr', 
          'tidyverse', 'memoise', 'htmltools', 'formattable', 'dtplyr', 
          'zoo', 'forecast', 'seasonal', 'seasonalview', 'rjson', 
          'rugarch', 'rmgarch', 'mfGARCH', 'sparklyr', 'jcolors', 
          'microbenchmark', 'dendextend', 'lhmetools', 'ggthemr', 
          'stringr', 'pacman', 'profmem', 'ggthemes', 'flyingfox', 
          'htmltools', 'echarts4r', 'viridis', 'hrbrthemes', 'gtools', 
          'fable', 'fabletools', 'Rfast', 'Metrics', 'MLmetrics')

suppressAll(lib(pkgs))
# load_pkg(pkgs)

.dtr <- 'C:/Users/User/Documents/GitHub/binary.com-interview-question-data/'

## Set the timezone but not change the datetime
Sys.setenv(TZ = 'Asia/Tokyo')
## options(knitr.table.format = 'html') will set all kableExtra tables to be 'html', otherwise need to set the parameter on every single table.
options(warn = -1, knitr.table.format = 'html')#, digits.secs = 6)

## https://stackoverflow.com/questions/39417003/long-vectors-not-supported-yet-abnor-in-rmd-but-not-in-r-script
knitr::opts_chunk$set(cache = TRUE, warning = FALSE, 
                      message = FALSE, cache.lazy = FALSE)

rm(pkgs)


#########################################################################
## check if data path set
if(!exists('dtr')) {
  dtr <- 'C:/Users/User/Documents/GitHub/binary.com-interview-question-data/'}

## save files if not exists
if(!file.exists(paste0(dtr, 'data/fx/USDJPY/dsmp.rds')) & exists('dsmp')) {
  saveRDS(dsmp, paste0(dtr, 'data/fx/USDJPY/dsmp.rds'))}

## read files if not exists
if(!exists('dsmp')) {
  dsmp <- readRDS(paste0(dtr, 'data/fx/USDJPY/dsmp.rds'))}


#########################################################################

timeID <- unique(dsmp$date)
bse <- dsmp[year == 2016]$date[1] #"2016-01-04" #1st trading date in 2nd year
timeID %<>% .[. >= bse]
timeID %<>% .[1:6] ##Take 6 dates as sample
#timeID %<>% .[. >= as_date('2016-01-04')]
data_len <- 7200 #last 7200  observations dsmp[(.N - (data_len - 1)):.N]
hrz1 <- 1440

.model = c('auto', 'Arima')
# .model %<>% .[1]
# .model %<>% .[2]
.d = 0:2
.D = 0:1 
seas = c(TRUE, FALSE)
## https://stackoverflow.com/questions/37400062/seasonality-in-auto-arima-from-forecast-package
#approximation = c(TRUE, FALSE)
#stepwise = c(TRUE, FALSE)

## .order = c(0, 0, 0)
.order = gtools::permutations(6, 3, 0:5, repeats = TRUE) %>% 
  as_tibble %>% 
  dplyr::rename(p = V1, d = V2, q = V3) %>% 
  dplyr::filter(d <= 2)

## .seas = c(0, 0, 0)
.seas = gtools::permutations(6, 3, 0:5, repeats = TRUE) %>% 
  as_tibble %>% 
  dplyr::rename(P = V1, D = V2, Q = V3) %>% 
  dplyr::filter(D <= 1)

source('function/sarima.R')

# --------- eval=FALSE ---------
llply(.model, function(md) {
  
  if(md == 'auto') {
    srm0 <- llply(.d, function(d) {
      srm1 <- llply(.D, function(D) {
        srm2 <- llply(seas, function(ss) {
          srm3 <- llply(timeID, function(dte) {
            res <- sarima(
              timeID = dte, dsmp, 
              data_len = data_len, hrz1 = hrz1, 
              .model = md, .d = d, .D = D, 
              seas = ss, stationary = FALSE, trace = FALSE, 
              ic = c('aicc', 'aic', 'bic'), 
              stepwise = TRUE, nmodels = 94, 
              #approximation=(length(x)>150|frequency(x)>12), 
              truncate = NULL, method = NULL, #x = y, 
              xreg = NULL, test = c('kpss', 'adf', 'pp'), 
              test.args = list(), 
              seasonal.test = c('seas', 'ocsb', 'hegy', 'ch'), 
              seasonal.test.args = list(), 
              allowdrift = TRUE, allowmean = TRUE, lambda = NULL, 
              biasadj = FALSE, parallel = FALSE, num.cores = 2)
            })
          })
        })
      })
    
  } else if (md == 'Arima') {
    srm0 <- llply(1:nrow(.order), function(i) {
      srm1 <- llply(1:nrow(.seas), function(j) {
        srm2 <- llply(timeID, function(dte) {
          res <- sarima(
            timeID = dte, dsmp, 
            data_len = data_len, hrz1 = hrz1, 
            .model = md, .order = unlist(.order[i,]), 
            .seas = unlist(.seas[j,]), xreg = NULL, 
            include.mean = TRUE, include.drift = FALSE, 
            #include.constant = include.constant, 
            #model = NULL, lambda = lambda, x = y, 
            biasadj = FALSE, method = c('CSS-ML', 'ML', 'CSS'))
        })
      })
    })
    
  } else {
    
  }
  return(srm)
})




