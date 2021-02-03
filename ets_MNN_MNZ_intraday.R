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


## best ETS models by referred to Interday High Frequency Trading Models Comparison Review (Part I).
#ets.m <- c('MNN', 'MNZ')
ets.m <- 'MNN'
source('function/tseas_intraday.R')


# --------- eval=FALSE ---------
timeID <- unique(dsmp$date)
bse <- dsmp[year == 2016]$date[1] #"2016-01-04" #1st trading date in 2nd year
timeID %<>% .[. >= bse]
#timeID %<>% .[. >= as_date('2016-01-04')]
data_len <- 1440 #last 1440  observations dsmp[(.N - (data_len - 1)):.N]
hrz1 <- 720
hrz2 <- 720

llply(ets.m, function(md) {
  tseas_intraday(timeID = timeID, dsmp, 
                 data_len = data_len, hrz1 = hrz1, 
                 hrz2 = hrz2, .model = md)
  })


#########################################################################
## Check forecast files observations
lst <- list.files(paste0(.dtr, 'data/fx/USDJPY'), pattern = '^ts_ets_MNZ_1440_1440|^ts_ets_MNN_1440_1440')

cnt <- lst %>% ldply(., function(x) {readRDS(paste0(.dtr, 'data/fx/USDJPY/', x)) %>% nrow})
cnt

## Check forecast files observations
lst <- list.files(paste0(.dtr, 'data/fx/USDJPY'), pattern = '^ts_ets_MNN_1440_720')
lst <- matrix(lst, ncol = 2) %>% t %>% as.vector
c(head(lst), tail(lst))

cnt <- lst %>% ldply(., function(x) {readRDS(paste0(.dtr, 'data/fx/USDJPY/', x)) %>% nrow}) %>% as.data.table
cnt

c(head(lst), tail(lst)) %>% llply(., function(x) {readRDS(paste0(.dtr, 'data/fx/USDJPY/', x)) %>% as.data.table})




