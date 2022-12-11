setwd('C:/Users/User/Documents/GitHub/binary.com-interview-question')

if(!suppressPackageStartupMessages(require('BBmisc'))) {
  install.packages('BBmisc', dependencies = TRUE, INSTALL_opts = '--no-lock')
}
suppressPackageStartupMessages(require('BBmisc'))
# suppressPackageStartupMessages(require('rmsfuns'))

pkgs <- c('devtools', 'knitr', 'kableExtra', 'tint', 'dygraphs', 
          'devtools','readr', 'lubridate', 'data.table', 'reprex', 
          'feather', 'purrr', 'quantmod', 'tidyquant', 'plotly', 
          'tibbletime', 'furrr', 'flyingfox', 'tidyr', 'jsonlite', 
          'timetk', 'plyr', 'dplyr', 'stringr', 'magrittr', 'tdplyr', 
          'tidyverse', 'memoise', 'htmltools', 'formattable', 'rbokeh', 
          'dash', 'dashCoreComponents', 'dashHtmlComponents', 'dtplyr', 
          ##https://dashr.plotly.com
          'zoo', 'forecast', 'seasonal', 'seasonalview', 'rjson', 
          'rugarch', 'rmgarch', 'mfGARCH', 'sparklyr', 'jcolors', 
          'microbenchmark', 'dendextend', 'lhmetools', 'ggthemr', 
          'stringr', 'pacman', 'profmem', 'DescTools', 'ggthemes', 
          'htmltools', 'echarts4r', 'viridis', 'hrbrthemes', 
          'fable', 'fabletools', 'Rfast', 'Metrics', 'MLmetrics')

## https://www.jianshu.com/p/4beb3d34ced2
#unlink('C:/Program Files/R/R-4.0.3/library/withr', recursive = TRUE)
#install.packages('withr', dependencies = TRUE, INSTALL_opts = '--no-lock')

# https://github.com/mpiktas/midasr
# https://github.com/onnokleen/mfGARCH
# devtools::install_github("business-science/tibbletime")
# devtools::install_github("DavisVaughan/furrr")

suppressAll(lib(pkgs))
# load_pkg(pkgs)

dtr <- 'C:/Users/User/Documents/GitHub/binary.com-interview-question-data/'

## Set the timezone but not change the datetime
Sys.setenv(TZ = 'Asia/Tokyo')
## options(knitr.table.format = 'html') will set all kableExtra tables to be 'html', otherwise need to set the parameter on every single table.
options(warn = -1, knitr.table.format = 'html')#, digits.secs = 6)

## https://stackoverflow.com/questions/39417003/long-vectors-not-supported-yet-abnor-in-rmd-but-not-in-r-script
knitr::opts_chunk$set(cache = TRUE, warning = FALSE, 
                      message = FALSE, cache.lazy = FALSE)

rm(pkgs)



## check if data path set
if(!exists('dtr')) {
  dtr <- 'C:/Users/User/Documents/GitHub/binary.com-interview-question-data/'}

## save files if not exists
if(!file.exists(paste0(dtr, 'data/fx/USDJPY/dsmp.rds')) & exists('dsmp')) {
  saveRDS(dsmp, paste0(dtr, 'data/fx/USDJPY/dsmp.rds'))}

## read files if not exists
if(!exists('dsmp')) {
  dsmp <- readRDS(paste0(dtr, 'data/fx/USDJPY/dsmp.rds'))}






# --------- eval=FALSE ---------
timeID <- unique(dsmp$date)
bse <- dsmp[year == 2016]$date[1] #"2016-01-04" #1st trading date in 2nd year
timeID %<>% .[. >= bse]
#timeID %<>% .[. >= as_date('2016-01-04')]
data_len <- 7200 #last 7200  observations dsmp[(.N - (data_len - 1)):.N]
hrz1 <- 7200
hrz2 <- 1440

for (i in 780:length(timeID)) {
  
  if(i == 1) {
    
    cat('\n')
    cat('===========================================\n')
    cat('train[', i, ']\n')
    print(train <- dsmp[date < timeID[i]][(.N - (data_len - 1)):.N])
    ctr <- train$sq[1]:(range(train$sq)[2] + hrz1)
    
    cat('\n')
    cat('-------------------------------------------\n')
    cat('train_test[', i, ']\n')
    
    print(train_test <- dsmp[sq %in% ctr])
    
    sets <- train[, .(index, close)] %>% 
      tk_ts(frequency = hrz1) %>% 
      forecast(h = hrz1) %>% 
      tk_tbl %>% 
	    dplyr::mutate(index = train_test[(.N - hrz1 + 1):.N,]$index, 
	                  mk.price = train_test[(.N - hrz1 + 1):.N,]$close) %>% 
      dplyr::rename(fc.price = `Point Forecast`) %>% 
      dplyr::select(index, mk.price, fc.price)
    
    cat('\n')
    cat('-------------------------------------------\n')
    cat('forecast[', i, ']\n')
    
    print(sets %>% as.data.table)
    
    saveRDS(sets, paste0(
      dtr, 'data/fx/USDJPY/ts_', data_len, '_', hrz1, '.', 
      as_date(sets$index[1]), '.rds'))

    cat('\n')
    cat(i, '=', paste0('~/data/fx/USDJPY/ts_', data_len, '_', hrz1, '.', 
                       as_date(sets$index[1]), '.rds saved!\n'))
    cat('\n\n')
    
  } else if(i > (length(timeID) - hrz1/hrz2) & i != length(timeID)) {
    
    lst_sq <- dsmp[date < timeID[i],][.N]$sq + 1
    ## filter the length of forecasted data to fit with train_test data 
	##   when the length of forecasted data more then length of test data.
	#lst_date <- timeID[(length(timeID) - (hrz1/hrz2)):length(timeID)]
	lst_date <- timeID[timeID >= timeID[i]]
	lst_date_sq <- grep(timeID[i], timeID[(length(timeID) - (hrz1/hrz2 - 1)):length(timeID)])
	
	cat('\n')
    cat('===========================================\n')
    cat('train[', i, ']\n')
    
    print(train <- dsmp[(lst_sq - data_len + 1):lst_sq])
    ctr <- train$sq[1]:(range(train$sq)[2] + hrz1)
    
    cat('\n')
    cat('-------------------------------------------\n')
    cat('train_test[', i, ']\n')
    
    print(train_test <- dsmp[sq %in% ctr])
    
    sets <- train[, .(index, close)] %>% 
      tk_ts(frequency = hrz1) %>% 
      forecast(h = hrz1) %>% 
      tk_tbl
	  
	sets <- sets[1:(hrz1 - (hrz2 * lst_date_sq)),] %>% 
      dplyr::mutate(index = train_test[(.N - (hrz1 - (hrz2 * lst_date_sq)) + 1):.N, ]$index, 
                    mk.price = train_test[(.N - (hrz1 - (hrz2 * lst_date_sq)) + 1):.N, ]$close) %>% 
      dplyr::rename(fc.price = `Point Forecast`) %>% 
      dplyr::select(index, mk.price, fc.price)
    
    cat('\n')
    cat('-------------------------------------------\n')
    cat('forecast[', i, ']\n')
    
    print(sets %>% as.data.table)
    
    saveRDS(sets, paste0(
      dtr, 'data/fx/USDJPY/ts_', data_len, '_', hrz1, '.', 
      as_date(sets$index[1]), '.rds'))
    
    cat('\n')
    cat(i, '=', paste0('~/data/fx/USDJPY/ts_', data_len, '_', hrz1, '.', 
                       as_date(sets$index[1]), '.rds saved!\n'))
    cat('\n\n')    
	
  } else if(i %in% seq(1, length(timeID), by = 6)[-1]) {
    
    
  } else if(i == length(timeID)) {
    
    
  } else {
    
    lst_sq <- dsmp[date < timeID[i],][.N]$sq + 1
    
    cat('\n')
    cat('===========================================\n')
    cat('train[', i, ']\n')
    
    print(train <- dsmp[(lst_sq - data_len + 1):lst_sq])
    ctr <- train$sq[1]:(range(train$sq)[2] + hrz1)
    
    cat('\n')
    cat('-------------------------------------------\n')
    cat('train_test[', i, ']\n')
    
    print(train_test <- dsmp[sq %in% ctr])
    
    sets <- train[, .(index, close)] %>% 
      tk_ts(frequency = hrz1) %>% 
      forecast(h = hrz1) %>% 
      tk_tbl %>% 
      dplyr::mutate(index = train_test[(.N - hrz1 + 1):.N,]$index, 
                    mk.price = train_test[(.N - hrz1 + 1):.N,]$close) %>% 
      dplyr::rename(fc.price = `Point Forecast`) %>% 
      dplyr::select(index, mk.price, fc.price)
    
    cat('\n')
    cat('-------------------------------------------\n')
    cat('forecast[', i, ']\n')
    
    print(sets %>% as.data.table)
    
    saveRDS(sets, paste0(
      dtr, 'data/fx/USDJPY/ts_', data_len, '_', hrz1, '.', 
      as_date(sets$index[1]), '.rds'))
    
    cat('\n')
    cat(i, '=', paste0('~/data/fx/USDJPY/ts_', data_len, '_', hrz1, '.', 
                       as_date(sets$index[1]), '.rds saved!\n'))
    cat('\n\n')
  }
}


###############################

timeID <- unique(dsmp$date)
bse <- dsmp[year == 2016]$date[1] #"2016-01-04" #1st trading date in 2nd year
timeID %<>% .[. >= bse]
#timeID %<>% .[. >= as_date('2016-01-04')]
data_len <- 7200 #last 7200  observations dsmp[(.N - (data_len - 1)):.N]
hrz1 <- 1440

i = 785

    lst_sq <- dsmp[date < timeID[i],][.N]$sq + 1
    
    cat('\n')
    cat('===========================================\n')
    cat('train[', i, ']\n')
    
    print(train <- dsmp[(lst_sq - data_len + 1):lst_sq])
    ctr <- train$sq[1]:(range(train$sq)[2] + hrz1)
    
    cat('\n')
    cat('-------------------------------------------\n')
    cat('train_test[', i, ']\n')
    
    print(train_test <- dsmp[sq %in% ctr])
    
    sets <- train[, .(index, close)] %>% 
      tk_ts(frequency = hrz1) %>% 
      forecast(h = hrz1) %>% 
      tk_tbl %>% 
      dplyr::mutate(index = train_test[(.N - hrz1 + 1):.N,]$index, 
                    mk.price = train_test[(.N - hrz1 + 1):.N,]$close) %>% 
      dplyr::rename(fc.price = `Point Forecast`) %>% 
      dplyr::select(index, mk.price, fc.price)
	  
    sets %>% as.data.table

