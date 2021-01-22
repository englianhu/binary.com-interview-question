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
          'tidyverse', 'memoise', 'htmltools', 'formattable', 
          'dash', 'dashCoreComponents', 'dashHtmlComponents', ##https://dashr.plotly.com
          'zoo', 'forecast', 'seasonal', 'seasonalview', 'rjson', 
          'rugarch', 'rmgarch', 'mfGARCH', 'sparklyr', 'jcolors', 
          'microbenchmark', 'dendextend', 'lhmetools', 'ggthemr', 
          'stringr', 'pacman', 'profmem', 'DescTools', 'ggthemes', 
          'htmltools', 'echarts4r', 'viridis', 'hrbrthemes')

# https://github.com/mpiktas/midasr
# https://github.com/onnokleen/mfGARCH
# devtools::install_github("business-science/tibbletime")
# devtools::install_github("DavisVaughan/furrr")

suppressAll(lib(pkgs))
# load_pkg(pkgs)

funs <- c('uv_fx.R', 'opt_arma.R', 'multi_seasons.R', 
          'filterFX.R', 'filter_spec.R', 'mv_fx.R', 
          'task_progress.R', 'read_umodels.R', 'convertOHLC.R')
l_ply(funs, function(x) source(paste0('./function/', x)))

# spark_install()

# if(FALSE) {
  # Not run due to side-effects
#   spark_home_set()
#   }
# sc <- spark_connect(master = 'local')

#spark_install()
#sc <- spark_connect(master = 'local')

.cl = FALSE

Sys.setenv(TZ = 'Asia/Tokyo')
## options(knitr.table.format = 'html') will set all kableExtra tables to be 'html', otherwise need to set the parameter on every single table.
options(warn = -1, knitr.table.format = 'html')#, digits.secs = 6)
rm(pkgs, funs)

cr_code <- c('AUDUSD=X', 'EURUSD=X', 'GBPUSD=X', 'CHF=X', 'CAD=X', 'CNY=X', 'JPY=X')

names(cr_code) <- c('AUDUSD', 'EURUSD', 'GBPUSD', 'USDCHF', 'USDCAD', 'USDCNY', 'USDJPY')
# names(cr_code) <- c('USDAUD', 'USDEUR', 'USDGBP', 'USDCHF', 'USDCAD', 'USDCNY', 'USDJPY')

dtr <- str_extract_all(getwd(), '.*/', '')[1]
dtr1 <- paste0(dtr, 'real-time-fxcm/data/')

## Read presaved FXCM data.
# mbase <- sapply(names(cr_code), function(x) readRDS(paste0('./data/', x, '.rds')) %>% na.omit)

fls <- sapply(names(cr_code), function(x) {
  dtr1 <- paste0(dtr1, x)
  list.files(dtr1, pattern = '^Y[0-9]{4}W[0-9]{1,2}.rds$') %>% 
    str_replace_all('.rds', '')
  })
fls[lengths(fls) == 0] <- NA_character_
fls[is.na(fls)] <- NULL

# AUDUSD <- sapply(fls[[1]], read_rds)
# EURUSD <- sapply(fls[[2]], read_rds)
# GBPUSD <- sapply(fls[[3]], read_rds)
# USDCHF <- sapply(fls[[4]], read_rds)
# USDCAD <- sapply(fls[[5]], read_rds)
# USDCNY <- sapply(fls[[6]], read_rds)
# mbase <- llply(as.list(fls[[7]]), read_rds) #185 files where 1 files contains 1 million observation.

## Here I take USDJPY as example...
dtr1s <- paste0(dtr1, names(fls[length(fls)]))
fs <- list.files(dtr1s, pattern = '^Y[0-9]{4}W[0-9]{1,2}.rds$') %>% 
  str_replace_all('.rds', '')
# eval(parse(text = paste0(fs, "<- readRDS('", fls[[7]], "') %>% as_tibble")))

t.unit <- c('seconds', 'minutes', 'hours', 'days', 'weeks', 'months', 'quarters', 'quarters')
## https://www.alphavantage.co/
## https://www.alphavantage.co/support/#api-key
# api = 'UL7EPVVEGDVC3TXC'
# getSymbols('JPY=X', src='av', api.key=api, periodicity='intraday')

## Get all files.
fls <- paste0('data/fx/USDJPY/', list.files(
    'data/fx/USDJPY/', pattern = '^sets|^mts'))
fls1 <- fls[str_detect(fls, 'sets.qt.7200.')]
baseDate <- fls1[length(fls1)] %>% str_extract_all('[0-9]{4}-[0-9]{2}-[0-9]{2}') %>% .[[1]] %>% as_date
#2015-02-23


# --------- eval=FALSE ---------
#sq <- seq(1 , length(data_m1$index), by = 7200)
#sets <- list()
if(!exists('data_m1')) {
  data_m1 <- read_rds(paste0(dtr1s, '/data_m1.rds'))
}
if(names(data_m1) %>% str_detect('Bid|Ask') %>% any()) {
data_m1 %<>% 
  mutate(open = (BidOpen + AskOpen)/2, 
         high = (BidHigh + AskHigh)/2, 
         low = (BidLow + AskLow)/2, 
         close = (BidClose + AskClose)/2) %>% 
  dplyr::select(index, open, high, low, close)
}


# --------- eval=FALSE ---------
#sq <- seq(1 , length(data_m1$index), by = 7200)
#sets <- list()
timeID <- data_m1$index %>% 
  as_date %>% 
  unique %>% 
  sort
timeID %<>% .[. > as_date('2015-01-05')]

for (dt in timeID) {
  smp <- data_m1 %>% 
    tk_xts(silent = TRUE)
  dt %<>% as_date
  smp <- smp[paste0(dt %m-% months(3) + seconds(59), '/', dt + seconds(59))]
  
  sets <- smp %>% 
    tk_ts(frequency = 7200) %>% 
    forecast(h = 7200) %>% 
    llply(tk_tbl)
  
  if(is.double(sets$forecast$index[1])){
    sq <- smp %>% 
      tail(1) %>% 
      index
    if(weekdays(sq) == '土曜日'|weekdays(sq) == 'Saturday') sq <- sq + days(2)
    sq <- seq(from = sq + minutes(1), sq + days(5), by = 'min')
    sets$forecast$index <- sq
    
  } else {
    sets$forecast$index <- data_m1$index[
    (which(data_m1$index == smp %>% 
             index %>% 
             xts::last()) + 1):(
     which(data_m1$index == smp %>% 
             index %>% 
             xts::last()) + 7200)]
  }
  
  if (!dir.exists(paste0('data/fx/USDJPY'))) 
    dir.create(paste0('data/fx/USDJPY'))
  
  saveRDS(sets, paste0('data/fx/USDJPY/sets.qt.7200.', 
                       as_date(sets$forecast$index[1]), '.rds'))
  
  cat(paste0(
    'data/fx/USDJPY/sets.qt.7200.', 
    as_date(sets$forecast$index[1]), '.rds saved!\n'))
  }

