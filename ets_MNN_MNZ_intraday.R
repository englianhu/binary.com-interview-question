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
knitr::opts_chunk$set(warning = FALSE, #cache = TRUE, 
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
source('function/intra_1440.R')


#########################################################################
# --------- eval=FALSE ---------
source('function/intra_min.R')

timeID <- unique(dsmp$date)
bse <- dsmp[year == 2016]$date[1] #"2016-01-04" #1st trading date in 2nd year
timeID %<>% .[. >= bse]
timeID %<>% .[. >= as_date('2016-02-26')]
timeID %<>% .[. <= as_date('2016-12-31')]
#timeID %<>% .[. >= as_date('2016-12-28')]
data_len <- 720
hrz1 <- 1
intr <- data_len/hrz1

llply(ets.m, function(md) {
  intra_min(timeID = timeID, dsmp, 
		    data_len = data_len, hrz1 = hrz1, 
            .model = md, vb = FALSE)
  })

#ts_ets_MNN_960_1.p_1281.2016-03-02
#ts_ets_MNN_840_1.p_1222.2016-03-03
#ts_ets_MNN_720_1.p_1201.2016-02-29
#ts_ets_MNN_600_1.p_267.2016-02-29

# --------- eval=FALSE ---------
source('function/intra_1440.R')

timeID <- unique(dsmp$date)
bse <- dsmp[year == 2016]$date[1] #"2016-01-04" #1st trading date in 2nd year
timeID %<>% .[. >= bse]
timeID %<>% .[1:5]
#timeID %<>% .[. >= as_date('2016-01-04')]
data_len <- 1440
hrz1 <- 480
intr <- data_len/hrz1

llply(ets.m, function(md) {
  intra_1440(timeID = timeID, dsmp, 
			 data_len = data_len, hrz1 = hrz1, 
             .model = md, vb = FALSE)
  })


# --------- eval=FALSE ---------
source('function/intra_7200.R')

timeID <- unique(dsmp$date)
bse <- dsmp[year == 2016]$date[1] #"2016-01-04" #1st trading date in 2nd year
timeID %<>% .[. >= bse]
#timeID %<>% .[. >= as_date('2016-01-04')]
data_len <- 7200
hrz1 <- 720
intr <- data_len/hrz1

llply(ets.m, function(md) {
  intra_7200(timeID = timeID, dsmp, 
			 data_len = data_len, hrz1 = hrz1, 
             .model = md)
  })

#########################################################################
## Check forecast files observations
##read files 1st methods sort by list
setfls <- llply(timeID, function(x) {
    llply(1:intr, function(y) {
        paste0('ts_ets_MNN_', data_len, '_', hrz1, '.p', y, '.', x)
    }) %>% unlist
}) %>% unlist
avfls <- paste0(.dtr, 'data/fx/USDJPY/', setfls, '.rds')[file.exists(paste0(.dtr, 'data/fx/USDJPY/', setfls, '.rds'))]
cnt <- avfls %>% ldply(., function(x) {readRDS(x)) %>% nrow}) %>% as.data.table
cnt

dtbl <- llply(avfls, function(x) {readRDS(x) %>% as.data.table})
nms <- str_replace_all(avfls, 'C:/Users/User/Documents/GitHub/binary.com-interview-question-data/data/fx/USDJPY/|.rds', '')
names(dtbl) <- nms
dtbl

##read files 2nd methods, follow files on dir which is unable sort.
#ptn <- paste0('^ts_ets_MNN_', data_len, '_', hrz1, '.p_[0-9]{0,}')
ptn <- paste0('^ts_ets_MNN_960_[0-9]{0,}.p_[0-9]{0,}')
lst <- list.files(paste0(.dtr, 'data/fx/USDJPY/intraday/'), pattern = ptn)
#lst <- matrix(lst, ncol = intr) %>% t %>% as.vector
c(head(lst), tail(lst))

cnt <- lst %>% ldply(., function(x) {readRDS(paste0(.dtr, 'data/fx/USDJPY/intraday/', x)) %>% nrow}) %>% as.data.table
cnt

dtbl <- llply(lst, function(x) {readRDS(paste0(.dtr, 'data/fx/USDJPY/intraday/', x)) %>% as.data.table})
dtbl



llply(lst, function(x) {
    old <- paste0(.dtr, 'data/fx/USDJPY/intraday/', x)
    y <- str_replace_all(x, '.p__', '.p_')
    new <- paste0(.dtr, 'data/fx/USDJPY/intraday/', y)
    file.rename(old, new)
	cat('------------------------------------\n', x, '\n changed to \n', y, '\n')
})

lst[str_detect(lst, '2017-11-01')]

#ts_ets_MNN_480_1.p_2.2017-04-25.rds
#ts_ets_MNN_360_1.p_198.2017-04-03.rds
#ts_ets_MNN_240_1.p_82.2017-01-27.rds
#ts_ets_MNN_180_1.p_295.2017-01-27.rds


#########################################################################
require('filesstrings')
require('zip')

fls1 <- list.files('C:/Users/User/Desktop/intraday')
fls2 <- list.files('C:/Users/User/Documents/GitHub/binary.com-interview-question-data/data/fx/USDJPY/intraday')
fls2 <- fls2[-1]

fls1_chk <- fls1[fls2 %in% fls1]
fls1_chk <- na.omit(fls1_chk)
attributes(fls1_chk) <- NULL
fls2_chk <- fls2[fls1 %in% fls2]

pth <- 'C:/Users/User/Documents/GitHub/binary.com-interview-question-data/data/fx/USDJPY/intraday'
fls <- paste0('C:/Users/User/Desktop/intraday/', fls1_chk)


move_files(fls, pth)

## https://stackoverflow.com/questions/37405424/what-is-the-fastest-way-to-delete-files-using-r
unlink(fls1[!fls1 %in% fls1_chk], recursive = TRUE, force = TRUE)
system(paste('del', paste0('C:/Users/User/Desktop/intraday/', fls1[!fls1 %in% fls1_chk], collapse = ' ')), wait = FALSE)

## https://stackoverflow.com/questions/47370686/how-to-zip-multiple-csv-files-in-r
zip::zipr(zipfile = 'C:/Users/User/Desktop/intraday/ts_ets.zip', files = fls)


