if(!require('BBmisc')) install.packages('BBmisc')
suppressPackageStartupMessages(require(BBmisc))
pkgs <- c('data.table', 'plyr', 'dplyr', 'magrittr', 'purrr', 
          'tidyr', 'stringr', 'lubridate', 'R.utils')

suppressAll(lib(pkgs))
rm(pkgs)

## get currency dataset online.
yr <- c(2015, 2016, 2017, 2018)
wk <- 1:53
dr <- 'data/USDJPY/'

## https://www.epochconverter.com/years
llply(yr, function(i) {
  if(i == 2015|i == 2017) wk <- 1:53 else wk <- 1:52
  llply(wk, function(j) {
    lnk <- paste0(
      'https://tickdata.fxcorporate.com/USDJPY/', i, '/', j, '.csv.gz')
    if(!dir.exists(dr)) dir.create(dr)
    if(!file.exists(paste0(dr, 'Y', i, 'W', j, '.csv.gz'))) {
      download.file(lnk, destfile = paste0(
        dr, 'Y', i, 'W', j, '.csv.gz'))
      #cat(paste0(dr, 'Y', i, 'W', j, '.csv.gz downloaded!\n'))
    }
  })
})

llply(yr, function(i) {
  if(i == 2015|i == 2017) wk <- 1:53 else wk <- 1:52
  llply(wk, function(j) {
    if(file.exists(paste0(dr, 'Y', i, 'W', j, '.csv.gz')) & 
       !file.exists(paste0(dr, 'Y', i, 'W', j, '.csv'))) {
      R.utils::gunzip(paste0(dr, 'Y', i, 'W', j, '.csv.gz'), 
                      remove = FALSE)
      cat(paste0(dr, 'Y', i, 'W', j, '.csv.gz extracted!\n'))
    }
  })
})

## remove all files size less than 1MB
if(any(file.exists(paste0(dr, dir(dr, pattern = '.csv')[file.size(paste0(
  dr, dir(dr, pattern = '.csv'))) <= 1000000])))) {
  file.remove(paste0(dr, dir(dr, pattern = '.csv')[file.size(
    paste0(dr, dir(dr, pattern = '.csv'))) <= 1000000]))
}



