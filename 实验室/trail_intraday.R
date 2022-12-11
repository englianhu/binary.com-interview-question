# install.packages('remotes', dependencies = TRUE, INSTALL_opts = '--no-lock')
library('BBmisc', 'rmsfuns')
#remotes::install_github("rstudio/sass")
lib('sass')

## https://support.rstudio.com/hc/en-us/articles/200532197
## https://community.rstudio.com/t/r-does-not-display-korean-chinese/30889/3?u=englianhu
#Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
#Sys.setlocale("LC_CTYPE", "zh_CN.UTF-8")
#Sys.setlocale(category = "LC_CTYPE", "Chinese (Simplified)_China.936")
#Sys.setlocale(locale = "Chinese")
#Sys.setlocale(locale = "Japanese")
#Sys.setlocale(locale = "English")

# rmarkdown::render('/home/englianhu/Documents/owner/ryo-cn.Rmd',  encoding = 'UTF-8')
#Sys.setlocale("LC_CTYPE", "UTF-8")
#Sys.setlocale(locale = "UTF-8")
#Sys.setlocale(category = "LC_ALL", locale = "chs")
#Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
#Sys.setlocale(category = "LC_ALL", locale = "Chinese")
#Sys.setlocale(category = "LC_ALL", locale = "zh_CN.UTF-8")
Sys.setlocale("LC_ALL", "en_US.UTF-8")

## 更换时间区域，保留日期时间。
Sys.setenv(TZ = 'Asia/Tapei')

## 忽略所有警讯
## https://stackoverflow.com/a/36846793/3806250
## 设置宽度
## options(knitr.table.format = 'html')将所有kableExtra图表一致设置为'html'格式，省略设置各别图表。
options(warn = -1, width = 999, knitr.table.format = 'html', 
        digits = 16, digits.secs = Inf)

## https://stackoverflow.com/questions/39417003/long-vectors-not-supported-yet-abnor-in-rmd-but-not-in-r-script
## https://yihui.org/knitr/options
knitr::opts_chunk$set(
  class.source = 'hover01', class.output = 'hover02', class.error = 'hover03', 
  message = FALSE, warning = FALSE, error = TRUE, 
  autodep = TRUE, aniopts = 'loop', progress = TRUE, verbose = TRUE, 
  cache = FALSE, cache.lazy = FALSE, result = 'asis')

## 读取pkgs、设置编织与环境选项。
## 3210448065@qq.com
## leiou123

## 2849108450@qq.com
## leiou123
## https://rstudio.cloud/project/1198888

## 读取'BBmisc'pkgs。
if(suppressMessages(!require('BBmisc'))){
  install.packages('BBmisc', dependencies = TRUE, INSTALL_opts = '--no-lock')
  suppressMessages(library('BBmisc'))
}
if (suppressMessages(!require('rmsfuns'))) {
  install.packages('rmsfuns', dependencies = TRUE, INSTALL_opts = '--no-lock')
  suppressMessages(library('rmsfuns'))
}

if(!require('REmap')) devtools::install_github('lchiffon/REmap')

## 一次性读取所需pkgs。
library('dplyr', warn.conflicts = FALSE)
library('lubridate', warn.conflicts = FALSE)
conflict_prefer('filter', 'dplyr', quiet = TRUE)
conflict_prefer('select', 'dplyr', quiet = TRUE)
conflict_prefer('mutate', 'dplyr', quiet = TRUE)
conflict_prefer('rename', 'dplyr', quiet = TRUE)
conflict_prefer('collapse', 'dplyr', quiet = TRUE)
conflict_prefer('year', 'lubridate', quiet = TRUE)

pkgs <- c('devtools', 'knitr', 'kableExtra', 'tint', 'furrr', 'tidyr', 
          'readr', 'lubridate', 'data.table', 'reprex', 'stringr', 
          'feather', 'purrr', 'quantmod', 'tidyquant', 'tibbletime', 
          'timetk', 'plyr', 'dplyr', 'dbplyr', 'magrittr', 'sarima', 
          'tidyverse', 'memoise', 'htmltools', 'formattable', 'dtplyr', 
          'zoo', 'forecast', 'seasonal', 'seasonalview', 'rjson', 
          'rugarch', 'rmgarch', 'mfGARCH', 'sparklyr', 'jcolors', 'TSA', 
          'microbenchmark', 'dendextend', 'lhmetools', 'gtools', 
          'stringi', 'pacman', 'profmem', 'ggthemes', 'flyingfox', 
          'htmltools', 'echarts4r', 'viridis', 'hrbrthemes', 
          'fable', 'fabletools', 'Rfast', 'Metrics', 'MLmetrics')

# load_pkg(pkgs)
suppressAll(lib(pkgs))
load_pkg(pkgs)
rm(pkgs)

.path <- '/home/englianhu/Documents/GitHub/binary.com-interview-question-data/'

## 设置googleVis选项，促使plot.gvis只陈列HTML格式的完成品。
ggvisplot <- options(gvis.plot.tag = 'chart')

conflict_prefer('filter', 'dplyr', quiet = TRUE)
conflict_prefer('select', 'dplyr', quiet = TRUE)
conflict_prefer('mutate', 'dplyr', quiet = TRUE)
conflict_prefer('rename', 'dplyr', quiet = TRUE)
conflict_prefer('collapse', 'dplyr', quiet = TRUE)
conflict_prefer('year', 'lubridate', quiet = TRUE)
conflict_prefer('permutations', 'gtools', quiet = TRUE)

## 检验是否已设置路径。
if(!exists('.path')) {
  .path <- '/home/englianhu/Documents/GitHub/binary.com-interview-question-data/'}

## 倘若环境尚未有数据，读取文件数据。
if(!exists('dsmp')) {
  dsmp <- readRDS(paste0(.path, '文艺数据库/fx/USDJPY/样本1.rds'))}

source('函数/日内高频指数平滑.R')

timeID <- unique(dsmp$日期)
# baseline <- filter(dsmp, 年份 == 2016)$日期[1] #"2016-01-04" 第2年第1个交易日
baseline <- dsmp[年份 == 2016]$日期[1]
timeID %<>% .[. >= baseline]
# timeID %<>% .[. >= as_date('2016-01-04')]
index <- dsmp[日期 %chin% timeID]$序列
data_length <- 1200 #筛选数据中的最后1200观测值：dsmp[(.N - (data_length - 1)):.N]
forecast_horizon <- 1

.model = c('MNN')

# --------- eval=FALSE ---------
hft = 1200

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 600

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 400

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 300

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 240

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 200

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 150

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 120

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 100

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 80

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 60

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 50

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 40

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 30

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 24

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 20

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 16

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 15

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 12

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 10

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 8

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 6

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 5

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 4

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 3

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 2

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)

# --------- eval=FALSE ---------
hft = 1

日内高频指数平滑(
     timeID = timeID, dsmp = dsmp, data_length = data_length, hft = hft, 
     forecast_horizon = forecast_horizon, .model = .model)
