if(!suppressPackageStartupMessages(require('BBmisc'))) {
  install.packages('BBmisc', dependencies = TRUE, INSTALL_opts = '--no-lock')
}
suppressPackageStartupMessages(require('BBmisc'))
# suppressPackageStartupMessages(require('rmsfuns'))

pkgs <- c('devtools', 'knitr', 'kableExtra', 'tint', 
          'devtools','readr', 'lubridate', 'data.table', 
          'feather', 'purrr', 'quantmod', 'tidyquant', 
          'tibbletime', 'furrr', 'flyingfox', 'tidyr', 'plotly', 
          'timetk', 'plyr', 'dplyr', 'stringr', 'magrittr', 
          'tidyverse', 'memoise', 'htmltools', 'formattable', 
          'zoo', 'forecast', 'seasonal', 'seasonalview', 'dygraphs', 
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

## =======================================================================

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

## =======================================================================

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


## =======================================================================

#dir('data/fx/USDJPY', pattern = '*.z') %>% 
#  llply(., function(x) {
#    suppressAll(unzip(paste0('data/fx/USDJPY/', x)))
#  })
seasonal_m1 <- read_rds('data/fx/USDJPY/seasonal_m1.rds')

## https://stackoverflow.com/a/52490634/3806250
#seasonal_m1[is.nan(seasonal_m1)] <- NA
if(!is.data.table(seasonal_m1)) seasonal_m1 <- data.table(seasonal_m1)

seasonal_m1 <- data.table(seasonal_m1)
setorder(seasonal_m1, index)

open.accr <- seasonal_m1[, {
  open = open
  open.Point.Forecast = open.Point.Forecast
  .SD[, .(.N, open.mape = MAPE(open, open.Point.Forecast), 
          open.smape = SMAPE(open, open.Point.Forecast), 
          open.mse = MSE(open, open.Point.Forecast), 
          open.rmse = RMSE(open, open.Point.Forecast)), 
      by={index=as_date(index)}]}, 
  by=.(Model, Period)]

high.accr <- seasonal_m1[, {
  high = high
  high.Point.Forecast = high.Point.Forecast
  .SD[, .(.N, high.mape = MAPE(high, high.Point.Forecast), 
          high.smape = SMAPE(high, high.Point.Forecast), 
          high.mse = MSE(high, high.Point.Forecast), 
          high.rmse = RMSE(high, high.Point.Forecast)), 
      by={index=as_date(index)}]}, 
  by=.(Model, Period)]

low.accr <- seasonal_m1[, {
  low = low
  low.Point.Forecast = low.Point.Forecast
  .SD[, .(.N, low.mape = MAPE(low, low.Point.Forecast), 
          low.smape = SMAPE(low, low.Point.Forecast), 
          low.mse = MSE(low, low.Point.Forecast), 
          low.rmse = RMSE(low, low.Point.Forecast)), 
      by={index=as_date(index)}]}, 
  by=.(Model, Period)]

close.accr <- seasonal_m1[, {
  close = close
  close.Point.Forecast = close.Point.Forecast
  .SD[, .(.N, close.mape = MAPE(close, close.Point.Forecast), 
          close.smape = SMAPE(close, close.Point.Forecast), 
          close.mse = MSE(close, close.Point.Forecast), 
          close.rmse = RMSE(close, close.Point.Forecast)), 
      by={index=as_date(index)}]}, 
  by=.(Model, Period)]
 
## =======================================================================

## Checking models
## Due to the low precision and low accuracy, here I plot the graph and rerun the code to check the models.
yr_2018 <- data.table(seasonal_m1)[as_date(index) > as_date('2017-12-31')]

dy.qt_dy.yr_2018 <- yr_2018[Model == 'tbats' & Period %in% c('dy.qt', 'dy.yr')]


#yr_2018 <- data.table(yr_2018)
setorder(yr_2018, index)

open.accr <- yr_2018[, {
  open = open
  open.Point.Forecast = open.Point.Forecast
  .SD[, .(.N, open.mape = MAPE(open, open.Point.Forecast), 
          open.smape = SMAPE(open, open.Point.Forecast), 
          open.mse = MSE(open, open.Point.Forecast), 
          open.rmse = RMSE(open, open.Point.Forecast)), 
      by={index=as_date(index)}]}, 
  by=.(Model, Period)]

high.accr <- yr_2018[, {
  high = high
  high.Point.Forecast = high.Point.Forecast
  .SD[, .(.N, high.mape = MAPE(high, high.Point.Forecast), 
          high.smape = SMAPE(high, high.Point.Forecast), 
          high.mse = MSE(high, high.Point.Forecast), 
          high.rmse = RMSE(high, high.Point.Forecast)), 
      by={index=as_date(index)}]}, 
  by=.(Model, Period)]

low.accr <- yr_2018[, {
  low = low
  low.Point.Forecast = low.Point.Forecast
  .SD[, .(.N, low.mape = MAPE(low, low.Point.Forecast), 
          low.smape = SMAPE(low, low.Point.Forecast), 
          low.mse = MSE(low, low.Point.Forecast), 
          low.rmse = RMSE(low, low.Point.Forecast)), 
      by={index=as_date(index)}]}, 
  by=.(Model, Period)]

close.accr <- yr_2018[, {
  close = close
  close.Point.Forecast = close.Point.Forecast
  .SD[, .(.N, close.mape = MAPE(close, close.Point.Forecast), 
          close.smape = SMAPE(close, close.Point.Forecast), 
          close.mse = MSE(close, close.Point.Forecast), 
          close.rmse = RMSE(close, close.Point.Forecast)), 
      by={index=as_date(index)}]}, 
  by=.(Model, Period)]

## check the imprecision and bias
hist(open.accr$open.mape)
hist(high.accr$high.smape)
hist(low.accr$low.mse)
hist(close.accr$close.rmse)


open.accr[open.mape >= 0.01]

fig <- plot_ly(x = ~unique(open.accr$index))
fig <- fig %>% 
	add_lines(y = ~open.accr[Period == 'dy.yr']$open.mape, 
	name = 'tbats_dy.yr', line = list(shape = 'linear'))
fig <- fig %>% 
	add_lines(y = ~open.accr[Period == 'dy.wk.mo']$open.mape, 
	name = 'tbats_dy.wk.mo', line = list(shape = 'linear'))
fig <- fig %>% 
	add_lines(y = ~open.accr[Period == 'dy.wk']$open.mape, 
	name = 'tbats_dy.wk', line = list(shape = 'linear'))
fig <- fig %>% 
	add_lines(y = ~open.accr[Period == 'mo.1440']$open.mape, 
	name = 'ts_mo.1440', line = list(shape = 'linear'))
fig <- fig %>% 
	add_lines(y = ~open.accr[Period == 'qt.1440']$open.mape, 
	name = 'ts_qt.1440', line = list(shape = 'linear'))
fig <- fig %>% 
	add_lines(y = ~open.accr[Period == 'wk.1440']$open.mape, 
	name = 'ts_wk.1440', line = list(shape = 'linear'))
fig <- fig %>% 
	add_lines(y = ~open.accr[Period == 'wk.7200']$open.mape, 
	name = 'ts_wk.7200', line = list(shape = 'linear'))
fig <- fig %>% 
	add_lines(y = ~open.accr[Period == 'yr.1440']$open.mape, 
	name = 'ts_yr.1440', line = list(shape = 'linear'))
fig

## ----- start error, not run... ----- 
dens <- with(yr_2018, tapply(open.Point.Forecast, INDEX = Period, density))
df <- data.frame(
  index = unlist(lapply(dens, '[[', 'index')),
  open.price = unlist(lapply(dens, '[[', 'open.Point.Forecast')),
  cut = rep(names(dens), each = length(dens[[1]]$index)))

fig <- plot_ly(df, x = ~index, y = ~open.price, color = ~cut) 
fig <- fig %>% add_lines()

fig
## ----- end error, not run... ----- 

# ---------------------------------

dct <- yr_2018 %>% 
	mutate(
	open.mape = MAPE(open, open.Point.Forecast), 
	open.smape = SMAPE(open, open.Point.Forecast), 
	open.mse = MSE(open, open.Point.Forecast), 
	open.rmse = RMSE(open, open.Point.Forecast), 
	
	high.mape = MAPE(high, high.Point.Forecast), 
	high.smape = SMAPE(high, high.Point.Forecast), 
	high.mse = MSE(high, high.Point.Forecast), 
	high.rmse = RMSE(high, high.Point.Forecast), 
	
	low.mape = MAPE(low, low.Point.Forecast), 
	low.smape = SMAPE(low, low.Point.Forecast), 
	low.mse = MSE(low, low.Point.Forecast), 
	low.rmse = RMSE(low, low.Point.Forecast), 
	
	close.mape = MAPE(close, close.Point.Forecast), 
	close.smape = SMAPE(close, close.Point.Forecast), 
	close.mse = MSE(close, close.Point.Forecast), 
	close.rmse = RMSE(close, close.Point.Forecast))


dct_dy.qt_dy <- dy.qt_dy.yr_2018 %>% 
	mutate(
	open.mape = MAPE(open, open.Point.Forecast), 
	open.smape = SMAPE(open, open.Point.Forecast), 
	open.mse = MSE(open, open.Point.Forecast), 
	open.rmse = RMSE(open, open.Point.Forecast), 
	
	high.mape = MAPE(high, high.Point.Forecast), 
	high.smape = SMAPE(high, high.Point.Forecast), 
	high.mse = MSE(high, high.Point.Forecast), 
	high.rmse = RMSE(high, high.Point.Forecast), 
	
	low.mape = MAPE(low, low.Point.Forecast), 
	low.smape = SMAPE(low, low.Point.Forecast), 
	low.mse = MSE(low, low.Point.Forecast), 
	low.rmse = RMSE(low, low.Point.Forecast), 
	
	close.mape = MAPE(close, close.Point.Forecast), 
	close.smape = SMAPE(close, close.Point.Forecast), 
	close.mse = MSE(close, close.Point.Forecast), 
	close.rmse = RMSE(close, close.Point.Forecast))

## =======================================================================
# grph <- seasonal_m1 %>% 
grph <- yr_2018 %>% 
  tidyr::unite(Model, Model:Period) %>% 
  data.table
prc <- unique(grph[, .(index, open, high, low, close)])
prc <- prc[, Model := 'Market.Price'][]
grph <- grph[, (c('open', 'high', 'low', 'close')) := NULL]
names(grph) <- c('index', 'Model', 'open', 'high', 'low', 'close')
grph <- rbind(grph, prc)
rm(prc)

tb5 <- grph %>% data.table
tb5

####
p_tb5_op <- tb5 %>% 
    group_by(Model) %>% 
    e_charts(x = index) %>% 
    e_line(open, smooth = TRUE) %>% 
  e_datazoom(
    type = 'slider', 
    toolbox = FALSE,
    bottom = -5) %>% 
  e_tooltip() %>% 
  e_title(text = 'Model', subtext = 'open', left = 'center') %>% 
  e_axis_labels(x = 'index', y = 'open') %>%
  e_x_axis(index, axisPointer = list(show = TRUE)) %>% 
  e_legend(
    orient = 'vertical', 
    type = c('scroll'), 
    #selectedMode = 'multiple', #https://echarts.apache.org/en/option.html#legend
    #selected = list('Model'), 
    left = 0, top = 80) %>% 
  e_grid(left = 150, top = 90) %>% 
  #e_theme('shine') %>% 
  e_toolbox_feature('saveAsImage', title = 'Screenshot')
p_tb5_op
# C:\Users\User\AppData\Local\Temp\RtmpOOeSsG\viewhtml40e8290451f\index.html


fig <- plot_ly(tb5, x = ~index, y = ~open, color = ~Model) 
fig <- fig %>% add_lines()
fig



