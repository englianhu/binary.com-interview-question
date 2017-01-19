## =============== draft 2 ==============================
# using moving 365 days dataset and rerun the compStocks.
suppressPackageStartupMessages(library("BBmisc"))
suppressAll(library('devtools'))
suppressAll(library('lubridate'))
suppressAll(library('plyr'))
suppressAll(library('stringr'))
suppressAll(library('magrittr'))
suppressAll(library('dplyr'))
suppressAll(library('tidyr'))
suppressAll(library('readr'))
suppressAll(library('tidyverse')) #load c(dplyr, tidyr, stringr, readr) due to system doesn't work.
suppressAll(library('tidyquant'))
suppressAll(library("shiny"))
suppressAll(library("shinyjs"))
suppressAll(library('shinyBS'))
suppressAll(library('DT'))
suppressAll(library('quantmod'))
suppressAll(library('formattable'))
suppressAll(library('highcharter'))
suppressAll(library('PerformanceAnalytics'))
suppressAll(library('memoise'))
suppressAll(source('./function/loadLAD.R'))
suppressAll(source('./function/filterLAD.R'))
suppressAll(source('./function/compStocks.R'))
suppressAll(source('./function/plotChart2.R'))

## ========= Read Data =================================
eval(parse(text = paste0('datam = loadLAD(); LAD = datam$LAD; LADDT = datam$LADDT; rm(datam)')))

## Testing 365 days data.
gsfit <- compStocks(LADDT, family = 'gaussian', xy.matrix = 'h2', maxit = 100000, 
                    yv = c('daily.mean1', 'daily.mean2', 'daily.mean3', 'mixed1', 
                           'mixed2', 'mixed3'), pred.type = 'class', .print = TRUE)
saveRDS(gsfit, file = './data/gsfit.rds')

mse1 <- ldply(gsfit$fit, function(x) x$mse) %>% tbl_df %>% filter(mse == min(mse))
saveRDS(mse1, file = './data/mse1.rds')

name514gs <- unique(mse1$.id)
tmpsumgs <- ldply(gsfit$fit[name514gs], function(x) x$mse) %>% tbl_df
saveRDS(tmpsumgs, file = './data/tmpsumgs.rds')

tmpgsfit <- gsfit$fit[name514gs]
saveRDS(tmpgsfit, file = './data/tmpgsfit.rds')

tmpgsform <- gsfit$formula1[str_replace_all(name514gs, 'fitgaum', '') %>% as.numeric]
saveRDS(tmpgsform, file = './data/tmpgsform.rds')

fitgaum16.alpha08 <- tmpgsfit$fitgaum16$fit[[filter(mse1, .id == 'fitgaum16')$model %>% 
                                               str_replace_all('mse', '') %>% as.numeric]]
fitgaum16.alpha08 <- read_rds(path = './data/fitgaum16.alpha08.rds')


## predict() and forecast()
## https://stat.ethz.ch/R-manual/R-devel/library/stats/html/predict.arima.html
## http://www.dummies.com/programming/r/how-to-predict-new-data-values-with-r/



