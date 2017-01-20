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


## http://stats.stackexchange.com/questions/223872/forecasting-arima-with-predict-vs-forecast-in-r
#They will give you the same answers. But the combination of Arima (not arima) and forecast from the forecast package are enhanced versions with additional functionality.
#Arima calls stats::arima for the estimation, but stores more information in the returned object. It also allows some additional model functionality such as including a drift term in a model with a unit root.
#forecast calls stats::predict to generate the forecasts. It will automatically handle the drift term from Arima. It returns a forecast object (rather than a simple list) which is useful for plotting, displaying, summarizing and analysing the results.

## http://stackoverflow.com/questions/31409591/difference-between-forecast-and-predict-function-in-r

#> #load training data
#> trnData = read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
#> 
#> model <- lm(frequency ~ attitude + scenario, trnData)
#> 
#> #create test data
#> tstData <- t(cbind(c("H1", "H", 2, "pol", 185),
#                       +                    c("M1", "M", 1, "pol", 115),
#                       +                    c("M1", "M", 1, "inf", 118),
#                       +                    c("F1", "F", 3, "inf", 210)))
#> 
#> tstData <- data.frame(tstData,stringsAsFactors = F)
#> colnames(tstData) <- colnames(trnData)
#> tstData[,3]=as.numeric(tstData[,3])
#> tstData[,5]=as.numeric(tstData[,5])
#> 
#> cbind(Obs=tstData$frequency,pred=predict(model,newdata=tstData))
#Obs     pred
#1 185 187.4289
#2 115 189.0037
#3 118 207.3126
#4 210 204.1629
#> 
#> #forecast
#> x <- read.table(text='day       sum
#                    +                 2015-03-04   44           
#                    +                 2015-03-05   46           
#                    +                 2015-03-06   48           
#                    +                 2015-03-07   48           
#                    +                 2015-03-08   58           
#                    +                 2015-03-09   58           
#                    +                 2015-03-10   66           
#                    +                 2015-03-11   68           
#                    +                 2015-03-12   85           
#                    +                 2015-03-13   94           
#                    +                 2015-03-14   98           
#                    +                 2015-03-15  102           
#                    +                 2015-03-16  102           
#                    +                 2015-03-17  104           
#                    +                 2015-03-18  114', header=TRUE, stringsAsFactors=FALSE)
#> library(xts)
#> dates=as.Date(x$day,"%Y-%m-%d")
#> xs=xts(x$sum,dates)
#> 
#> library("forecast")
#> fit <- ets(xs)
#> plot(forecast(fit))
#> forecast(fit, h=4)
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#16       118.4725 112.4748 124.4702 109.2998 127.6452
#17       123.4706 115.4583 131.4828 111.2169 135.7242
#18       128.4686 118.8548 138.0824 113.7656 143.1717
#19       133.4667 122.4821 144.4513 116.6672 150.2662
