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
#'@ suppressAll(library('tidyquant'))
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
#                     c("M1", "M", 1, "pol", 115),
#                     c("M1", "M", 1, "inf", 118),
#                     c("F1", "F", 3, "inf", 210)))
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


## =================================== Basic Model ===========================================
## Draft simulate the function while I added-on some-more criterias and selections.
## Simulate 365 days dataset with Markov Chain.
##   using moving 365 days dataset and rerun the compStocks.
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
#'@ suppressAll(library('tidyquant'))
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

## load once and saved.
#'@ getSymbols('LAD')
#'@ saveRDS(LAD, file = './data/LAD_full.rds')
LAD <- read_rds(path = './data/LAD_full.rds')
LADDT <- LAD %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
  tbl_df %>% mutate(Date = ymd(Date)) %>% arrange(Date)
#mutate(LAD.Volume = formattable::digits(
#       LAD.Volume, 0, format = 'd', big.mark = ','))

dateID0 <- ymd('2015-01-01')
LADDT %<>% filter(Date >= (dateID0 - years(1))) #dateID == ymd('2016-02-29') has error and will need to review the coding.
dateID <- unique(LADDT$Date)
dateID <- dateID[dateID >= dateID0]
#'@ dir.create(paste0('./data/', str_replace_all(dateID, '-', '')))

## list the cv.glmnet models.
families <- c('gaussian', 'binomial', 'poisson', 'multinomial', 'cox', 'mgaussian', 'all')

bsGSfit <- llply(dateID, function(dt) {
  
  ## create a folder to save all models.
  pth = paste0('./data/', str_replace_all(dt, '-', ''))
  if(!dir.exists(pth)) dir.create(pth)
  
  ## predict dateID onwards from data < dateID
  ##   for example : in order to predict today price, I used the data from 
  ##   1 years (365 days or 366 days for leap years.) ago from yesterday.
  #'@ ymd("2016-2-29") %m-% years(1)
  ## http://stackoverflow.com/questions/8490799/how-to-account-for-leap-years
  
  smp = filter(LADDT, Date < dt & Date >= (dt %m-% years(1)))
  gsfit = compStocks(smp, family = families[1], xy.matrix = 'h2', yv.lm = c(TRUE, FALSE), 
                     yv = c('open1', 'open2', 'high1', 'high2', 'low1', 'low2', 'close1', 'close2', 'daily.mean1', 'daily.mean2', 'daily.mean3', 'mixed1', 'mixed2', 'mixed3'), 
                     pred.type = 'class', .print = TRUE, .save = TRUE, pth = pth)
  
  ## basic model weight is FALSE
  weight.date = FALSE
  weight.volume = FALSE
  
  ## save basic or weight models.
  if(weight.date != FALSE | weight.volume != FALSE) {
    wtfitgaum = gsfit; rm(gsfit)
    
    ## generates 224 models
    saveRDS(wtfitgaum, file = paste0(pth, '/wtfitgaum.rds'))
    
    ## saved best mean-squared error comparison.
    wtfitgaum.mse1 = ldply(wtfitgaum$fit, function(x) x$mse) %>% tbl_df %>% 
      filter(mse == min(mse))
    saveRDS(wtfitgaum.mse1, file = paste0(pth, '/wtfitgaum.mse1.rds'))
    
    ## saved summary of all best models. (if more than 1)
    name514gs = unique(wtfitgaum.mse1$.id)
    wtfitgaum.sum = ldply(wtfitgaum$fit[name514gs], function(x) x$mse) %>% tbl_df
    saveRDS(wtfitgaum.sum, file = paste0(pth, '/wtfitgaum.sum.rds'))
    
    ## saved best model.
    wtfitgaum.best = wtfitgaum$fit[name514gs]
    saveRDS(wtfitgaum.best, file = paste0(pth, '/wtfitgaum.best.rds'))
    
    ## saved best model's formula.
    wtfitgaum.form = wtfitgaum$formula1[str_replace_all(
      name514gs, 'wtfitgaum', '') %>% as.numeric]
    saveRDS(wtfitgaum.form, file = paste0(pth, '/wtfitgaum.form.rds'))
    
  } else {
    fitgaum = gsfit; rm(gsfit)
    
    ## generates 224 models
    saveRDS(fitgaum, file = paste0(pth, '/fitgaum.rds'))
    
    ## saved best mean-squared error comparison.
    fitgaum.mse1 = ldply(fitgaum$fit, function(x) x$mse) %>% tbl_df %>% 
      filter(mse == min(mse))
    saveRDS(fitgaum.mse1, file = paste0(pth, '/fitgaum.mse1.rds'))
    
    ## saved summary of all best models. (if more than 1)
    name514gs = unique(fitgaum.mse1$.id)
    fitgaum.sum = ldply(fitgaum$fit[name514gs], function(x) x$mse) %>% tbl_df
    saveRDS(fitgaum.sum, file = paste0(pth, '/fitgaum.sum.rds'))
    
    ## saved best model.
    fitgaum.best = fitgaum$fit[name514gs]
    saveRDS(fitgaum.best, file = paste0(pth, '/fitgaum.best.rds'))
    
    ## saved best model's formula.
    fitgaum.form = fitgaum$formula1[str_replace_all(
      name514gs, 'fitgaum', '') %>% as.numeric]
    saveRDS(fitgaum.form, file = paste0(pth, '/fitgaum.form.rds'))
  }
})

## ================================= Save Basic Model ========================================
## Arrange the best fit and save the model summary.
files <- list.files('./data', pattern = '[0-9]{8}')
#'@ pth <- paste0('./data/', files, '/fitgaum.mse1.rds')

## list all daily minimum standard error models.
pre.mse1 <- ldply(files, function(x) {
  y = read_rds(path = paste0('./data/', x, '/fitgaum.mse1.rds'))
  y %>% data.frame(Date = x, .) %>% tbl_df %>% filter(mse == min(mse))
}) %>% tbl_df

## filter and only get first unique element since the mse is same.
#'@ unique(pre.mse1[c('Date', 'model', 'mse')])
pre.mse1 <- pre.mse1[!duplicated(pre.mse1$Date, pre.mse1$mse), ]
saveRDS(pre.mse1, file = './data/pre.mse1.rds')
#> pre.mse1
# A tibble: 517 × 4
#       Date        .id  model       mse
#     <fctr>      <chr> <fctr>     <dbl>
#1  20150102 fitgaum119   mse9 0.1209190
#2  20150105 fitgaum135   mse9 0.1211786
#3  20150106 fitgaum132   mse3 0.1210193
#4  20150107  fitgaum81   mse8 0.1221952
#5  20150108 fitgaum144   mse3 0.1220447
#6  20150109  fitgaum27   mse3 0.1210785
#7  20150112 fitgaum129   mse3 0.1211022
#8  20150113  fitgaum17   mse3 0.1214222
#9  20150114  fitgaum28   mse3 0.1215031
#10 20150115  fitgaum18   mse3 0.1208796

## list all daily minimum standard error models' formula.
pre.gsform <- ldply(files, function(x) {
  y = read_rds(path = paste0('./data/', x, '/fitgaum.form.rds'))
  z = paste0(substr(x, 1, 4), '-', substr(x, 5, 6), '-', substring(x, nchar(x) - 1)) %>% 
    ymd 
  data.frame(Date = z, form = y) %>% tbl_df
}) %>% tbl_df

## filter and only get first unique element since the mse is same.
pre.gsform <- pre.gsform[!duplicated(pre.gsform$Date), ]
saveRDS(pre.gsform, file = './data/pre.gsform.rds')

## ================================= Weighted Model ==========================================
## Application bayesian as weighted function and MCMC for prediction.

## Read summary of the best fit model
## From below table, we noted that the dynamic model required compare 
##   to using constant model across the days as we can know from column `.id`.
pre.mse1 <- read_rds(path = './data/pre.mse1.rds')
#> pre.mse1
# A tibble: 517 × 4
#       Date        .id  model       mse
#     <fctr>      <chr> <fctr>     <dbl>
#1  20150102 fitgaum119   mse9 0.1209190
#2  20150105 fitgaum135   mse9 0.1211786
#3  20150106 fitgaum132   mse3 0.1210193
#4  20150107  fitgaum81   mse8 0.1221952
#5  20150108 fitgaum144   mse3 0.1220447
#6  20150109  fitgaum27   mse3 0.1210785
#7  20150112 fitgaum129   mse3 0.1211022
#8  20150113  fitgaum17   mse3 0.1214222
#9  20150114  fitgaum28   mse3 0.1215031
#10 20150115  fitgaum18   mse3 0.1208796

## Read best fit model.
pre.gsform <- read_rds(path = './data/pre.gsform.rds')

## ============================== Save Weighted Model ========================================
## Arrange the best fit and save the data.
## 
#'@ llply(seq(nrow(pre.mse1)), function(i) {
#'@   y = read_rds(path = paste0('./data/', pre.mse1$Date[i], '/', pre.mse1$.id[i], '.rds'))
#'@   j = filter(y$mse, mse == pre.mse1$mse[i]) %>% .$model %>% 
#'@     str_replace_all('mse', '') %>% as.numeric
#'@   y$yhat[j] %>% unlist
#'@   })

testW <- suppressAll(ldply(seq(nrow(pre.mse1)), function(i) {
  y = read_rds(path = paste0('./data/', pre.mse1$Date[i], '/', pre.mse1$.id[i], '.rds'))
  j = filter(y$mse, mse == pre.mse1$mse[i]) %>% .$model %>% str_replace_all('mse', '') %>% 
    as.numeric
  z = y$yhat[j] %>% unlist %>% mean
  data.frame(Date = pre.mse1$Date[i], meanPrice = z) %>% tbl_df
})) %>% tbl_df

## ----------------------- Start Correction Section --------------------------------
## Check and notice NA value occured, the mse doesn't match.
##   The 'fitgaum.rds' file under below 'Date' different with the seperated fiiles.
# 
#> filter(testW, is.na(meanPrice))
# A tibble: 9 × 2
#       Date meanPrice
#     <fctr>     <dbl>
# 1 20150114        NA
# 2 20150217        NA
# 3 20150218        NA
# 4 20150219        NA
# 5 20150220        NA
# 6 20150223        NA
# 7 20150224        NA
# 8 20150615        NA
# 9 20151106        NA
# 
#'@ > read_rds(path = './data/20150114/fitgaum28.rds')$mse
# A tibble: 11 × 2
#      model       mse
#*    <fctr>     <dbl>
#  1    mse0 0.5900867
#  2    mse1 0.1255345
#  3    mse2 0.1296575
#  4    mse3 0.1332039 #20150114 fitgaum28   mse3 0.1215031
#  5    mse4 0.1263277
#  6    mse5 0.1254592
#  7    mse6 0.1287976
#  8    mse7 0.1290183
#  9    mse8 0.1334721
#  10   mse9 0.1390591
#  11  mse10 0.1238789
# 
#> pre.mse1[9, ]
# A tibble: 1 × 4
#       Date       .id  model       mse
#     <fctr>     <chr> <fctr>     <dbl>
# 1 20150114 fitgaum28   mse3 0.1215031
#
#> ldply(1:224, function(i) {
#     ddt = read_rds(path = paste0('./data/20150114/fitgaum', i, '.rds'))$mse %>% 
#        filter(mse == min(mse))
#     data.frame(No = i, ddt) %>% tbl_df
# }) %>% tbl_df %>% filter(mse == min(mse))
# A tibble: 2 × 3
#      No  model       mse
#   <int> <fctr>     <dbl>
# 1    18   mse3 0.1215031
# 2    23   mse3 0.1215031
# 
## For correction, due to the seperated saved files are not tally with the single file 
##   'fitgaum.rds' which list all 224 models. The NA value occured. I can either apply 
##   assign() to 'fitgaum.rds' to split and overwrite the existing independent 224 files 
##   or use the seperated 224 models to correct the best fit model.
## 
## Here I keep the 'fitgaum.rds' but correct the 'fitgaum.best.rds', 'fitgaum.form.rds', 
##   'fitgaum.mse1.rds' and 'fitgaum.sum.rds' due to the compStocks() will saved once 
##  calculated prior to completed whole 224 models.
## 
##
## Correction but keep 'fitgaum.rds'
dateID <- filter(testW, is.na(meanPrice))$Date %>% as.character

llply(dateID, function(x) {
  ## mean squared error
  fitgaum.mse1 <- ldply(1:224, function(i) {
    ddt = read_rds(path = paste0('./data/', x, '/fitgaum', i, '.rds'))$mse %>% 
      filter(mse == min(mse))
    data.frame(.id = paste0('fitgaum', i), ddt) %>% tbl_df
  }) %>% tbl_df %>% filter(mse == min(mse))
  saveRDS(fitgaum.mse1, file = paste0('./data/', x, '/fitgaum.mse1.rds'))
  cat(x, ': "fitgaum.mse1.rds" file saved.\n')
  
  ## summary
  fitgaum.sum <- ldply(fitgaum.mse1$.id, function(y) {
    ddt = read_rds(path = paste0('./data/', x, '/', y, '.rds'))$mse
  }) %>% tbl_df
  saveRDS(fitgaum.sum, file = paste0('./data/', x, '/fitgaum.sum.rds'))
  cat(x, ': "fitgaum.sum.rds" file saved.\n')
  
  ## best fit model
  fitgaum.best <- llply(fitgaum.mse1$.id, function(y) {
    read_rds(path = paste0('./data/', x, '/', y, '.rds'))
  })
  names(fitgaum.best) <- paste0('fitgaum', fitgaum.mse1$.id)
  saveRDS(fitgaum.best, file = paste0('./data/', x, '/fitgaum.best.rds'))
  cat(x, ': "fitgaum.best.rds" file saved.\n')
  
  ## best fit model's formula
  #'@ namegs <- unique(fitgaum.mse1$.id) %>% as.character
  nm <- unique(fitgaum.mse1$.id) %>% as.character %>% 
    str_replace_all('fitgaum', '') %>% as.numeric
  
  fitgaum.form <- read_rds(path = paste0('./data/', x, '/fitgaum.rds'))$formula1[nm]
  saveRDS(fitgaum.form, file = paste0('./data/', x, '/fitgaum.form.rds'))
  rm(nm)
  cat(x, ': "fitgaum.form.rds" file saved.\n')
  cat(x, ': completed.\n')
})
rm(dateID)
## ----------------------- End Correction Section --------------------------------

## rerun "Save Basic Model" and "Weighted Model" with corrected files.
testW <- suppressAll(ldply(seq(nrow(pre.mse1)), function(i) {
  y = read_rds(path = paste0('./data/', pre.mse1$Date[i], '/', pre.mse1$.id[i], '.rds'))
  j = filter(y$mse, mse == pre.mse1$mse[i]) %>% .$model %>% str_replace_all('mse', '') %>% 
    as.numeric
  z = y$yhat[j] %>% unlist %>% last
  data.frame(Date = pre.mse1$Date[i], meanPrice = z) %>% tbl_df
})) %>% tbl_df

## Draft simulate the function while I added-on some-more criterias and selections.
## Simulate 365 days dataset with Markov Chain.
##   using moving 365 days dataset and rerun the compStocks.
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
#'@ suppressAll(library('tidyquant'))
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

## load once and saved.
#'@ getSymbols('LAD')
#'@ saveRDS(LAD, file = './data/LAD_full.rds')
LAD <- read_rds(path = './data/LAD_full.rds')
LADDT <- LAD %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
  tbl_df %>% mutate(Date = ymd(Date)) %>% arrange(Date)
#mutate(LAD.Volume = formattable::digits(
#       LAD.Volume, 0, format = 'd', big.mark = ','))

dateID0 <- ymd('2015-01-01')
LADDT %<>% filter(Date >= (dateID0 - years(1))) #dateID == ymd('2016-02-29') has error and will need to review the coding.
dateID <- unique(LADDT$Date)
dateID <- dateID[dateID >= dateID0]
#'@ dir.create(paste0('./data/', str_replace_all(dateID, '-', '')))

smp = filter(LADDT, Date <= dateID0)

glmPrice(smp, family = 'gaussian', xy.matrix = 'h2', setform = 'l1', 
         yv = 'daily.mean2', yv.lm = TRUE, logistic.yv = TRUE, tmeasure = 'deviance', 
         tmultinomial = 'grouped', maxit = 100000, 
         alpha = 0:10, nfolds = 10, foldid = NULL, s = 'lambda.min', 
         weight.date = FALSE, weight.volume = FALSE, wt.control = FALSE, 
         newx = NULL, pred.type = 'class', parallel = TRUE, .log = FALSE)



llply(dateID, function(x) {
  as.numeric(difftime(x, LADDT$Date[LADDT$Date < x], units = 'days'))
  })
as.numeric(difftime(dateID0, LADDT$Date[LADDT$Date < dateID], units = 'days'))^2

## =============================================================
## Application bayesian as weighted function and MCMC for prediction.

## Read summary of the best fit model
## From below table, we noted that the dynamic model required compare 
##   to using constant model across the days as we can know from column `.id`.

## rerun "Save Basic Model" and "Weighted Model" with corrected files.
#'@ testW <- suppressAll(ldply(seq(nrow(pre.mse1)), function(i) {
#'@   y = read_rds(path = paste0('./data/', pre.mse1$Date[i], '/', pre.mse1$.id[i], '.rds'))
#'@   j = filter(y$mse, mse == pre.mse1$mse[i]) %>% .$model %>% str_replace_all('mse', '') %>% 
#'@     as.numeric
#'@   z = y$yhat[j] %>% unlist %>% mean
#'@   data.frame(Date = pre.mse1$Date[i], meanPrice = z) %>% tbl_df
#'@   })) %>% tbl_df

#'@ folder <- list.files('./data', pattern = '[0-9]{8}')
#'@ files <- list.files(paste0('./data/', folder), pattern = 'fitgaum+[0-9]{1,}.rds$')

#'@ llply(folder, function(x) {
#'@   llply(files, function(y) {
#'@     read_rds(path = paste0('./data/', x, '/', y))
#'@     })
#'@   })

#'@ test <- llply(folder[1], function(x) {
#'@   ldply(files[1], function(y) {
#'@     y = read_rds(path = paste0('./data/', x, '/', y))
#'@     z = paste0(substr(x, 1, 4), '-', substr(x, 5, 6), '-', 
#'@                substring(x, nchar(x) - 1)) %>% ymd
#'@     yy = filter(LADDT, Date < z & Date >= (z %m-% years(1)))
#'@     all.yhat = data.frame(Date = yy$Date, 
#'@                Y = yy[grep('.Close', names(yy), value = TRUE)] %>% 
#'@                  unlist, y$yhat) %>% tbl_df
#'@     saveRDS(all.yhat, file = paste0('./data/', x, '/yhat', y))
#'@     })
#'@   })

#'@ wt <- ldply(files, function(x) {
#'@   y = read_rds(path = paste0('./data/', x, '/', x))
#'@   y %>% data.frame(Date = x, .) %>% tbl_df %>% filter(mse == min(mse))
#'@ }) %>% tbl_df
suppressAll(source('./function/simulateWT.R'))
simulateWT(mbase = LADDT, .print = TRUE)

## read_rds(path = './data/20160601/wt.fitgaum176.rds') %>% data.frame
## read_rds(path = './data/20160601/wt.fitgaum177.rds') %>% data.frame
## read_rds(path = './data/20170103/wt.fitgaum176.rds') %>% data.frame
## read_rds(path = './data/20170103/wt.fitgaum177.rds') %>% data.frame

## temporary function for comparison among 224 models.
#'@ if(fordate) {
#'@   folder <- list.files('./data', pattern = '[0-9]{8}')
#'@ } else {
#'@   
#'@ } else {
#'@   
#'@ }
#'@ 
#'@ weight.volume = grep('P[0-9]', names(testwt), value = TRUE)
#'@ 
#'@ ldply(seq(224), function(x) {
#'@     read_rds(path = paste0('./data/', folder[1], '/wt.fitgaum', x, '.rds'))
#'@   }) %>% tbl_df

wtGSfit <- llply(dateID, function(dt) {
  
  ## create a folder to save all models.
  pth = paste0('./data/', str_replace_all(dt, '-', ''))
  if(!dir.exists(pth)) dir.create(pth)
  
  ## predict dateID onwards from data < dateID
  ##   for example : in order to predict today price, I used the data from 
  ##   1 years (365 days or 366 days for leap years.) ago from yesterday.
  #'@ ymd("2016-2-29") %m-% years(1)
  ## http://stackoverflow.com/questions/8490799/how-to-account-for-leap-years
  
  smp = filter(LADDT, Date < dt & Date >= (dt %m-% years(1)))
  fld = str_replace_all(dt, '-', '')
  fl = ldply(seq(224), function(x) {
    read_rds(path = paste0('./data/', fld, '/wt.fitgaum', x, '.rds'))
  }) %>% tbl_df
  #'@ files <- list.files(paste0('./data/', fld), pattern = 'wt.fitgaum+[0-9]{1,}.rds$')
  #'@ paste0(substr(x, 1, 4), '-', substr(x, 5, 6), '-', 
  #'@       substring(x, nchar(x) - 1)) %>% ymd
  
  ## sqrt to emphasized the recent stock price more heavily. (based on the decay rate to lighten
  ##   previous stock price effects.)
  wtdt = exp(-log(as.numeric(difftime(dt, smp$Date))^2))
  ## I temporary to use `weight.volume` as the weight function on stock price to avoid 
  ##   troublesome to modify existing function. However the weight function doesn't take 
  ##   the effect of trade volume.
  wtpc = fl[, grep('P[0-9]', names(fl), value = TRUE)]
  
  #`preset.weight = TRUE` inside compStocks() is the temporarily set argument for testing 224 models.
  gsfit = compStocks(smp, family = families[1], xy.matrix = 'h2', yv.lm = c(TRUE, FALSE), 
                     weight.date = wtdt, weight.volume = wtpc, fordate = dt, preset.weight = TRUE, 
                     yv = c('open1', 'open2', 'high1', 'high2', 'low1', 'low2', 'close1', 'close2', 'daily.mean1', 'daily.mean2', 'daily.mean3', 'mixed1', 'mixed2', 'mixed3'), 
                     pred.type = 'class', .print = TRUE, .save = TRUE, pth = pth)
  
  ## basic model weight is FALSE
  weight.date != FALSE
  weight.volume != FALSE
  
  ## save basic or weight models.
  if(weight.date != FALSE | weight.volume != FALSE) {
    wtfitgaum = gsfit; rm(gsfit)
    
    ## generates 224 models
    saveRDS(wtfitgaum, file = paste0(pth, '/wtfitgaum.rds'))
    
    ## saved best mean-squared error comparison.
    wtfitgaum.mse1 = ldply(wtfitgaum$fit, function(x) x$mse) %>% tbl_df %>% filter(mse == min(mse))
    saveRDS(wtfitgaum.mse1, file = paste0(pth, '/wtfitgaum.mse1.rds'))
    
    ## saved summary of all best models. (if more than 1)
    name514gs = unique(wtfitgaum.mse1$.id)
    wtfitgaum.sum = ldply(wtfitgaum$fit[name514gs], function(x) x$mse) %>% tbl_df
    saveRDS(wtfitgaum.sum, file = paste0(pth, '/wtfitgaum.sum.rds'))
    
    ## saved best model.
    wtfitgaum.best = wtfitgaum$fit[name514gs]
    saveRDS(wtfitgaum.best, file = paste0(pth, '/wtfitgaum.best.rds'))
    
    ## saved best model's formula.
    wtfitgaum.form = wtfitgaum$formula1[str_replace_all(name514gs, 'wtfitgaum', '') %>% as.numeric]
    saveRDS(wtfitgaum.form, file = paste0(pth, '/wtfitgaum.form.rds'))
    
  } else {
    fitgaum = gsfit; rm(gsfit)
    
    ## generates 224 models
    saveRDS(fitgaum, file = paste0(pth, '/fitgaum.rds'))
    
    ## saved best mean-squared error comparison.
    fitgaum.mse1 = ldply(fitgaum$fit, function(x) x$mse) %>% tbl_df %>% filter(mse == min(mse))
    saveRDS(fitgaum.mse1, file = paste0(pth, '/fitgaum.mse1.rds'))
    
    ## saved summary of all best models. (if more than 1)
    name514gs = unique(fitgaum.mse1$.id)
    fitgaum.sum = ldply(fitgaum$fit[name514gs], function(x) x$mse) %>% tbl_df
    saveRDS(fitgaum.sum, file = paste0(pth, '/fitgaum.sum.rds'))
    
    ## saved best model.
    fitgaum.best = fitgaum$fit[name514gs]
    saveRDS(fitgaum.best, file = paste0(pth, '/fitgaum.best.rds'))
    
    ## saved best model's formula.
    fitgaum.form = fitgaum$formula1[str_replace_all(name514gs, 'fitgaum', '') %>% as.numeric]
    saveRDS(fitgaum.form, file = paste0(pth, '/fitgaum.form.rds'))
  }
})

## ---------- Start corerection on the selected models ---------------------------

check.form <- ldply(list.files('./data', pattern = '^[0-9]{8}$'), function(x) {
   ldply(list.files(paste0('./data/', x), pattern = '^fitgaum.mse1.rds$'), 
		function(y) {
     read_rds(path = paste0('./data/', x, '/', y))
   }) %>% data.frame(x, .) %>% tbl_df
}) %>% tbl_df

> ## best fit models across the study from 2015-01-01 to 2017-01-20 (trading days only).
> check.form$.id %>% as.character %>% unique %>% str_extract_all('[0-9]{1,}') %>% as.numeric %>% sort
 [1]  17  18  19  20  22  23  24  25  26  27  28  30  31  49  50  51  52  53  54  55  56  57  58  59  60  61
[27]  62  63  64  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96 113 114 115 116 117 118 119
[53] 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 153
[79] 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168

## Need to review and filter only take models (based on formula) from 17 to 168 for weighted models.


## ---------- End corerection on the selected models ---------------------------


## =============================================================
files <- list.files('./data/20150102/', pattern = 'fitgaum+[0-9]{1,}.rds$')

wt <- ldply(files, function(x) {
  y = read_rds(path = paste0('./data/', x, '/', x))
  y %>% data.frame(Date = x, .) %>% tbl_df %>% filter(mse == min(mse))
}) %>% tbl_df



## ==================================== MCMC Model ===========================================
## Application of MCMC to simulate the Profit and Loss of opt.Kelly() and optimal.f() 
##   staking model.








