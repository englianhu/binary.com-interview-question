library('quantmod')
library('forecast')
library('rugarch')

## ============= Function ==============================
filterFX <- function(mbase, currency = 'JPY=X', price = 'Cl') {
  
  if(currency == 'AUDUSD=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; rm(`AUDUSD=X`)
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; rm(`AUDUSD=X`)
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; rm(`AUDUSD=X`)
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; rm(`AUDUSD=X`)
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; rm(`AUDUSD=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('AUDUSD=X', 'AUD.USD')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else if(currency == 'EURUSD=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; rm(`EURUSD=X`)
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; rm(`EURUSD=X`)
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; rm(`EURUSD=X`)
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; rm(`EURUSD=X`)
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; rm(`EURUSD=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('EURUSD=X', 'EUR.USD')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else if(currency == 'GBPUSD=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; rm(`GBPUSD=X`)
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; rm(`GBPUSD=X`)
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; rm(`GBPUSD=X`)
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; rm(`GBPUSD=X`)
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; rm(`GBPUSD=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('GBPUSD=X', 'GBP.USD')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else if(currency == 'CHF=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; rm(`CHF=X`)
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; rm(`CHF=X`)
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; rm(`CHF=X`)
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; rm(`CHF=X`)
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; rm(`CHF=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('CHF=X', 'USD.CHF')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else if(currency == 'CAD=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; rm(`CAD=X`)
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; rm(`CAD=X`)
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; rm(`CAD=X`)
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; rm(`CAD=X`)
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; rm(`CAD=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('CAD=X', 'USD.CAD')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else if(currency == 'CNY=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; rm(`CNY=X`)
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; rm(`CNY=X`)
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; rm(`CNY=X`)
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; rm(`CNY=X`)
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; rm(`CNY=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('CNY=X', 'USD.CNY')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else if(currency == 'JPY=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; rm(`JPY=X`)
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; rm(`JPY=X`)
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; rm(`JPY=X`)
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; rm(`JPY=X`)
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; rm(`JPY=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('JPY=X', 'USD.JPY')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else {
    stop('Kindly choose common currencies exchange.')
  }
  return(mbase)
}


## ============= Data ==============================
prd = 1 #since count trading day.
fxObj <- c('USDJPY')

for(i in seq(fx)) {
  assign(fxObj[i], na.omit(suppressWarnings(
    getSymbols(fx[i], from = (today('GMT') - days(prd)) %m-% years(2), 
               to = (today('GMT') - days(prd)), auto.assign = FALSE)))) }
rm(i)

USDJPY <- `JPY=X`
mbase <- USDJPY
rm(`JPY=X`)
#names(mbase) <- str_replace_all(names(mbase), 'JPY=X', 'USDJPY')

if(!is.xts(mbase)) mbase <- xts(mbase[, -1], order.by = mbase$Date)
dateID <- index(mbase)
dateID0 <- dateID[259] #since 2 years data length(dateID) is 518.
dateID <- dateID[dateID >= dateID0]

## Now we try to use the daily mean value which is (Hi + Lo) / 2.
pred.data <- ldply(dateID, function(dt) {
  smp = mbase
  dtr = tail(index(smp[index(smp) < dt]), 1)
  smp <- smp[paste0(dtr %m-% years(1), '/', dtr)]
  frd = as.numeric(difftime(dt, dtr), units = 'days')
  fit = forecastUSDJPYHL(mbase = smp, currency = 'JPY=X', ahead = 1)
  saveRDS(fit, paste0('testRealTimeTransc/data/fcstPunterGMT', as.character(dt), '.rds'))
  cat(as.character(dt), '\n')
  fit
  }, .parallel = FALSE)

## -----------------------------------------------------------------------
## Now we try to use the daily mean value which is (Hi + Lo) / 2.
pred.dataHL <- ldply(dateID, function(dt) {
  smp = mbase
  dtr = tail(index(smp[index(smp) < dt]), 1)
  smp <- smp[paste0(dtr %m-% years(1), '/', dtr)]
  frd = as.numeric(difftime(dt, dtr), units = 'days')
  fit = forecastUSDJPYHL(mbase = smp, currency = 'JPY=X', ahead = 1, .preCat = 'Hi', .setPrice = 'Lo')
  #saveRDS(fit, paste0('testRealTimeTransc/data/fcstPunterGMT', as.character(dt), '.rds'))
  cat(as.character(dt), '\n')
  fit
}, .parallel = FALSE)


## -----------------------------------------------------------------------
#ldply(dir('testRealTimeTransc/data', pattern = '^fit.'), function(dt) 
#  readRDS(paste0('testRealTimeTransc/data/', as.character(dt))))

#nm <- llply(dir('testRealTimeTransc/data', pattern = '^fit.'), function(x) {
#  str_replace_all(x, '^fit.', 'fcstPunterGMT')
#})
#
#file.rename(from = file.path(getwd(), 'testRealTimeTransc/data', 
#                             list.files('testRealTimeTransc/data', pattern = '^fit.')), 
#            to = file.path(getwd(), 'testRealTimeTransc/data', nm))

## Set as our daily settlement price.
obs.data <- mbase[index(mbase) > dateID0]

#pred.data %>% mutate(
#  ProbB = pnorm(Fct.Low, mean = mean(Fct.High), sd = sd(Fct.High)), 
#  ProbS = 1 - ProbB, Fct.High = round(Fct.High, 3), 
#  Fct.Low = round(Fct.Low, 3)) %>% data.table

#pred.data %>% mutate(
#  ProbB = pnorm(Fct.High, mean = mean(Fct.Low), sd = sd(Fct.Low)), 
#  ProbS = 1 - ProbB, Fct.High = round(Fct.High, 3), 
#  Fct.Low = round(Fct.Low, 3)) %>% data.table

## Closed price as settlement price for those no closing transaction.
Closed <- mbase %>% Cl %>% data.frame
Closed <- data.frame(LatestDate.GMT = as.Date(rownames(Closed)), 
                     Lst.Close = Closed$JPY.X.Close)
rownames(Closed) <- NULL

pred.data <- plyr::join(pred.data, Closed, by = 'LatestDate.GMT')

# ---------------- KellyS ------------------------------------------
KellyS <- function(fitm, .preCat = 'Lo', .forCat = 'Hi', .initialFundSize = 10000, 
                   .filterBets = FALSE, .fundLeverageLog = FALSE) {
  
  if(.preCat == 'Op') fitm %<>% rename(Point.Forecast = Fct.Open)
  if(.preCat == 'Hi') fitm %<>% rename(Point.Forecast = Fct.High)
  if(.preCat == 'Lo') fitm %<>% rename(Point.Forecast = Fct.Low)
  if(.preCat == 'Cl') fitm %<>% rename(Point.Forecast = Fct.Close)
  
  if(.forCat == 'Op') fitm %<>% rename(forClose = Fct.Open)
  if(.forCat == 'Hi') fitm %<>% rename(forClose = Fct.High)
  if(.forCat == 'Lo') fitm %<>% rename(forClose = Fct.Low)
  if(.forCat == 'Cl') fitm %<>% rename(forClose = Fct.Close)
  
  fitm %<>% mutate(
    ProbB = pnorm(Point.Forecast, mean = mean(forClose), sd = sd(forClose)), 
    ProbS = 1 - ProbB)#, Fct.High = round(Fct.High, 3), 
    #Fct.Low = round(Fct.Low, 3))
  
  fitm %<>% mutate(Point.Forecast = round(lag(Point.Forecast), 3), 
                   forClose = round(lag(forClose), 3)) %>% na.omit %>% data.table
  
  fitm %<>% mutate(BR = .initialFundSize) %>% 
    #'@ mutate(Return.Back = ifelse(Prob > 0.5, Diff * Back * stakes, 0), 
    #'@        Return.Lay = ifelse(Prob < 0.5, -Diff * Lay * stakes, 0))
    mutate(fB = 2 * ProbB - 1, fS = 2 * ProbS - 1, 
           EUB = ProbB * log(BR * (1 + fB)) + (1 - ProbB) * log(BR * (1 - fB)), 
           EUS = ProbS * log(BR * (1 + fS)) + (1 - ProbS) * log(BR * (1 - fS)), 
           #EUB = ProbB * (BR * (1 + fB)) + (1 - ProbB) * (BR * (1 - fB)), 
           #EUS = ProbS * (BR * (1 + fS)) + (1 - ProbS) * (BR * (1 - fS)), 
           #'@ Edge = ifelse(f > 0, EUB, EUS), #For f > 0 need to buy and f <= 0 need to sell.
           #need to study on the risk management on "predicted profit" and "real profit".
           Edge = ifelse(fB > 0, EUB, ifelse(fS > 0, EUS, 0)), 
           PF = ifelse(Point.Forecast >= Lst.Low & 
                         Point.Forecast <= Lst.High, 
                       Point.Forecast, 0), #if forecasted place-bet price doesn't existing within Hi-Lo price, then the buying action is not stand. Assume there has no web bandwith delay.
           FC = ifelse(forClose >= Lst.Low & forClose <= Lst.High, 
                       forClose, Lst.Close), #if forecasted settle price doesn't existing within Hi-Lo price, then the closing action at closing price. Assume there has no web bandwith delay.
           #'@ Diff = round(forClose - USDJPY.Close, 2),
           ##forecasted closed price minus real close price.
           
           Buy = ifelse(PF > 0 & FC > PF, 1, 0), ##buy action
           Sell = ifelse(PF > 0 & FC < PF, 1, 0), ##sell action
           BuyS = Edge * Buy * (forClose - PF), 
           SellS = Edge * Sell * (PF - forClose), 
           Profit = BuyS + SellS, Bal = BR + Profit)
  
  
  #'@ fitm %>% dplyr::select(Point.Forecast, forClose, Prob, BR, f, EU, Edge, PF, FC, Buy, Sell, SP, Bal)
  #'@ fitm %>% dplyr::select(ProbB, ProbS, BR, fB, fS, EUB, EUS, Edge, PF, USDJPY.Open, FC, Buy, Sell, BuyS, SellS, Profit, Bal) %>% filter(PF > 0, FC > 0)
  
  ## The ets staking models (Kelly criterion) Adjusted Banl-roll and Balance column.
  for(i in seq(2, nrow(fitm))) {
    fitm$BR[i] = fitm$Bal[i - 1]
    fitm$fB[i] = 2 * fitm$ProbB[i] - 1
    fitm$fS[i] = 2 * fitm$ProbS[i] - 1
    fitm$EUB[i] = fitm$ProbB[i] * log(fitm$BR[i] * (1 + fitm$fB[i])) + 
      (1 - fitm$ProbB[i]) * log(fitm$BR[i] * (1 - fitm$fB[i]))
    fitm$EUS[i] = fitm$ProbS[i] * log(fitm$BR[i] * (1 + fitm$fS[i])) + 
      (1 - fitm$ProbS[i]) * log(fitm$BR[i] * (1 - fitm$fS[i]))
    fitm$Edge[i] = ifelse(fitm$fB[i] > 0, fitm$EUB[i], 
                          ifelse(fitm$fS[i] > 0, fitm$EUS[i], 0)) #For f > 0 need to buy and f <= 0 need to sell.
    #need to study on the risk management on "predicted profit" and "real profit".
    
    fitm$BuyS[i] = fitm$Edge[i] * fitm$Buy[i] * (fitm$forClose[i] - fitm$PF[i])
    fitm$SellS[i] = fitm$Edge[i] * fitm$Sell[i] * (fitm$PF[i] - fitm$forClose[i])
    fitm$Profit[i] = fitm$BuyS[i] + fitm$SellS[i]
    fitm$Bal[i] = fitm$BR[i] + fitm$Profit[i]
    if(fitm$Bal[i] <= 0) stop('All invested fund ruined!')
  }; rm(i)
  
  #names(mbase) <- str_replace_all(names(mbase), '^(.*?)+\\.', nm)
  
  if(.filterBets == TRUE) {
    fitm %<>% filter(PF > 0, FC > 0)
  }
  
  fitm %<>% mutate(RR = Bal/BR)
  
  ## convert the log leverage value of fund size and profit into normal digital figure with exp().
  if(.fundLeverageLog == TRUE) fitm %<>% 
    mutate(BR = exp(BR), BuyS = exp(BuyS), SellS = exp(SellS), 
           Profit = exp(Profit), Bal = exp(Profit))
  
  return(fitm)
  }
# --------------------------------------------------------------------

#> pred.dataHL <- plyr::join(pred.dataHL, Closed, by = 'LatestDate.GMT')
#> 
#> KellyS(pred.dataHL, .preCat = 'Hi', .forCat = 'Lo') %>% 
#    select(BR, Profit, Bal) %>% tail
# BR    Profit      Bal
#255 10009.05  0.000000 10009.05
#256 10009.05  0.000000 10009.05
#257 10009.05 -4.163753 10004.88
#258 10004.88  0.000000 10004.88
#259 10004.88 -3.678163 10001.21
#260 10001.21 10.258022 10011.46
#> pred.dataLH <- plyr::join(pred.dataLH, Closed, by = 'LatestDate.GMT')
#> 
#> KellyS(pred.dataLH, .preCat = 'Lo', .forCat = 'Hi') %>% select(BR, Profit, Bal) %>% tail
# BR   Profit      Bal
#255 10395.06 3.368258 10398.43
#256 10398.43 0.000000 10398.43
#257 10398.43 0.000000 10398.43
#258 10398.43 0.000000 10398.43
#259 10398.43 0.000000 10398.43
#260 10398.43 0.000000 10398.43




