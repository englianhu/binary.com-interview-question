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

pred.data %>% mutate(
  ProbB = pnorm(Fct.Low, mean = mean(Fct.High), sd = sd(Fct.High)), 
  ProbS = 1 - ProbB, Fct.High = round(Fct.High, 3), 
  Fct.Low = round(Fct.Low, 3)) %>% data.table

pred.data %>% mutate(
  ProbB = pnorm(Fct.High, mean = mean(Fct.Low), sd = sd(Fct.Low)), 
  ProbS = 1 - ProbB, Fct.High = round(Fct.High, 3), 
  Fct.Low = round(Fct.Low, 3)) %>% data.table









