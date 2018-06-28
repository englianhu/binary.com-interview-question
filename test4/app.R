## ================== Declaration ========================================
suppressWarnings(require('shiny'))
suppressWarnings(require('BBmisc'))
suppressWarnings(require('memoise'))
suppressWarnings(require('stringr'))
suppressWarnings(require('xts'))
suppressWarnings(require('TFX'))
suppressWarnings(require('quantmod'))
suppressWarnings(require('rugarch'))
suppressWarnings(require('lubridate'))
suppressWarnings(require('ggplot2'))
suppressWarnings(require('highcharter'))
suppressWarnings(require('formattable'))
suppressWarnings(require('magrittr'))
suppressWarnings(require('plyr'))
suppressWarnings(require('dplyr'))
suppressWarnings(require('pryr'))
suppressWarnings(require('tidyr'))
suppressWarnings(require('purrr'))
suppressWarnings(require('cronR'))
suppressWarnings(require('microbenchmark'))
suppressWarnings(require('cronR'))
suppressWarnings(require('xts'))
suppressWarnings(require('quantmod'))
suppressWarnings(require('TFX'))
suppressWarnings(require('stringr'))
suppressWarnings(require('RCurl'))

Sys.setenv(TZ = 'GMT')
zones <- attr(as.POSIXlt(now('GMT')), 'tzone')
zone <- ifelse(zones[[1]] == '', paste(zones[-1], collapse = '/'), 
               zones[[1]])
timeR <- now('GMT')

fx <- c('EURUSD=X', 'JPY=X', 'GBPUSD=X', 'CHF=X', 'CAD=X', 'AUDUSD=X')
wd <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
wd %<>% factor(., levels = ., ordered = TRUE)
## https://beta.rstudioconnect.com/connect/#/apps/3803/logs
## ================== Functions ========================================
forecastData <- function(price = 'Cl') {
    forC.EURUSD <- calC('EURUSD=X', price = price)
    forC.USDJPY <- calC('JPY=X', price = price)
    forC.GBPUSD <- calC('GBPUSD=X', price = price)
    forC.USDCHF <- calC('CHF=X', price = price)
    forC.USDCAD <- calC('CAD=X', price = price)
    forC.AUDUSD <- calC('AUDUSD=X', price = price)
    
    fxC <- ldply(list(EURUSD = forC.EURUSD, 
                      USDJPY = forC.USDJPY, 
                      GBPUSD = forC.GBPUSD, 
                      USDCHF = forC.USDCHF, 
                      USDCAD = forC.USDCAD, 
                      AUDUSD = forC.AUDUSD), function(x) 
                          data.frame(ForecastDate.GMT = rownames(x$forecastPrice), 
                                     x$forecastPrice)) %>% 
        unite(., Currency, EUR.USD:AUD.USD) %>% 
        mutate(Currency = as.numeric(str_replace_all(Currency, 'NA|_', '')))
    if(price == 'Hi') names(fxC)[3] <- 'Currency.Hi'
    if(price == 'Lo') names(fxC)[3] <- 'Currency.Lo'
    
    return(fxC)
}

fcstPunterData <- reactive({
    isolate({
        withProgress({
            setProgress(message = "Processing algorithmic forecast...")
            fxLo <- forecastData(price = 'Lo')
            fxHi <- forecastData(price = 'Hi')
            fxHL <- merge(fxHi, fxLo, by = c('.id', 'ForecastDate.GMT'))
            rm(fxHi, fxLo)
        })
    })
    
    if(!dir.exists('data')) dir.create('data')
    if(!file.exists(paste0('data/fcstPunterGMT', today('GMT'), '.rds'))){
        saveRDS(fxHL, paste0('data/fcstPunterGMT', today('GMT'), '.rds')) }
    
    return(fxHL)
})




## Function to get new observations
#'@ get_new_data <- function() readLines('http://webrates.truefx.com/rates/connect.html')

armaSearch <- function(data, .method = 'CSS-ML'){ 
    .methods = c('CSS-ML', 'ML', 'CSS')
    
    if(!.method %in% .methods) stop(paste('Kindly choose .method among ', 
                                          paste0(.methods, collapse = ', '), '!'))
    
    armacoef <- data.frame()
    for (p in 0:5){
        for (q in 0:5) {
            #data.arma = arima(diff(data), order = c(p, 0, q))
            #'@ data.arma = arima(data, order = c(p, 1, q), method = .method)
            if(.method == 'CSS-ML') {
                data.arma = tryCatch({
                    arma = arima(data, order = c(p, 1, q), method = 'CSS-ML')
                    mth = 'CSS-ML'
                    list(arma, mth)
                }, error = function(e) {
                    arma = arima(data, order = c(p, 1, q), method = 'ML')
                    mth = 'ML'
                    list(arma = arma, mth = mth)
                })
            } else if(.method == 'ML') {
                data.arma = tryCatch({
                    arma = arima(data, order = c(p, 1, q), method = 'ML')
                    mth = 'ML'
                    list(arma = arma, mth = mth)
                }, error = function(e) {
                    arma = arima(data, order = c(p, 1, q), method = 'CSS-ML')
                    mth = 'CSS-ML'
                    list(arma = arma, mth = mth)
                })
            } else if(.method == 'CSS') {
                data.arma = tryCatch({
                    arma = arima(data, order = c(p, 1, q), method = 'CSS')
                    mth = 'CSS'
                    list(arma = arma, mth = mth)
                }, error = function(e) {
                    arma = arima(data, order = c(p, 1, q), method = 'CSS-ML')
                    mth = 'CSS-ML'
                    list(arma = arma, mth = mth)
                })
            } else {
                stop(paste('Kindly choose .method among ', 
                           paste0(.methods, collapse = ', '), '!'))
            }
            names(data.arma) <- c('arma', 'mth')
            
            #cat('p =', p, ', q =', q, 'AIC =', data.arma$arma$aic, '\n')
            armacoef <- rbind(armacoef, c(p, q, data.arma$arma$aic))
        }
    }
    
    colnames(armacoef) <- c('p', 'q', 'AIC')
    pos <- which(armacoef$AIC == min(armacoef$AIC))
    cat(paste0('method = \'', data.arma$mth, '\', the min AIC = ', 
               armacoef$AIC[pos], ', p = ', armacoef$p[pos], 
               ', q = ', armacoef$q[pos], '\n'))
    return(armacoef)
}

filterFX <- function(currency, price = 'Cl') {
    if(currency == 'AUDUSD=X') {
        if(price == 'Op') {
            mbase <- `AUDUSD=X` %>% Op %>% na.omit; rm(`AUDUSD=X`)
        } else if(price == 'Hi') {
            mbase <- `AUDUSD=X` %>% Hi %>% na.omit; rm(`AUDUSD=X`)
        } else if(price == 'Lo') {
            mbase <- `AUDUSD=X` %>% Lo %>% na.omit; rm(`AUDUSD=X`)
        } else if(price == 'Cl') {
            mbase <- `AUDUSD=X` %>% Cl %>% na.omit; rm(`AUDUSD=X`)
        } else if(price == 'Ad') {
            mbase <- `AUDUSD=X` %>% Ad %>% na.omit; rm(`AUDUSD=X`)
        } else {
            stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
        }
        names(mbase) %<>% str_replace_all('AUDUSD=X', 'AUD.USD')
        names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
        
    } else if(currency == 'EURUSD=X') {
        if(price == 'Op') {
            mbase <- `EURUSD=X` %>% Op %>% na.omit; rm(`EURUSD=X`)
        } else if(price == 'Hi') {
            mbase <- `EURUSD=X` %>% Hi %>% na.omit; rm(`EURUSD=X`)
        } else if(price == 'Lo') {
            mbase <- `EURUSD=X` %>% Lo %>% na.omit; rm(`EURUSD=X`)
        } else if(price == 'Cl') {
            mbase <- `EURUSD=X` %>% Cl %>% na.omit; rm(`EURUSD=X`)
        } else if(price == 'Ad') {
            mbase <- `EURUSD=X` %>% Ad %>% na.omit; rm(`EURUSD=X`)
        } else {
            stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
        }
        names(mbase) %<>% str_replace_all('EURUSD=X', 'EUR.USD')
        names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
        
    } else if(currency == 'GBPUSD=X') {
        if(price == 'Op') {
            mbase <- `GBPUSD=X` %>% Op %>% na.omit; rm(`GBPUSD=X`)
        } else if(price == 'Hi') {
            mbase <- `GBPUSD=X` %>% Hi %>% na.omit; rm(`GBPUSD=X`)
        } else if(price == 'Lo') {
            mbase <- `GBPUSD=X` %>% Lo %>% na.omit; rm(`GBPUSD=X`)
        } else if(price == 'Cl') {
            mbase <- `GBPUSD=X` %>% Cl %>% na.omit; rm(`GBPUSD=X`)
        } else if(price == 'Ad') {
            mbase <- `GBPUSD=X` %>% Ad %>% na.omit; rm(`GBPUSD=X`)
        } else {
            stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
        }
        names(mbase) %<>% str_replace_all('GBPUSD=X', 'GBP.USD')
        names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
        
    } else if(currency == 'CHF=X') {
        if(price == 'Op') {
            mbase <- `CHF=X` %>% Op %>% na.omit; rm(`CHF=X`)
        } else if(price == 'Hi') {
            mbase <- `CHF=X` %>% Hi %>% na.omit; rm(`CHF=X`)
        } else if(price == 'Lo') {
            mbase <- `CHF=X` %>% Lo %>% na.omit; rm(`CHF=X`)
        } else if(price == 'Cl') {
            mbase <- `CHF=X` %>% Cl %>% na.omit; rm(`CHF=X`)
        } else if(price == 'Ad') {
            mbase <- `CHF=X` %>% Ad %>% na.omit; rm(`CHF=X`)
        } else {
            stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
        }
        names(mbase) %<>% str_replace_all('CHF=X', 'USD.CHF')
        names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
        
    } else if(currency == 'CAD=X') {
        if(price == 'Op') {
            mbase <- `CAD=X` %>% Op %>% na.omit; rm(`CAD=X`)
        } else if(price == 'Hi') {
            mbase <- `CAD=X` %>% Hi %>% na.omit; rm(`CAD=X`)
        } else if(price == 'Lo') {
            mbase <- `CAD=X` %>% Lo %>% na.omit; rm(`CAD=X`)
        } else if(price == 'Cl') {
            mbase <- `CAD=X` %>% Cl %>% na.omit; rm(`CAD=X`)
        } else if(price == 'Ad') {
            mbase <- `CAD=X` %>% Ad %>% na.omit; rm(`CAD=X`)
        } else {
            stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
        }
        names(mbase) %<>% str_replace_all('CAD=X', 'USD.CAD')
        names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
        
    } else if(currency == 'CNY=X') {
        if(price == 'Op') {
            mbase <- `CNY=X` %>% Op %>% na.omit; rm(`CNY=X`)
        } else if(price == 'Hi') {
            mbase <- `CNY=X` %>% Hi %>% na.omit; rm(`CNY=X`)
        } else if(price == 'Lo') {
            mbase <- `CNY=X` %>% Lo %>% na.omit; rm(`CNY=X`)
        } else if(price == 'Cl') {
            mbase <- `CNY=X` %>% Cl %>% na.omit; rm(`CNY=X`)
        } else if(price == 'Ad') {
            mbase <- `CNY=X` %>% Ad %>% na.omit; rm(`CNY=X`)
        } else {
            stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
        }
        names(mbase) %<>% str_replace_all('CNY=X', 'USD.CNY')
        names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
        
    } else if(currency == 'JPY=X') {
        if(price == 'Op') {
            mbase <- `JPY=X` %>% Op %>% na.omit; rm(`JPY=X`)
        } else if(price == 'Hi') {
            mbase <- `JPY=X` %>% Hi %>% na.omit; rm(`JPY=X`)
        } else if(price == 'Lo') {
            mbase <- `JPY=X` %>% Lo %>% na.omit; rm(`JPY=X`)
        } else if(price == 'Cl') {
            mbase <- `JPY=X` %>% Cl %>% na.omit; rm(`JPY=X`)
        } else if(price == 'Ad') {
            mbase <- `JPY=X` %>% Ad %>% na.omit; rm(`JPY=X`)
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

# Using "memoise" to automatically cache the results
calC <- memoise(function(currency, ahead = 1, price = 'Cl') {
    
    mbase = filterFX(currency, price = price)
    
    armaOrder = armaSearch(mbase)
    armaOrder %<>% dplyr::filter(AIC == min(AIC)) %>% .[c('p', 'q')] %>% unlist
    
    spec = ugarchspec(
        variance.model = list(
            model = 'gjrGARCH', garchOrder = c(1, 1), 
            submodel = NULL, external.regressors = NULL, 
            variance.targeting = FALSE), 
        mean.model = list(
            armaOrder = armaOrder, 
            include.mean = TRUE, archm = FALSE, 
            archpow = 1, arfima = FALSE, 
            external.regressors = NULL, 
            archex = FALSE), 
        distribution.model = 'snorm')
    fit = ugarchfit(spec, mbase, solver = 'hybrid')
    fc = ugarchforecast(fit, n.ahead = ahead)
    res = attributes(fc)$forecast$seriesFor
    colnames(res) = names(mbase)
    latestPrice = tail(mbase, 1)
    forDate = latestPrice %>% index + days(1)
    rownames(res) <- as.character(forDate)
    
    tmp = list(latestPrice = latestPrice, forecastPrice = res)
    return(tmp)
})

forecastData <- function(price = 'Cl') {
    forC.EURUSD <- calC('EURUSD=X', price = price)
    forC.USDJPY <- calC('JPY=X', price = price)
    forC.GBPUSD <- calC('GBPUSD=X', price = price)
    forC.USDCHF <- calC('CHF=X', price = price)
    forC.USDCAD <- calC('CAD=X', price = price)
    forC.AUDUSD <- calC('AUDUSD=X', price = price)
    
    fxC <- ldply(list(EURUSD = forC.EURUSD, 
                      USDJPY = forC.USDJPY, 
                      GBPUSD = forC.GBPUSD, 
                      USDCHF = forC.USDCHF, 
                      USDCAD = forC.USDCAD, 
                      AUDUSD = forC.AUDUSD), function(x) 
                          data.frame(ForecastDate.GMT = rownames(x$forecastPrice), 
                                     x$forecastPrice)) %>% 
        unite(., Currency, EUR.USD:AUD.USD) %>% 
        mutate(Currency = as.numeric(str_replace_all(Currency, 'NA|_', '')))
    if(price == 'Hi') names(fxC)[3] <- 'Currency.Hi'
    if(price == 'Lo') names(fxC)[3] <- 'Currency.Lo'
    
    return(fxC)
}

## ================== Reference ========================================
## https://shiny.rstudio.com/articles/persistent-data-storage.html
## https://github.com/bnosac/cronR
## http://www.bnosac.be/index.php/blog/64-scheduling-r-scripts-and-processes-on-windows-and-unix-linux

# === Data =====================================================
Sys.setenv(TZ = 'GMT')
zones <- attr(as.POSIXlt(now('GMT')), 'tzone')
zone <- ifelse(zones[[1]] == '', paste(zones[-1], collapse = '/'), zones[[1]])

fx <<- c('EURUSD=X', 'JPY=X', 'GBPUSD=X', 'CHF=X', 'CAD=X', 'AUDUSD=X')
cur <<- c('EUR/USD', 'USD/JPY', 'GBP/USD', 'USD/CHF', 'USD/CAD', 'AUD/USD')
wd <<- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

# === Shiny UI =====================================================
ui <- fluidPage(
    textOutput('currentTime'), 
    tableOutput('transc'), 
    formattableOutput('fxdata'))

server <- function(input, output, session) {
    
    output$currentTime <- renderText({
        # Forces invalidation in 1000 milliseconds
        invalidateLater(1000, session)
        as.character(now('GMT'))
        })
    
    fetchData <- reactive({
        if(!input$pause)
            invalidateLater(750)
        qtf <- QueryTrueFX() %>% mutate(TimeStamp = as.character(TimeStamp)) %>% 
            rename(`TimeStamp (GMT)` = TimeStamp) %>% 
            filter(Symbol %in% c('EUR/USD', 'USD/JPY', 'GBP/USD', 'USD/CHF', 
                                 'USD/CAD', 'AUD/USD'))
        ## http://webrates.truefx.com/rates/connect.html
        qtf <<- qtf[, c(6, 1:3, 5:4)]
        return(qtf)
    })
    
    output$transc <- renderTable({
        
        #invalidateLater(750)
        rx <- qtf %>% filter(Symbol == 'USD/JPY') %>% 
            dplyr::select(`TimeStamp (GMT)`, Bid.Price, Ask.Price)
        
        fxHL <- fcstPunterData()
        Hi <- tail(fxHL, 1)$Currency.Hi %>% round(3)
        Lo <- tail(fxHL, 1)$Currency.Lo %>% round(3)
        transc.buy <- data.frame()
        transc.sell <- data.frame()
        
        # qtf %>% filter(Symbol == 'USD/JPY') %>% select(`TimeStamp (GMT)`, Bid.Price, Ask.Price)
        # fxHL %>% filter(.id == 'USDJPY') %>% select(Currency.Hi) %>% unclass %>% .$Currency.Hi
        if(Lo == rx$Bid.Price){
            transc.buy <- tail(fxHL, 1) %>% 
                dplyr::select(ForecastDate.GMT, Currency.Lo) %>% 
                mutate(Currency.Lo = round(Currency.Lo, 3))
            saveRDS(transc.buy, paste0('data/buy.', now('GMT'), '.rds')) }
        
        if(Hi == rx$Ask.Price){
            transc.sell <- tail(fxHL, 1) %>% 
                dplyr::select(ForecastDate.GMT, Currency.Hi) %>% 
                mutate(Currency.Hi = round(Currency.Hi, 3))
            saveRDS(transc.sell, paste0('data/sell.', now('GMT'), '.rds')) }
        
        tmp <- list(buy.transc = transc.buy, sell.transc = transc.sell)
        return(tmp)
    })
    
    output$fxdata <- renderFormattable({
        line <- fetchData()
        line %>% formattable(list(
            Symbol = formatter('span', 
                               style = x ~ ifelse(x == 'Technology', 
                                                  style(font.weight = 'bold'), NA)),
            Bid.Price = formatter('span', 
                                  style = x ~ style(color = ifelse(x > (line$Low + line$High) / 2, 'red', 'green')), 
                                  x ~ icontext(ifelse(x > (line$Low + line$High) / 2, 'arrow-down', 'arrow-up'), x)), 
            Ask.Price = formatter('span', 
                                  style = x ~ style(color = ifelse(x < (line$Low + line$High) / 2, 'red', 'green')),
                                  x ~ icontext(ifelse(x < (line$Low + line$High) / 2, 'arrow-down', 'arrow-up'), x)), 
            Low = formatter('span', 
                            style = x ~ style(color = ifelse(x > 0, 'red', 'green')), 
                            x ~ icontext(ifelse(x > 0, 'arrow-down', 'arrow-up'), x)), 
            High = formatter('span',
                             style = x ~ style(color = ifelse(x < 0, 'red', 'green')),
                             x ~ icontext(ifelse(x < 0, 'arrow-down', 'arrow-up'), x))
        ))})
    
    session$allowReconnect(TRUE)
    }


shinyApp(server, ui)


