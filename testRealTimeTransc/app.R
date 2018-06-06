library('shiny')
library('shinyjs')
library('TFX')
library('cronR')
library('formattable')
library('data.table')
library('DT')

#'@ jscode <- 'shinyjs.refresh = function() { history.go(0); }'


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel('Test the real-time closing transaction'),
    
    mainPanel(
        p('I created this app to test the real-time transaction matching... ', 
          'Once the bid/ask price match with forecasted price, a transaction ', 
          'will be done.'), 
        h4('Real Time Data'), 
        p('Real Time bid/ask price and placed orders.'), 
        formattableOutput('fxdata'), 
        br(), 
        h4('Closed Transaction'), 
        p('Transactions done.'), 
        #'@ useShinyjs(), 
        #'@ downloadButton('downloadData', 'Download'), 
        actionButton('refresh', 'Refresh Data', 
                     icon = icon('refresh'), class = 'btn-primary'), 
        DT::dataTableOutput('transc')))
    
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    armaSearch <- function(data, .method = 'CSS-ML'){ 
        ## ARMA Modeling寻找AIC值最小的p,q
        ##
        ## I set .method = 'CSS-ML' as default method since the AIC value we got is 
        ##  smaller than using method 'ML' while using method 'CSS' facing error.
        ## 
        ## https://stats.stackexchange.com/questions/209730/fitting-methods-in-arima
        ## According to the documentation, this is how each method fits the model:
        ##  - CSS minimises the sum of squared residuals.
        ##  - ML maximises the log-likelihood function of the ARIMA model.
        ##  - CSS-ML mixes both methods: first, CSS is run, the starting parameters 
        ##    for the optimization algorithm are set to zeros or to the values given 
        ##    in the optional argument init; then, ML is applied passing the CSS 
        ##    parameter estimates as starting parameter values for the optimization algorithm.
        
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
        forC.USDJPY <- calC('JPY=X', price = price)
        
        fxC <- ldply(list(
            USDJPY = forC.USDJPY), function(x) 
                data.frame(ForecastDate.GMT = rownames(x$forecastPrice), 
                                         x$forecastPrice)) %>% 
            rename(Currency = USD.JPY) %>% 
            mutate(Currency = as.numeric(str_replace_all(Currency, 'NA|_', '')))
        if(price == 'Hi') names(fxC)[3] <- 'Currency.Hi'
        if(price == 'Lo') names(fxC)[3] <- 'Currency.Lo'
        
        return(fxC)
    }
    
    fcstPunterData <- reactive({
        ## Change when the "update" button is pressed...
        #'@ input$curr
        
        ## ...but not for anything else
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
            saveRDS(fxHL, paste0('data/fcstPunterGMT', today('GMT'), '.rds'))
        }
        return(fxHL)
    })
    
    fetchData <- reactive({
        #if(!input$pause)
            invalidateLater(750)
        qtf <- QueryTrueFX() ## http://webrates.truefx.com/rates/connect.html
        qtf$TimeStamp <- as.character(qtf$TimeStamp)
        names(qtf)[6] <- 'TimeStamp (GMT)'
        qtf <- qtf[, c(6, 1:3, 5:4)]
        return(qtf)
    })
    
    refresh <- reactive({
        line <- fetchData()
        invalidateLater(1000, session)
        rx <- line %>% filter(Symbol == 'USD/JPY') %>% 
            mutate(
                Bid.Price = round(Bid.Price, 3), 
                Ask.Price = round(Ask.Price, 3), 
                #fc.High = round(quantile(c(High, Low))[4], 3), 
                #fc.Low = round(quantile(c(High, Low))[2], 3)) %>%
                fc.High = 110.180, fc.Low = 110.170) %>% 
            dplyr::select(`TimeStamp (GMT)`, Bid.Price, Ask.Price, 
                          fc.High, fc.Low)
        return(rx)
    })
    
    output$fxdata <- renderFormattable({
        rx <- refresh()
        rx %>% formattable(list(
            Bid.Price = formatter('span', 
                                  style = x ~ style(color = ifelse(x > (rx$fc.Low + rx$fc.High) / 2, 'red', 'green')), 
                                  x ~ icontext(ifelse(x > (rx$fc.Low + rx$fc.High) / 2, 'arrow-down', 'arrow-up'), x)), 
            Ask.Price = formatter('span', 
                                  style = x ~ style(color = ifelse(x < (rx$fc.Low + rx$fc.High) / 2, 'red', 'green')),
                                  x ~ icontext(ifelse(x < (rx$fc.Low + rx$fc.High) / 2, 'arrow-down', 'arrow-up'), x)), 
            fc.Low = formatter('span', 
                            style = x ~ style(color = ifelse(x > 0, 'red', 'green')), 
                            x ~ icontext(ifelse(x > 0, 'arrow-down', 'arrow-up'), x)), 
            fc.High = formatter('span',
                             style = x ~ style(color = ifelse(x < 0, 'red', 'green')),
                             x ~ icontext(ifelse(x < 0, 'arrow-down', 'arrow-up'), x))
        ))})
    
    output$transc <- DT::renderDataTable({
        
        rx <- refresh()
        input$refresh
        
        #'@ invalidateLater(1000, session)
        #'@ tr.buy <- data.frame()
        #'@ tr.sell <- data.frame()
        tr.buy <- ldply(dir('data', pattern = 'buy'), function(x){
            readRDS(paste0('data/', x))
        })
        tr.sell <- ldply(dir('data', pattern = 'sell'), function(x){
            readRDS(paste0('data/', x))
        })
        
        if(rx$fc.Low == rx$Bid.Price){
            tr.buy <- rx %>% mutate(Price = fc.Low, Transaction = 'Buy') %>% 
                dplyr::select(`TimeStamp (GMT)`, Price, Transaction)
            saveRDS(tr.buy, paste0('data/buy.', now('GMT'), '.rds'))
        }
        if(rx$fc.Hi == rx$Ask.Price){
            tr.sell <- rx %>% mutate(Price = fc.High, Transaction = 'Sell') %>% 
                dplyr::select(`TimeStamp (GMT)`, Price, Transaction)
            saveRDS(tr.sell, paste0('data/sell.', now('GMT'), '.rds'))
        }
        
        trn <- bind_rows(tr.sell, tr.buy) %>% 
            mutate(`TimeStamp (GMT)` = ymd_hms(`TimeStamp (GMT)`), 
                   Transaction = factor(Transaction)) %>% 
            dplyr::arrange(desc(`TimeStamp (GMT)`)) %>% 
            mutate(ID = rev(seq_len(nrow(.))), 
                   `TimeStamp (GMT)` = factor(`TimeStamp (GMT)`))
        
        trn %>% DT::datatable(caption = "Transaction Table", 
                              escape = FALSE, filter = 'top', rownames = FALSE, 
                              extensions = list('ColReorder' = NULL, 'RowReorder' = NULL, 
                                                'Buttons' = NULL, 'Responsive' = NULL), 
                              options = list(dom = 'BRrltpi', scrollX = TRUE, #autoWidth = TRUE, 
                                             lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')), 
                                             ColReorder = TRUE, rowReorder = TRUE, 
                                             buttons = list('copy', 'print', 
                                                            list(extend = 'collection', 
                                                                 buttons = c('csv', 'excel', 'pdf'), 
                                                                 text = 'Download'), I('colvis'))))
    })
    
    #'@ observeEvent(input$refresh, {
    #'@     js$refresh();
    #'@ })
    
    #'@ trnData <- reactive({
    #'@     ldply(dir('data', pattern = 'sell|buy'), function(x){
    #'@         readRDS(paste0('data/', x))}) %>% 
    #'@         mutate(`TimeStamp (GMT)` = ymd_hms(`TimeStamp (GMT)`), 
    #'@                Transaction = factor(Transaction)) %>% 
    #'@         dplyr::arrange(desc(`TimeStamp (GMT)`)) %>% 
    #'@         mutate(ID = rev(seq_len(nrow(.))), 
    #'@                `TimeStamp (GMT)` = factor(`TimeStamp (GMT)`))
    #'@ })
    
    ## https://stackoverflow.com/questions/44504759/shiny-r-download-the-result-of-a-table?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
    #'@ output$downloadData <- downloadHandler(
    #'@     filename = function() {'transaction.csv'},
    #'@     content = function(file) {
    #'@         fwrite(trnData(), file)
    #'@     })
    
    ## https://rdrr.io/cran/DT/src/inst/examples/DT-reload/app.R
    #'@ loopData = reactive({
    #'@     input$refresh
    #'@     trn <- trnData()
    #'@     trn$ID <<- c(trn$ID[nrow(trn)], trn$ID[-nrow(trn)])
    #'@     trn
    #'@ })
    
    #'@ output$transc = DT::renderDataTable(isolate(loopData()))
    #'@ 
    #'@ proxy = dataTableProxy('transc')
    #'@ 
    #'@ observe({
    #'@     replaceData(proxy, loopData(), resetPaging = FALSE)
    #'@ })
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
#'@ runApp('testRealTimeTransc', display.mode = 'showcase')


