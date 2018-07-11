# === Setting ======================================================
# options(repos = 'https://cran.rstudio.com')

suppressWarnings(require('BBmisc'))
suppressWarnings(require('shiny'))
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
suppressWarnings(require('stringr'))

#'@ pkgs <- c('shiny', 'memoise', 'stringr', 'xts', 'TFX', 'quantmod', 
#'@           'rugarch', 'lubridate', 'ggplot2', 'highcharter', 'formattable', 
#'@           'magrittr', 'plyr', 'dplyr', 'pryr', 'tidyr', 'purrr', 'cronR', 
#'@           'microbenchmark')
#'@ suppressAll(requirePackages(pkgs))
#'@ plyr::l_ply(pkgs, require, quietly = TRUE, character.only = TRUE)
#'@ rm(pkgs)

## https://beta.rstudioconnect.com/connect/3768
## https://beta.rstudioconnect.com/connect/3770
## https://beta.rstudioconnect.com/content/3771

# === Data =====================================================
Sys.setenv(TZ = 'Asia/Tokyo')
zones <- attr(as.POSIXlt(now('Asia/Tokyo')), 'tzone')
zone <- ifelse(zones[[1]] == '', paste(zones[-1], collapse = '/'), zones[[1]])

fx <<- c('EURUSD=X', 'JPY=X', 'GBPUSD=X', 'CHF=X', 'CAD=X', 'AUDUSD=X')
#'@ wd <<- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 
#'@          'Sunday')
cur <<- c('EUR/USD', 'USD/JPY', 'GBP/USD', 'USD/CHF', 'USD/CAD', 'AUD/USD')
wd <<- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')#, 'Saturday', 'Sunday')

# === Shiny UI =====================================================
ui <- shinyUI(fluidPage(
  
  titlePanel(
    div(
      tags$a(href='http://www.binary.com', target='_blank', 
             tags$img(height = '80px', alt='binary', #align='right', 
                      src='binary-logo-resize.jpg')), 
      img(src = 'ENG.jpg', width = '40', align = 'right'), 
      img(src = 'RYO.jpg', width = '20', align = 'right'))),
  
  pageWithSidebar(
    #sidebarPanel(), 
    mainPanel(
      tabsetPanel(
        tabPanel('Price', 
                 tabsetPanel(
                   tabPanel('Board', 
                            h3('Real Time Price'), 
                            div(class='container',
                                p(strong(paste0('Current time (', zone, '):')),
                                  textOutput('currentTime')),
                                p(strong('Latest FX Quotes:'),
                                  formattableOutput('fxdata'), 
                                  checkboxInput('pause', 'Pause updates', FALSE)))), 
                   tabPanel('Intraday Graph', 
                            h3('Real Time Price'), 
                            div(class='container',
                                p(strong(paste0('Current time (', zone, '):')),
                                  textOutput('currentTime2')), 
                                br(), 
                                p(strong('Latest FX Price:'),
                                  plotOutput('realPlot')))), 
                   tabPanel('Daily Chart', 
                            h3('Daily Price'), 
                            div(class='container',
                                p(strong(paste0('Current time (', zone, '):')),
                                  textOutput('currentTime3')), 
                                br(), 
                                p(strong('Daily Chart:'),
                                  highchartOutput('dailyPlot')))))), 
        
        tabPanel('Punter', 
                 tabsetPanel(
                   tabPanel('Trading', 
                            h3('Transaction'), 
                            br(), 
                            p('Algorithmic measurement will auto place order at ', 
                              'predicted price and also sell at predicted price ', 
                              'within 1 day. System will using closing price as ', 
                              'settlement price if the later predicted price was ', 
                              'not occurred. Only value bet be placed.'), 
                            tags$hr(), 
                            h4('Forecasted Daily Price'), 
                            formattableOutput('fcastPPr'),
                            br(), 
                            p('Currently only 6 digits count from left-hand side for trading.'), 
                            tableOutput('transc')), 
                   tabPanel('Profit and Loss', 
                            h3('Profit and Loss'), 
                            p('Below graph shows the return of investment.')
                   ))), 
        tabPanel('Banker', 
                 h3('Latest Price'), 
                 p('By refer to the idea from', 
                   HTML("<a href='https://www.binary.com/en/trading.html?currency=USD&market=forex&underlying=frxAUDJPY&formname=risefall&date_start=now&duration_amount=1&duration_units=d&amount=10&amount_type=payout&expiry_type=duration'>binary.com,</a>"), 
                   'I tried to create this', strong('Banker'), 'page. The daily data is getting from ', 
                   HTML("<a href='https://finance.yahoo.com/'>Yahoo! finance</a>")), 
                 htmlOutput('EURUSDlstPrice'), 
                 htmlOutput('USDJPYlstPrice'), 
                 htmlOutput('GBPUSDlstPrice'), 
                 htmlOutput('USDCHFlstPrice'), 
                 htmlOutput('USDCADlstPrice'), 
                 htmlOutput('AUDUSDlstPrice'), 
                 br(), 
                 p('You can either buy or sell at the below forecast closing price for next trading day. Kindly refer ', 
                   'to the web application ', 
                   HTML("<a href='https://beta.rstudioconnect.com/content/3073/'>financial betting</a>"), 
                   ' for more information.'), 
                 tags$hr(), 
                 h4('Forecasted Daily Price'), 
                 p('Below forecasted price will be automatically calculcate 12AM every weekday.'), 
                 formattableOutput('fcastBPr')), 
        
        tabPanel('Appendix', 
                 tabsetPanel(
                   tabPanel('Statistics', 
                            tabsetPanel(
                              tabPanel('Statistics',  
                                       h3('Statistical Modelling'), 
                                       htmlOutput('video'), 
                                       p('As I tried to build couples of univariate models and concludes that ', 
                                         HTML("<a href='https://vlab.stern.nyu.edu/doc/3?topic=mdls'>GJR-GARCH Model</a>"), 
                                         'is the best fit model. You are feel free to browse over ', 
                                         tags$ul(
                                           tags$li(HTML("<a href='https://englianhu.github.io/2017/09/binary-forex-trading-Q1.html'>binary.com Interview Question I</a>")), 
                                           tags$li(HTML("<a href='http://rpubs.com/englianhu/316133'>binary.com Interview Question I (Extention)</a>")), 
                                           tags$li(HTML("<a href='https://beta.rstudioconnect.com/content/3073/'>Q1App</a>")), 
                                           tags$li(HTML("<a href='https://beta.rstudioconnect.com/content/2367/'>ShinyApp</a>"), 
                                                   '(App for 3 Questions : Blooper)'), 
                                           tags$li(HTML("<a href='https://beta.rstudioconnect.com/content/3775/'>testRealTimeTransc</a>"))), 
                                         'for the research study which compare the accuracy and the return of investment of various statistical models. '), 
                                       p('You are feel free to surf over', 
                                         HTML("<a href='https://github.com/englianhu/binary.com-interview-question'>binary.com Interview Question (GitHub Source Codes)</a>"), 
                                         ' to get the source codes as well as some research papers on the quantitative trading.'), 
                                       
                                       ##https://www.zybuluo.com/knight/note/96093
                                       p('Below is the equation for the model.', 
                                         withMathJax(
                                           helpText('$$\\delta_{t}^{2} = \\omega + (\\alpha + \\gamma I_{t-1}) \\varepsilon_{t-1}^{2} + \\beta \\sigma_{t-1}^{2}$$')), 
                                         'where'), 
                                       p(withMathJax(
                                         helpText('$$I_{t-1}=
                                       \\begin{cases}
                                       0& \\text{if } r_{t-1} \\leq \\mu\\\\
                                       1& \\text{if } r_{t-1} > \\mu
                                       \\end{cases}$$'))), 
                                       p('The daily data for calculation is getting from ', 
                                         HTML("<a href='https://finance.yahoo.com/'>Yahoo! finance</a>"), 
                                         ' while the real-time price to staking and settlement is getting from ', 
                                         HTML("<a href='https://www.truefx.com/'>TrueFX.com.</a>"), 
                                         'Therefore there has no any guarantee of profit and also accuracy of price dataset.')), 
                              #tabPanel('ShinyApp', 
                              #         tags$iframe(src = 'https://beta.rstudioconnect.com/content/2367/', height = 800, width = '100%', frameborder = 0)), 
                              tabPanel('Q1', 
                                       tags$iframe(src = 'https://englianhu.github.io/2017/09/binary-forex-trading-Q1.html', height = 800, width = '100%', frameborder = 0)), 
                              #tabPanel('Q1App', 
                              #         tags$iframe(src = 'https://beta.rstudioconnect.com/content/3073/', height = 800, width = '100%', frameborder = 0)), 
                              tabPanel('Q1E', 
                                       tags$iframe(src = 'http://rpubs.com/englianhu/316133', height = 800, width = '100%', frameborder = 0))#, 
                              #tabPanel('Real-Time App', 
                              #         tags$iframe(src = 'https://beta.rstudioconnect.com/content/3775/', height = 800, width = '100%', frameborder = 0))
                              )), 
                   tabPanel('Reference', 
                            h3('Future Works'), 
                            p('This application is an algorithmic trading in daily ', 
                              'forex market. The orders will be placed everyday at ', 
                              '12:00AM (GMT). The closed transaction orders will also ', 
                              'falling on 12:00AM (GMT) everyday. For high frequency ', 
                              'trading (which includes 1 minute, 5 minutes, 10 minutes, ', 
                              '15 minutes etc.), I put it as future research...', 
                              tags$ul(
                                tags$li(HTML("<a href='https://blog.testproject.io/2016/12/22/open-source-test-automation-tools-for-desktop-applications/'>8 Open Source Test Automation Tools for Desktop Applications</a>")), 
                                tags$li(HTML("<a href='https://github.com/scibrokes/real-time-fxcm'>Real Time FXCM</a>")), 
                                tags$li(HTML("<a href='https://finance.yahoo.com/'>Yahoo! finance</a>")), 
                                tags$li(HTML("<a href='https://www.fxcmapps.com/apps/basic-historical-data-downloader/'>Basic Historical Data Downloader</a>")))), 
                            p('Kindly browse over', HTML("<a href='https://github.com/scibrokes/real-time-fxcm'>Real Time FXCM</a>"), 'for more information.'), 
                            br(), 
                            h3('Reference'), 
                            p('01. ', HTML("<a href='http://matchodds.org/ords/f?p=101:1'>MatchOdds.org</a>"), 
                              tags$a(href='https://github.com/scibrokes/owner', target='_blank', 
                                     tags$img(height = '20px', alt='hot', #align='right', 
                                              src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/hot.jpg'))), 
                            p('02. ', HTML("<a href='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Successful%20Algorithmic%20Trading.pdf'>A Step-by-Step Guide to Quantitative Strategies - SUCCESSFUL ALGORITHMIC TRADING</a>"), 
                              tags$a(href='https://github.com/scibrokes/owner', target='_blank', 
                                     tags$img(height = '20px', alt='hot', #align='right', 
                                              src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/hot.jpg')))), 
                   tabPanel('Author', 
                            h3('Author'), 
                            tags$iframe(src = 'https://beta.rstudioconnect.com/content/3091/ryo-eng.html', height = 800, width = '100%', frameborder = 0)))))), 
    br(), 
    p('Powered by - Copyright® Intellectual Property Rights of ', 
      tags$a(href='http://www.scibrokes.com', target = '_blank', 
             tags$img(height = '20px', alt = 'scibrokes', #align='right', 
                      src='https://raw.githubusercontent.com/scibrokes/betting-strategy-and-model-validation/master/regressionApps/oda-army.jpg')), 
      HTML("<a href='http://www.scibrokes.com'>Scibrokes®</a>"))
        )))






# === Shiny Server ===============================================
#server <- shinyServer(function(input, output, session) {
server <- function(input, output, session) {
  
  output$currentTime <- renderText({
    # Forces invalidation in 1000 milliseconds
    invalidateLater(1000, session)
    as.character(now('Asia/Tokyo'))
  })
  
  fetchData <- reactive({
    if(!input$pause)
      invalidateLater(750)
    qtf <- QueryTrueFX() %>% mutate(TimeStamp = as.character(TimeStamp)) %>% 
      dplyr::rename(`TimeStamp (GMT)` = TimeStamp) %>% 
      filter(Symbol %in% c('EUR/USD', 'USD/JPY', 'GBP/USD', 'USD/CHF', 
                           'USD/CAD', 'AUD/USD'))
    ## http://webrates.truefx.com/rates/connect.html
    qtf <<- qtf[, c(6, 1:3, 5:4)]
    return(qtf)
  })
  
  output$transc <- renderTable({
    
    invalidateLater(750)
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
      saveRDS(transc.buy, paste0('data/buy.', now('GMT'), '.rds'))
    }
    if(Hi == rx$Ask.Price){
      transc.sell <- tail(fxHL, 1) %>% 
        dplyr::select(ForecastDate.GMT, Currency.Hi) %>% 
        mutate(Currency.Hi = round(Currency.Hi, 3))
      saveRDS(transc.sell, paste0('data/sell.', now('GMT'), '.rds'))
    }
    
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
  
  ## ----------- Start Tab Real Time Intraday Graph Server ----------------------
  output$currentTime2 <- renderText({
    # Forces invalidation in 1000 milliseconds
    invalidateLater(1000, session)
    as.character(now('Asia/Tokyo'))
  })
  
  # Function to get new observations
  get_new_data <- function(){
    if(!(weekdays(today('GMT'))) %in% c('Saturday', 'Sunday')) {
      
      ## https://finance.yahoo.com/quote/AUDUSD=X?p=AUDUSD=X
      ## Above link prove that https://finance.yahoo.com using GMT time zone.  
      for(i in seq(fx)) {
        assign(fx[i], suppressWarnings(
          getSymbols(fx[i], from = (today('GMT') - 1) %m-% years(1), 
                     to = (today('GMT') - 1), auto.assign = FALSE)))
      }
      rm(i)
      
      ## Due to duplicated date but various price, here I forced to filter the dataset.
      `EURUSD=X` <<- `EURUSD=X`[index(`EURUSD=X`)==unique(index(`EURUSD=X`)), ]
      `JPY=X`    <<- `JPY=X`[index(`JPY=X`)==unique(index(`JPY=X`)), ]
      `GBPUSD=X` <<- `GBPUSD=X`[index(`GBPUSD=X`)==unique(index(`GBPUSD=X`)), ]
      `CHF=X`    <<- `CHF=X`[index(`CHF=X`)==unique(index(`CHF=X`)), ]
      `CAD=X`    <<- `CAD=X`[index(`CAD=X`)==unique(index(`CAD=X`)), ]
      `AUDUSD=X` <<- `AUDUSD=X`[index(`AUDUSD=X`)==unique(index(`AUDUSD=X`)), ]
      
      data <- getSymbols('JPY=X', from = today('GMT'), 
                         src = 'yahoo', auto.assign = FALSE) %>% Cl
      names(data) <- 'USDJPY'
      index(data) <- now('GMT')
    }
    return(data)
  }
  
  # Initialize realData
  realData <<- get_new_data() #ldply(1:60, get_new_data()) %>% tbl_df
  
  
  # Function to update realData
  update_data <- function(){
    realData <<- rbind(realData, get_new_data())
    
    if(nrow(realData) > 60) {
      #'@ if(length(realData) > 60) {
      realData <<- realData[((nrow(realData) - 59):nrow(realData)),]
      #'@ realData <<- tail(realData, 60)
      } else {
        realData <<- realData
      }
    }
  
  # Plot the 60 most recent values
  output$realPlot <- renderPlot({
    #'@ print("Render")
    invalidateLater(1000, session)
    update_data()
    #'@ print(realData)
    
    ggplot() + geom_line(aes(x = index(realData), y = coredata(realData)), 
                         colour = 'blue') + xlab('Time [Seconds]') + ylab('USD / JPY')
  })
  ## ----------- End Tab Real Time Intraday Graph Server ----------------------
  
  ## ----------- Start Tab Dynamic Daily Chart Server ----------------------
  output$currentTime3 <- renderText({
    # Forces invalidation in 1000 milliseconds
    invalidateLater(1000, session)
    as.character(now('Asia/Tokyo'))
  })
  
  output$dailyPlot <- renderHighchart({
    JPY <- `JPY=X`
    JPY <- na.omit(JPY)
    JPY <- adjustOHLC(JPY)
    
    JPY.SMA.10 <- SMA(Cl(JPY), n = 5)
    JPY.SMA.200 <- SMA(Cl(JPY), n = 100)
    JPY.RSI.14 <- RSI(Cl(JPY))
    JPY.RSI.SellLevel <- xts(rep(70, NROW(JPY)), index(JPY))
    JPY.RSI.BuyLevel <- xts(rep(30, NROW(JPY)), index(JPY))
    
    highchart(type = "stock") %>% 
      hc_title(text = 'FOREX : USD / JPY (DATE [GMT])') %>% 
      hc_subtitle(text = paste0('Candle stick chart with initial stock price : ', 
                                first(Op(JPY)))) %>% 
      hc_yAxis_multiples(
        create_yaxis(3, height = c(2, 1, 1), turnopposite = TRUE)
      ) %>% 
      # series :D
      hc_add_series(JPY, yAxis = 0, name = 'JPY') %>% 
      hc_add_series(JPY.SMA.10, yAxis = 0, name = 'Fast MA') %>% 
      hc_add_series(JPY.SMA.200, yAxis = 0, name = 'Slow MA') %>% 
      #'@ hc_add_series(JPY$`JPY=X.Volume`, color = 'gray', yAxis = 1, name = 'Volume', type = 'column') %>% 
      hc_add_series(JPY.RSI.14, yAxis = 2, name = 'Osciallator', color = hex_to_rgba('green', 0.7)) %>%
      hc_add_series(JPY.RSI.SellLevel, color = hex_to_rgba('red', 0.7),
                    yAxis = 2, name = 'Sell level') %>% 
      hc_add_series(JPY.RSI.BuyLevel, color = hex_to_rgba('blue', 0.7),
                    yAxis = 2, name = 'Buy level') 
  })
  ## ----------- End Tab Dynamic Daily Chart Server ----------------------
  
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
  
  fcstBankerData <- reactive({
    ## Change when the "update" button is pressed...
    #'@ input$curr
    
    ## ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing algorithmic forecast...")
        fxCl <- forecastData()
        names(fxCl) <- str_replace_all(names(fxCl), '\\.x$', '.Cl')
      })
    })
    if(!dir.exists('data')) dir.create('data')
    if(!file.exists(paste0('data/fcstBankerGMT', today('GMT'), '.rds'))){
      saveRDS(fxCl, paste0('data/fcstBankerGMT', today('GMT'), '.rds')) }
    
    return(fxCl)
  })
  
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
  
  output$EURUSDlstPrice <- renderText({
    paste('The latest closing price of', '<font color=\"#FF0000\"><b>', 
          'EUR/USD</b></font>', 'on <font color=\"#FF0000\"><b>', 
          index(tail(`EURUSD=X`, 1)), '</b></font>', '(GMT) is', '<font color=\"#FF0000\"><b>', 
          coredata(tail(Cl(`EURUSD=X`), 1)), '</b></font>.')
  })
  
  output$USDJPYlstPrice <- renderText({
    paste('The latest closing price of', '<font color=\"#FF0000\"><b>', 
          'USD/JPY</b></font>', 'on <font color=\"#FF0000\"><b>', 
          index(tail(`JPY=X`, 1)), '</b></font>', '(GMT) is', '<font color=\"#FF0000\"><b>', 
          coredata(tail(Cl(`JPY=X`), 1)), '</b></font>.')
  })
  
  output$GBPUSDlstPrice <- renderText({
    paste('The latest closing price of', '<font color=\"#FF0000\"><b>', 
          'GBP/USD</b></font>', 'on <font color=\"#FF0000\"><b>', 
          index(tail(`GBPUSD=X`, 1)), '</b></font>', '(GMT) is', '<font color=\"#FF0000\"><b>', 
          coredata(tail(Cl(`GBPUSD=X`), 1)), '</b></font>.')
  })
  
  output$USDCHFlstPrice <- renderText({
    paste('The latest closing price of', '<font color=\"#FF0000\"><b>', 
          'USD/CHF</b></font>', 'on <font color=\"#FF0000\"><b>', 
          index(tail(`CHF=X`, 1)), '</b></font>', '(GMT) is', '<font color=\"#FF0000\"><b>', 
          coredata(tail(Cl(`CHF=X`), 1)), '</b></font>.')
  })
  
  output$USDCADlstPrice <- renderText({
    paste('The latest closing price of', '<font color=\"#FF0000\"><b>', 
          'USD/CAD</b></font>', 'on <font color=\"#FF0000\"><b>', 
          index(tail(`CAD=X`, 1)), '</b></font>', '(GMT) is', '<font color=\"#FF0000\"><b>', 
          coredata(tail(Cl(`CAD=X`), 1)), '</b></font>.')
  })
  
  output$AUDUSDlstPrice <- renderText({
    paste('The latest closing price of', '<font color=\"#FF0000\"><b>', 
          'AUD/USD</b></font>', 'on <font color=\"#FF0000\"><b>', 
          index(tail(`AUDUSD=X`, 1)), '</b></font>', '(GMT) is', '<font color=\"#FF0000\"><b>', 
          coredata(tail(Cl(`AUDUSD=X`), 1)), '</b></font>.')
  })
  
  output$fcastBPr <- renderFormattable({
    
    #'@ validate(
    #'@   need(exists(fcstBankerData()), 'Scrapping forex data error, kindly refresh the webpage.')
    #'@ )
    #fxD <- fcstBankerData() %>% mutate(
    #  Currency = ifelse(.id == 'USDJPY', round(Currency, 3), round(Currency, 6)))
    
    fxD <- refreshBanker()
    fxD %<>% mutate(
      Currency = ifelse(Symbol == 'USD/JPY', round(Currency, 3), round(Currency, 6)))
    
    data.frame(fxD, Buy = 'BUY', Sell = 'SELL') %>% 
      formattable(list(
        Buy = formatter('span', style = ~ style(color = ifelse(
          Buy == 'SELL', 'red', 'green')), 
          ~ icontext(ifelse(Buy == 'SELL', 'arrow-down', 'arrow-up'), Buy)), 
        Sell = formatter('span', style = ~ style(color = ifelse(
          Sell == 'SELL', 'red', 'green')), 
          ~ icontext(ifelse(Sell == 'SELL', 'arrow-down', 'arrow-up'), Sell))))
  })
  
  # ----------------------- Transaction ---------------------------------
  refreshPunter <- reactive({
    
    line <- fetchData()
    
    if(file.exists(paste0('data/fcstPunterGMT', today('GMT'), '.rds'))) {
      fcPR <- ldply(dir('data', 
                        pattern = paste0('fcstPunterGMT', today('GMT'))), function(x){
                          readRDS(paste0('data/', x)) })
      
      ## filter and only pick USDJPY
      fcPR %<>% filter(.id == 'USDJPY')
      
    } else {
      
      repeat{
        
        #'@ startTime <- now('GMT')
        startTime <- today('GMT')
        
        validate(need(weekdays(today('GMT')) %in% wd, 'Today has no data.'))
        
        ## https://finance.yahoo.com/quote/AUDUSD=X?p=AUDUSD=X
        ## Above link prove that https://finance.yahoo.com using GMT time zone.  
        if(weekdays(today('GMT')) %in% wd) {
          prd <- ifelse(weekdays(today('GMT')) %in% wd[1:4], 1, 3)
          
          for(i in seq(fx)) {
            assign(fx[i], suppressWarnings(
              getSymbols(fx[i], from = (today('GMT') - prd) %m-% years(1), 
                         to = (today('GMT') - prd), auto.assign = FALSE))) }
          rm(i) }
        
        fcPR <- fcstPunterData()
        #'@ print(as.character(now('GMT')))
        #'@ print(fcPR)
        if(exists('fcPR')) break
        
        ## scheduled sleepTime as 24 hours to start next task
        sleepTime <- startTime + 24*60*60 - startTime
        if (sleepTime > 0)
          Sys.sleep(sleepTime) }
      
      ## filter and only pick USDJPY
      fcPR %<>% filter(.id == 'USDJPY')
    }
    
    #'@ invalidateLater(1000, session)
    rx <- line %>% filter(Symbol == 'USD/JPY') %>% 
      mutate(
        Bid.Price = round(Bid.Price, 3), 
        Ask.Price = round(Ask.Price, 3), 
        fc.High = round(fcPR$Currency.Hi, 3), 
        fc.Low = round(fcPR$Currency.Lo, 3)) %>% 
      dplyr::select(`TimeStamp (GMT)`, Bid.Price, Ask.Price, 
                    fc.High, fc.Low)
    
    if(rx$fc.Low == rx$Bid.Price){
      tr.buy <- rx %>% mutate(Price = fc.Low, Transaction = 'Buy') %>% 
        dplyr::select(`TimeStamp (GMT)`, Price, Transaction)
      saveRDS(tr.buy, paste0('data/buy.', now('GMT'), '.rds')) }
    
    if(rx$fc.High == rx$Ask.Price){
      tr.sell <- rx %>% mutate(Price = fc.High, Transaction = 'Sell') %>% 
        dplyr::select(`TimeStamp (GMT)`, Price, Transaction)
      saveRDS(tr.sell, paste0('data/sell.', now('GMT'), '.rds')) }
    
    return(rx)
  })
  
  refreshBanker <- reactive({
    
    line <- fetchData()
    
    if(file.exists(paste0('data/fcstBankerGMT', today('GMT'), '.rds'))) {
      fcBR <- ldply(
        dir('data', pattern = paste0('fcstBankerGMT', today('GMT'))), function(x){
          readRDS(paste0('data/', x)) })
      
    } else {
      
      repeat{
        ## startTime <- now('GMT')
        startTime <- today('GMT')
        validate(need(weekdays(today('GMT')) %in% wd, 'Today has no data.'))
        
        ## https://finance.yahoo.com/quote/AUDUSD=X?p=AUDUSD=X
        ## Above link prove that https://finance.yahoo.com using GMT time zone.  
        if(weekdays(today('GMT')) %in% wd) {
          prd <- ifelse(weekdays(today('GMT')) == wd[5], 3, 1)
          
          for(i in seq(fx)) {
            assign(fx[i], suppressWarnings(
              getSymbols(fx[i], from = (today('GMT') - prd) %m-% years(1), 
                         to = (today('GMT') - prd), auto.assign = FALSE))) }
          rm(i) }
        
        fcBR <- fcstBankerData()
        ## print(as.character(now('GMT')))
        ## print(fcBR)
        if(exists('fcBR')) break
        
        ## scheduled sleepTime as 24 hours to start next task
        sleepTime <- startTime + 24*60*60 - startTime
        if(sleepTime > 0)
          Sys.sleep(sleepTime) }}
    
    rx <- cbind(line, fcBR[-1])
    return(rx)
  })
  
  output$fcastPPr <- renderFormattable({
    
    #'@ validate(
    #'@   need(exists(fcstPunterData()), 'Scrapping forex data error, kindly refresh the webpage.')
    #'@ )
    fxD <- fcstPunterData()
    
    data.frame(fxD, Sell = 'BUY', Buy = 'SELL') %>% 
      formattable(list(
        Sell = formatter('span', style = ~ style(color = ifelse(
          Sell == 'BUY', 'green', 'red')), 
          ~ icontext(ifelse(Buy == 'SELL', 'arrow-down', 'arrow-up'), Buy)), 
        Buy = formatter('span', style = ~ style(color = ifelse(
          Buy == 'SELL', 'red', 'green')), 
          ~ icontext(ifelse(Sell == 'SELL', 'arrow-down', 'arrow-up'), Sell))))
  })
  
  output$video <- renderUI(tags$iframe(src = 'https://www.youtube.com/embed/VWAU1r6rPvg'))
  
  ## Real-time graph in shiny.
  ## https://stackoverflow.com/questions/42109370/invalidatelater-stopped-in-r-shiny-app
  
  ## https://shiny.rstudio.com/articles/reconnecting.html
  ## Set this to "force" instead of TRUE for testing locally (without Shiny Server)
  #session$allowReconnect(TRUE)
  
  }#)

# Run the application
shinyApp(ui = ui, server = server)
#'@ shiny::runApp('Q2', display.mode = 'showcase')

