# === Setting ======================================================
library('shiny')
library('memoise')
library('magrittr')
library('stringr')
library('TFX')
library('quantmod')
library('rugarch')
library('lubridate')
library('formattable')

# === Data =====================================================
Sys.setenv(TZ = 'Asia/Tokyo')
zones <- attr(as.POSIXlt(Sys.time()), 'tzone')
zone <- ifelse(zones[[1]] == '', paste(zones[-1], collapse = '/'), zones[[1]])

# === Function =====================================================
#ARMA Modeling寻找AIC值最小的p,q
armaSearch <- suppressWarnings(function(data, .method = 'CSS-ML'){ 
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
        stop(paste('Kindly choose .method among ', paste0(.methods, collapse = ', '), '!'))
      }
      names(data.arma) <- c('arma', 'mth')
      
      #cat('p =', p, ', q =', q, 'AIC =', data.arma$arma$aic, '\n')
      armacoef <- rbind(armacoef,c(p, q, data.arma$arma$aic))
    }
  }
  
  colnames(armacoef) <- c('p', 'q', 'AIC')
  pos <- which(armacoef$AIC == min(armacoef$AIC))
  cat(paste0('method = \'', data.arma$mth, '\', the min AIC = ', armacoef$AIC[pos], 
             ', p = ', armacoef$p[pos], ', q = ', armacoef$q[pos], '\n'))
  return(armacoef)
})

getFOREX <- memoise(function(currency) {
  getSymbols(currency, from = Sys.Date() %m-% years(1), to = Sys.Date())
  if(currency == 'AUD=X') {
    mbase <- `AUD=X` %>% Cl %>% na.omit; rm(`AUD=X`)
    mbase <- 1/mbase
    names(mbase) %<>% str_replace_all('AUD=X.Close', 'AUD.USD')
    
  } else if(currency == 'EUR=X') {
    mbase <- `EUR=X` %>% Cl %>% na.omit; rm(`EUR=X`)
    mbase <- 1/mbase
    names(mbase) %<>% str_replace_all('EUR=X.Close', 'EUR.USD')
    
  } else if(currency == 'GBP=X') {
    mbase <- `GBP=X` %>% Cl %>% na.omit; rm(`GBP=X`)
    mbase <- 1/mbase
    names(mbase) %<>% str_replace_all('GBP=X.Close', 'GBP.USD')
    
  } else if(currency == 'CHF=X') {
    mbase <- `CHF=X` %>% Cl %>% na.omit; rm(`CHF=X`)
    names(mbase) %<>% str_replace_all('CHF=X.Close', 'USD.CHF')
    
  } else if(currency == 'CAD=X') {
    mbase <- `CAD=X` %>% Cl %>% na.omit; rm(`CAD=X`)
    names(mbase) %<>% str_replace_all('CAD=X.Close', 'USD.CAD')
    
  } else if(currency == 'CNY=X') {
    mbase <- `CNY=X` %>% Cl %>% na.omit; rm(`CNY=X`)
    names(mbase) %<>% str_replace_all('CNY=X.Close', 'USD.CNY')
    
  } else if(currency == 'JPY=X') {
    mbase <- `JPY=X` %>% Cl %>% na.omit; rm(`JPY=X`)
    names(mbase) %<>% str_replace_all('JPY=X.Close', 'USD.JPY')
    
  } else {
    stop('Kindly choose common currencies exchange.')
  }
  return(mbase)
})

# Using "memoise" to automatically cache the results
calC <- memoise(function(currency, ahead) {
  
  mbase = getFOREX(currency)
  
  armaOrder = armaSearch(mbase)
  armaOrder %<>% dplyr::filter(AIC==min(AIC)) %>% .[c('p', 'q')] %>% unlist
  
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
  
  sim = ugarchsim(fit, n.sim = 1000, m.sim = 25, rseed = 1:25)
  
  tmp = list(latestPrice = tail(mbase, 1), forecastPrice = res, 
             sim = sim)
  return(tmp)
})


# === Shiny UI =====================================================
ui <- shinyUI(fluidPage(
  
  titlePanel(
    tags$a(href='http://www.binary.com', target='_blank', 
           tags$img(height = '80px', alt='binary', #align='right', 
                    src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/binary-logo-resize.jpg'))), 
  
  sidebarLayout(
    sidebarPanel(
      selectInput('curr', 'Currency :',
                  choices = c('AUD/USD' = 'AUD=X', 
                              'EUR/USD' = 'EUR=X', 
                              'GBP/USD' = 'GBP=X', 
                              'USD/CHF' = 'CHF=X', 
                              'USD/CAD' = 'CAD=X', 
                              'USD/CNY' = 'CNY=X', 
                              'USD/JPY' = 'JPY=X'), 
                  selected = 'USD/JPY'), 
      sliderInput('ahead', HTML('Forecast ahead (&zeta; in day) :'), 
                  min = 1, max = 7, step = 1, value = 7)),
    
    mainPanel(
      tabsetPanel(
        tabPanel('Board', 
                 h3('Real Time Price'), 
                 div(class='container',
                     p(strong(paste0('Current time (', zone, '):')),
                       textOutput('currentTime')
                     ),
                     p(strong('Latest FX Quotes:'),
                       tableOutput('fxdata'),
                       checkboxInput('pause', 'Pause updates', FALSE))
                 )), 
        tabPanel('Trade', 
                 h3('Latest Price'), 
                 htmlOutput('lastPr'), 
                 br(), 
                 p('You can either buy or sell at the mentioned price.'), 
                 formattableOutput('fcastPr')), 
        tabPanel('Forecast', 
                 h3('Forecast Trend'), 
                 plotOutput('sim')), 
        tabPanel('Appendix', 
                 tabsetPanel(
                   tabPanel('Reference', 
                            h3('Future Works'), 
                            p('For the API and also real-time data visualization, I put it as future research...', 
                              tags$ul(
                                tags$li(HTML("<a href='http://www.techrepublic.com/blog/five-apps/create-real-time-graphs-with-these-five-free-web-based-apps/'>Create real-time graphs with these five free web-based apps</a>")), 
                                tags$li(HTML("<a href='https://www.slideshare.net/rorywinston/streaming-data-in-r'>Streaming Data in R</a>")), 
                                tags$li(HTML("<a href='https://www.quora.com/How-I-can-manage-real-time-data-with-R-programming-and-Tableau'>How I can manage real time data with R programming and Tableau?</a>")), 
                                tags$li(HTML("<a href='https://www.r-bloggers.com/real-time-predictive-analytics-with-big-data-and-r/'>Real-Time Predictive Analytics with Big Data, and R</a>")), 
                                tags$li(HTML("<a href='https://stackoverflow.com/questions/37049634/streaming-data-visualization-in-r'>Streaming data visualization in R</a>")))), 
                            br(), 
                            h3('Reference'), 
                            p('01. ', HTML("<a href='https://www.r-bloggers.com/accessing-apis-from-r-and-a-little-r-programming/'>Accessing APIs from R (and a little R programming)</a>")), 
                            p('02. ', HTML("<a href='https://developers.binary.com/'>Welcome to the Binary.com API</a>")), 
                            p('03. ', HTML("<a href='https://stats.stackexchange.com/questions/6021/r-update-a-graph-dynamically?answertab=votes#tab-top'>R: update a graph dynamically</a>")), 
                            p('04. ', HTML("<a href='https://www.r-bloggers.com/tfx-package/'>TFX Package</a>")), 
                            p('05. ', HTML("<a href='http://rpubs.com/gsee/TFX'>TFX: An R Interface to the TrueFX™ Web API</a>"), 
                              tags$a(href='https://github.com/scibrokes/owner', target='_blank', 
                                     tags$img(height = '20px', alt='hot', #align='right', 
                                              src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/hot.jpg'))), 
                            p('06. ', HTML("<a href='https://gist.github.com/gsee/4122626'>shiny TrueFX quotes</a>"), 
                              tags$a(href='https://github.com/scibrokes/owner', target='_blank', 
                                     tags$img(height = '20px', alt='hot', #align='right', 
                                              src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/hot.jpg')))), 
                   
                   tabPanel('Author', 
                            h3('Author'), 
                            tags$iframe(src = 'https://englianhu.github.io/2016/12/ryo-eng.html', height = 800, width = '100%', frameborder = 0))))))), 
  br(), 
  p('Powered by - Copyright® Intellectual Property Rights of ', 
    tags$a(href='http://www.scibrokes.com', target='_blank', 
           tags$img(height = '20px', alt='scibrokes', #align='right', 
                    src='https://raw.githubusercontent.com/scibrokes/betting-strategy-and-model-validation/master/regressionApps/oda-army.jpg')), 
    HTML("<a href='http://www.scibrokes.com'>Scibrokes®</a>"))))
  






# === Shiny Server ===============================================
# Based on (more) code by Joe Cheng:
  # https://groups.google.com/d/msg/shiny-discuss/NE-LqDAVqQQ/kNdrtC4WxGAJ
  # https://gist.github.com/4044364
  #-------------------------------------------------------------------------------
server <- shinyServer(function(input, output, session) {
  
  output$currentTime <- renderText({
    # Forces invalidation in 1000 milliseconds
    invalidateLater(1000, session)
    as.character(Sys.time())
  })
  
  fetchData <- reactive({
    if (!input$pause)
      invalidateLater(750)
    qtf <- QueryTrueFX()
    qtf$TimeStamp <- as.character(qtf$TimeStamp)
    names(qtf)[6] <- 'TimeStamp (GMT)'
    qtf[, c(6, 1:3, 5:4)]
  })
  
  output$fxdata <- renderTable({
    fetchData()
  }, digits = 5, row.names = FALSE)
  
  
  terms <- reactive({
    ## Change when the "update" button is pressed...
    input$curr
    input$ahead
    ## ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing algorithmic forecast...")
        calC(input$curr, input$ahead)
      })
    })
  })
  
  output$lastPr <- renderText({
    tmp = terms()$latestPrice
    paste('The latest closing price of', '<font color=\"#FF0000\"><b>', 
          names(tmp), '</b></font>', 'on <font color=\"#FF0000\"><b>', 
          index(tmp), '</b></font>', 'is', '<font color=\"#FF0000\"><b>', 
          tmp, '</b></font>')
  })
  
  output$fcastPr <- renderFormattable({
    tmp = terms()
    data.frame(Buy = 'BUY', Price = tmp$forecastPrice, Sell = 'SELL') %>% 
      formattable(list(
        Buy = formatter('span', style = ~ style(color = ifelse(
          Buy == 'SELL', 'red', 'green')), 
          ~ icontext(ifelse(Buy == 'SELL', 'arrow-down', 'arrow-up'), Buy)), 
        Price = color_tile('white', 'darkgolden'), 
        Sell = formatter('span', style = ~ style(color = ifelse(
          Sell == 'SELL', 'red', 'green')), 
          ~ icontext(ifelse(Sell == 'SELL', 'arrow-down', 'arrow-up'), Sell))))
  })
  
  output$sim <- renderPlot({
    tmp = terms()$sim
    plot(tmp, which = 'all', m.sim = 24)
  })
  
  
})

# Run the application 
shinyApp(ui = ui, server = server)
#'@ shiny::runApp('Q2', display.mode = 'showcase')
