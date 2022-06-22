require('shiny')
require('rdrop2')
require('magrittr')
require('plyr')
require('dplyr')
require('DT')
require('quantmod')
require('TFX')
require('highcharter')
require('forecast')
require('rugarch')
require('lubridate')
require('pryr')

#'@ drop_auth()
## email : scibrokes_demo@gmail.com
## pass : trader888
#
# https://github.com/karthik/rdrop2
#
#'@ token <- drop_auth()
#'@ saveRDS(token, "droptoken.rds")
# Upload droptoken to your server
# ******** WARNING ********
# Losing this file will give anyone 
# complete control of your Dropbox account
# You can then revoke the rdrop2 app from your
# dropbox account and start over.
# ******** WARNING ********
# read it back with readRDS
#'@ token <- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
#'@ drop_acc(dtoken = token)
#'@ token <<- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
#'@ drop_acc(dtoken = token)


currency <<- 'JPY=X'
#'@ Sys.setenv(TZ = 'America/Chicago')
zones <- attr(as.POSIXlt(now('America/Chicago')), 'tzone')
zone <- ifelse(zones[[1]] == '', paste(zones[-1], collapse = '/'), zones[[1]])
tday <<- today('America/Chicago')

ui <- shinyServer(fluidPage(
  div(class='container',
      p(strong(paste0('Current time (', zone, '):')),
        textOutput('currentTime')
      ),
  dataTableOutput('second_column'),
  actionButton('refresh', 'Refresh')), 
  tags$hr(),
  highchartOutput("first_column")
))

server <- shinyServer(function(input, output, session){
  
  output$currentTime <- renderText({
    # Forces invalidation in 1000 milliseconds
    invalidateLater(1000, session)
    as.character(now('America/Chicago'))
  })
  
  calC <- function(currency, dateID, choose.Pr = subs(choose.Pr)) {
    ## dateID must be in 'America/Chicago'
    mbase = getSymbols(currency, from = (dateID - 1) %m-% years(1), 
                       to = (dateID - 1), auto.assign = FALSE)
    mbase %<>% choose.Pr %>% na.omit
    nm <- str_replace_all(currency, '=X', '')
    nm <- ifelse(nchar(nm) == 3, paste0('USD', nm), nm)
    names(mbase) <- str_replace_all(names(mbase), '^.*\\.', paste0(nm, '.'))
    
    #ARMA Modeling minimum AIC value of `p,d,q`
    aa <- auto.arima(mbase)
    armaOrder = arimaorder(aa)
    
    ## Set arma order for `p, d, q` for GARCH model.
    #'@ https://stats.stackexchange.com/quAmerica/Chicagoions/73351/how-does-one-specify-arima-p-d-q-in-ugarchspec-for-ugarchfit-in-rugarch
    spec = ugarchspec(
      variance.model = list(
        model = 'gjrGARCH', garchOrder = c(1, 1), 
        submodel = NULL, external.regressors = NULL, 
        variance.targeting = FALSE), 
      mean.model = list(
        armaOrder = armaOrder[c(1, 3)], 
        include.mean = TRUE, archm = FALSE, 
        archpow = 1, arfima = TRUE, 
        external.regressors = NULL, 
        archex = FALSE), 
      fixed.pars = list(arfima = armaOrder[2]), 
      distribution.model = 'snorm')
    
    fit = ugarchfit(spec, mbase, solver = 'hybrid')
    fc = ugarchforecast(fit, n.ahead = 1)
    
    T1 = attributes(fc)$forecast$seriesFor
    if(as.character(subs(choose.Pr)) == 'Op') {
      pc = 'Open'
    } else if(as.character(subs(choose.Pr)) == 'Hi') {
      pc = 'High'
    } else if(as.character(subs(choose.Pr)) == 'Lo') {
      pc = 'Low'
    } else if(as.character(subs(choose.Pr)) == 'Cl') {
      pc = 'Close'
    } else {
      stop('choose.Pr = Op or Hi or Lo or Cl.')
    }
    dimnames(T1) <- list(dimnames(T1)[[2]], paste0(nm, '.T1.', pc))
    
    tmp = as.xts(data.frame(tail(mbase, 1), T1))
    return(tmp)
  }
  
  # Function to get new observations
  get_new_forecast <- function(){
    dt <- cbind(calC(currency, today('America/Chicago'), Hi), 
          calC(currency, today('America/Chicago'), Lo))
    data.frame(dt, savedTime = paste(now('America/Chicago'), 'America/Chicago'))
    #'@ QueryTrueFX()[2, c(2, 3, 6)]
    #'@ > QueryTrueFX()[2, c(2, 3, 6)]
    #'@ Bid.Price Ask.Price           TimAmerica/Chicagoamp
    #'@ 2   113.501   113.505 2017-11-10 21:45:00
  }
  
  # Initialize my_forecast
  if(!file.exists('my_forecast.rds')) {
    my_forecast <<- get_new_forecast()
    saveRDS(my_forecast, 'my_forecast.rds')
    
  } else {
    my_forecast <<- readRDS('my_forecast.rds')
  }
  
  # Function to update my_forecast
  update_data <- function(){
    my_forecast <<- rbind(get_new_forecast(), my_forecast) %>% unique
    saveRDS(my_forecast, 'my_forecast.rds')
    tday <<- today('America/Chicago')
  }
  
  #'@ cal <- reactive({
    ## Change when the "refresh" button is pressed...
    #if(now('America/Chicago') == ymd_hms('2017-11-15 13:33:00', tz = 'America/Chicago'))
    #'@ input$refresh
  #'@   invalidateLater(1000, session)
  #'@   if(today('America/Chicago') == tday) update_data()
    
    ## ...but not for anything else
  #'@   isolate({
  #'@     withProgress({
  #'@       setProgress(message = "Processing algorithmic forecast...")
  #'@       update_data()
  #'@     })
  #'@   })
  #'@ })
  
  # Plot the 30 most recent values
  output$first_column <- renderHighchart({
    #print("Render")
    invalidateLater(1000, session)
    if(today('America/Chicago') != tday) update_data()
    
    #print(my_forecast)
    if(nrow(my_forecast) > 60) {
      hc <- highchart(type = 'stock') %>% 
        hc_title(text = 'Forecast Price') %>% 
        hc_subtitle(text = 'High Low Price') %>% 
        hc_add_series(my_forecast[1:60, 2]) %>% 
        hc_add_series(my_forecast[1:60, 4])
      hc
    } else {
      hc <- highchart(type = 'stock') %>% 
        hc_title(text = 'Forecast Price') %>% 
        hc_subtitle(text = 'High Low Price') %>% 
        hc_add_series(my_forecast[, 2]) %>% 
        hc_add_series(my_forecast[, 4])
      hc
    }
  })
  
  terms <- reactive({
    input$refresh
    readRDS('my_forecast.rds')# %>% data.frame
  })
  
  output$second_column <- renderDataTable({
    terms() %>% datatable(
      caption = "Table : USD/JPY", 
      escape = FALSE, filter = "top", #rownames = FALSE, 
      extensions = list("ColReorder" = NULL, "RowReorder" = NULL, 
                        "Buttons" = NULL, "Responsive" = NULL), 
      options = list(dom = 'BRrltpi', scrollX = TRUE, #autoWidth = TRUE, 
                     lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')), 
                     ColReorder = TRUE, rowReorder = TRUE, 
                     buttons = list('copy', 'print', 
                                    list(extend = 'collection', 
                                         buttons = c('csv', 'excel', 'pdf'), 
                                         text = 'Download'), I('colvis'))))
  })
})

shinyApp(ui, server)