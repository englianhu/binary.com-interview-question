
## Testing...

#rm(list = ls())
library('shiny')
library('quantmod')
library('highcharter')
library('lubridate')

t <- now('Asia/Tokyo')
Sys.setenv(TZ="Asia/Tokyo")
zones <- attr(as.POSIXlt(Sys.time()), 'tzone')
zone <- if (zones[[1]] == '') {
  paste(zones[-1], collapse = '/')
} else zones[[1]]

JPY <- getSymbols('JPY=X', from = (today('Asia/Tokyo') - days(1)) - years(1), auto.assign = FALSE)
JPY <- na.omit(JPY)
JPY <- adjustOHLC(JPY)
n <- nrow(JPY) # some evaluation

## Need to randomize the JPY Closing value by refer to TFX.
## https://github.com/cran/TFX/blob/master/R/TFX.R
## https://stackoverflow.com/questions/42109370/invalidatelater-stopped-in-r-shiny-app
queryFX <- function(session) {
  session <- new.env()
  
  JPY <- getSymbols('JPY=X', from = (today('Asia/Tokyo') - days(1)) - years(1), auto.assign = FALSE)
  JPY <- na.omit(JPY)
  JPY <- adjustOHLC(JPY)
  n <- nrow(JPY) # some evaluation
  session$JPY <- JPY
  session$last.used <- Sys.time()
  
  return(JPY)
}

ui<-  shinyUI(
  bootstrapPage(
    div(class = 'container',
        p(strong(paste0('Current time (', zone, '):')),
          textOutput('currentTime')), 
        br(), 
        p(strong('Real Time USD/JPY Rate:'),
          highchartOutput('plot0')))))

server<- shinyServer(function(input, output,session) {
  
  output$currentTime <- renderText({
    # Forces invalidation in 1000 milliseconds
    invalidateLater(1000, session)
    as.character(Sys.time())
  })
  
  mydata <- reactive({
    invalidateLater(300, session) # updating the plot each 300 miliseconds
    n <- as.integer(difftime(now('Asia/Tokyo'), t, units = 'secs')) + n # updating the new elements which should be visualized
    queryFX()
  })
  
  output$plot0 <- renderHighchart({  # Signal realtime View
    JPY <- mydata()
    JPY.SMA.10 <- SMA(Cl(JPY), n = 5)
    JPY.SMA.200 <- SMA(Cl(JPY), n = 100)
    JPY.RSI.14 <- RSI(Cl(JPY))
    JPY.RSI.SellLevel <- xts(rep(70, NROW(JPY)), index(JPY))
    JPY.RSI.BuyLevel <- xts(rep(30, NROW(JPY)), index(JPY))
    
    highchart(type = 'stock') %>% 
      # create axis :)
      hc_yAxis_multiples(
        create_yaxis(3, height = c(2, 1, 1), turnopposite = TRUE)
      ) %>% 
      # series :D
      hc_add_series(JPY, yAxis = 0, name = 'JPY') %>% 
      hc_add_series(JPY.SMA.10, yAxis = 0, name = 'Fast MA') %>% 
      hc_add_series(JPY.SMA.200, yAxis = 0, name = 'Slow MA') %>% 
      hc_add_series(JPY$`JPY=X.Volume`, color = 'gray', yAxis = 1, name = 'Volume', type = 'column') %>% 
      hc_add_series(JPY.RSI.14, yAxis = 2, name = 'Osciallator', color = hex_to_rgba('green', 0.7)) %>%
      hc_add_series(JPY.RSI.SellLevel, color = hex_to_rgba('red', 0.7),
                    yAxis = 2, name = 'Sell level') %>% 
      hc_add_series(JPY.RSI.BuyLevel, color = hex_to_rgba('blue', 0.7),
                    yAxis = 2, name = 'Buy level') %>% 
      hc_rangeSelector('1 day')
  })
})

shinyApp(ui=ui, server=server)