
## Testing...

#rm(list = ls())
library('shiny')
library('quantmod')
library('ggplot2')
library('lubridate')

t <- now('Asia/Tokyo')
Sys.setenv(TZ="Asia/Tokyo")
zones <- attr(as.POSIXlt(Sys.time()), 'tzone')
zone <- if (zones[[1]] == '') {
  paste(zones[-1], collapse = '/')
} else zones[[1]]

JPY <- getSymbols('JPY=X', from = today('Asia/Tokyo') - days(1), auto.assign = FALSE)
JPY <- na.omit(JPY)
JPY <- adjustOHLC(JPY)
n <- nrow(JPY) # some evaluation

## Need to randomize the JPY Closing value by refer to TFX.
## https://github.com/cran/TFX/blob/master/R/TFX.R
## https://stackoverflow.com/questions/42109370/invalidatelater-stopped-in-r-shiny-app
## https://stackoverflow.com/questions/33480302/creating-a-shiny-app-with-real-time-data
queryFX <- function(session) {
  session <- new.env()
  
  JPY <- getSymbols('JPY=X', from = today('Asia/Tokyo'), auto.assign = FALSE)
  JPY <- JPY %>% Cl %>% na.omit
  n <- nrow(JPY) # some evaluation
  session$JPY <- JPY
  session$last.used <- Sys.time()
  names(JPY) <- 'USDJPY.Close'
  
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
  
  output$plot0 <- renderPlot({  # Signal realtime View
    ggplot() + geom_line(aes(x = mydata()[,1], y = mydata()[,2]), colour = "blue") +      
      xlab("Time [s]") + ylab("Channel") # normal ggplot :-)
  })
})

shinyApp(ui=ui, server=server)