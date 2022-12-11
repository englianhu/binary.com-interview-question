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

# === Data =========================================================
Sys.setenv(TZ = 'GMT')
zones <- attr(as.POSIXlt(now('GMT')), 'tzone')
zone <- ifelse(zones[[1]] == '', paste(zones[-1], collapse = '/'), zones[[1]])

fx <<- c('EURUSD=X', 'JPY=X', 'GBPUSD=X', 'CHF=X', 'CAD=X', 'AUDUSD=X')
cur <<- c('EUR/USD', 'USD/JPY', 'GBP/USD', 'USD/CHF', 'USD/CAD', 'AUD/USD')
wd <<- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

# === Function ======================================================
timeR <- function(tm = 5){
    repeat{
        startTime <- now('GMT')
        
        fcPR <- data.frame(Y = sample(1:5, 1))
        #'@ print(as.character(now('GMT')))
        #'@ print(fcPR)
        if(exists('fcPR')) break
        
        ## scheduled sleepTime as 24 hours to start next task
        #sleepTime <- startTime + 24*60*60 - startTime
        sleepTime <- startTime + tm*60 - startTime
        if (sleepTime > 0)
            Sys.sleep(sleepTime) }
    return(fcPR)
    }





# === Shiny UI =====================================================
ui <- fluidPage(
    textOutput('currentTime')#, 
    #tableOutput('transc'), 
    #formattableOutput('fxdata')
    )
    
server <- function(input, output, session) {
    
    output$currentTime <- renderText({
        # Forces invalidation in 1000 milliseconds
        invalidateLater(1000, session)
        as.character(now('GMT'))
    })
    
    fetchData <- reactive({
        if(!input$pause)
            invalidateLater(750)
        sample(1:5, 20, replace = TRUE) %>% matrix(nc = 4) %>% data.frame
    })
    
    refresh <- reactive({
        line <- fetchData()
        
        if(file.exists(paste0('data/fcstPunterGMT', today('GMT'), '.rds'))) {
            fcPR <- ldply(dir('data', 
                              pattern = paste0('fcstPunterGMT', today('GMT'))), function(x){
                                  readRDS(paste0('data/', x)) })
            
        } else {
            fcPR <- timeR()
        }
        fcPR
    })
    
    ## https://shiny.rstudio.com/articles/reconnecting.html
    ## # Set this to "force" instead of TRUE for testing locally (without Shiny Server)
    #session$allowReconnect(TRUE)
    #session$allowReconnect('force')
    }


shinyApp(server, ui)


