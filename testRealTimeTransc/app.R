library('shiny')
library('shinyjs')
library('TFX')
library('cronR')
library('formattable')
library('data.table')
library('DT')
library('plyr')
library('dplyr')
library('magrittr')
library('lubridate')


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel('Test the real-time closing transaction'),
    
    mainPanel(
        p('I created this app to test the real-time transaction matching... ', 
          'Once the bid/ask price match with forecasted price, a transaction ', 
          'will be done.'), 
        tags$hr(),
        h4('Real Time Data'), 
        p('Real Time bid/ask price and placed orders.'), 
        #'@ actionButton('calculate', 'Start Calculate', 
        #'@              icon = icon('calculator'), class = 'btn-primary'), 
        formattableOutput('fxdata'), 
        tags$hr(), 
        h4('Closed Transaction'), 
        p('Transactions done.'), 
        actionButton('refresh', 'Refresh Data', 
                     icon = icon('refresh'), class = 'btn-primary'), 
        br(), 
        br(), 
        DT::dataTableOutput('transc')))

## ======================================================================
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
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
        
        if(file.exists(paste0('testRealTimeTransc/data/fcstPunterGMT', today('GMT'), '.rds'))) {
            fcPR <- ldply(dir('data', 
                      pattern = paste0('fcstPunterGMT', today('GMT'))), function(x){
                readRDS(paste0('data/', x)) })
            
            ## filter and only pick USDJPY
            fcPR %<>% filter(.id == 'USDJPY')
            
        } else {
            fcPR <- fcstPunterData()
            
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
            saveRDS(tr.buy, paste0('data/buy.', now('GMT'), '.rds'))
        }
        if(rx$fc.Hi == rx$Ask.Price){
            tr.sell <- rx %>% mutate(Price = fc.High, Transaction = 'Sell') %>% 
                dplyr::select(`TimeStamp (GMT)`, Price, Transaction)
            saveRDS(tr.sell, paste0('data/sell.', now('GMT'), '.rds'))
        }
        
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
        
        input$refresh
        
        if(length(dir('data', pattern = 'sell|buy')) < 1) {
            trn <- ldply(dir('data', pattern = 'sell|buy'), function(x){
                readRDS(paste0('data/', x)) }) %>% 
                mutate(`TimeStamp (GMT)` = ymd_hms(`TimeStamp (GMT)`), 
                       Transaction = factor(Transaction)) %>% 
                dplyr::arrange(desc(`TimeStamp (GMT)`)) %>% 
                mutate(ID = rev(seq_len(nrow(.))), 
                       `TimeStamp (GMT)` = factor(`TimeStamp (GMT)`))
        } else {
            trn <- NULL
        }
        
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
    
    #'@ trnData <- reactive({
    #'@     ldply(dir('data', pattern = 'sell|buy'), function(x){
    #'@         readRDS(paste0('data/', x))}) %>% 
    #'@         mutate(`TimeStamp (GMT)` = ymd_hms(`TimeStamp (GMT)`), 
    #'@                Transaction = factor(Transaction)) %>% 
    #'@         dplyr::arrange(desc(`TimeStamp (GMT)`)) %>% 
    #'@         mutate(ID = rev(seq_len(nrow(.))), 
    #'@                `TimeStamp (GMT)` = factor(`TimeStamp (GMT)`))
    #'@ })
    }

# Run the application 
shinyApp(ui = ui, server = server)
#'@ runApp('testRealTimeTransc', display.mode = 'showcase')


