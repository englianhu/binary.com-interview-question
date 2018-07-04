## ================== Declaration ======================================
suppressWarnings(require('shiny'))
suppressWarnings(require('shinyjs'))
suppressWarnings(require('TFX'))
suppressWarnings(require('formattable'))
suppressWarnings(require('DT'))
suppressWarnings(require('cronR'))
suppressWarnings(require('xts'))
suppressWarnings(require('lubridate'))
suppressWarnings(require('plyr'))
suppressWarnings(require('dplyr'))
suppressWarnings(require('magrittr'))
suppressWarnings(require('memoise'))
suppressWarnings(require('stringr'))
suppressWarnings(require('RCurl'))
suppressWarnings(require('forecast'))

#fx <- c('EURUSD=X', 'JPY=X', 'GBPUSD=X', 'CHF=X', 'CAD=X', 'AUDUSD=X')
fx <- c('JPY=X')
fxObj <- c('USDJPY')

#wd <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
wd <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
wd %<>% factor(., levels = ., ordered = TRUE)

## ================== Server ===========================================
# Define server logic required to draw a histogram
#shinyServer <- function(input, output, session) {
server <- shinyServer(function(input, output, session) {
    
    output$currentTime <- renderText({
        # Forces invalidation in 1000 milliseconds
        invalidateLater(1000, session)
        as.character(now('Asia/Tokyo')) #Japanese Timezone
    })
    
    fcstPunterData <- reactive({
        isolate({
            withProgress({
                setProgress(message = "Processing algorithmic forecast...")
                #fxHL <- forecastUSDJPYHL(ahead = prd)
                
                if(file.exists(paste0('data/fcstPunterGMT', today('GMT'), '.rds'))) {
                    fxHL <- ldply(dir('data', 
                                      pattern = paste0('fcstPunterGMT', today('GMT'))), function(x){
                                          readRDS(paste0('data/', x)) })
                    
                } else {
                    
                    repeat{
                        # startTime <- now('GMT')
                        startTime <- today('GMT')
                        # validate(need(weekdays(today('GMT')) %in% wd, 'Today has no data.'))
                        
                        ## https://finance.yahoo.com/quote/AUDUSD=X?p=AUDUSD=X
                        ## Above link prove that https://finance.yahoo.com using GMT time zone.  
                        #if(weekdays(today('GMT')) %in% wd) {
                        #prd <- ifelse(weekdays(today('GMT')) %in% wd[2:5], 1, 3)
                        prd <- 1 #since count trading day.
                        
                        for(i in seq(fx)) {
                            assign(fxObj[i], na.omit(suppressWarnings(
                                getSymbols(fx[i], from = (today('GMT') - prd) %m-% years(1), 
                                           to = (today('GMT') - prd), auto.assign = FALSE)))) }
                        rm(i) #}
                        
                        fxHL <- forecastUSDJPYHL(USDJPY, ahead = prd)
                        #'@ print(as.character(now('GMT')))
                        #'@ print(fxHL)
                        if(exists('fxHL')) break
                        
                        ## scheduled sleepTime as 24 hours to start next task
                        sleepTime <- startTime + 24*60*60 - startTime
                        if (sleepTime > 0)
                            Sys.sleep(sleepTime) }
                }
            })
        })
        
        if(!dir.exists('data')) dir.create('data')
        if(!file.exists(paste0('data/fcstPunterGMT', today('GMT'), '.rds'))){
            saveRDS(fxHL, paste0('data/fcstPunterGMT', today('GMT'), '.rds')) }
        
        return(fxHL)
    })
    # ---------------------------------------------------------------
    ## https://segmentfault.com/a/1190000009775258
    ## seq(from = now('GMT'), length.out = 60 * 24, by = "mins") %>% range
    #'@ observeEvent({
    #'@     if(now('GMT') == timeR) {
    #'@         timeR <- timeR + minutes(1)
    #'@     } else {
    #'@         fxLo <- forecastUSDJPY(price = 'Lo')
    #'@         fxHi <- forecastUSDJPY(price = 'Hi')
    #'@         fxHL <- merge(fxHi, fxLo, by = c('.id', 'ForecastDate.GMT'))
    #'@         rm(fxHi, fxLo)
    #'@         fxHL %>% mutate(
    #'@             Currency.Hi = rnorm(6, mean = (Currency.Hi + Currency.Lo) / 2, sd = 0.001), 
    #'@             Currency.Lo = rnorm(6, mean = (Currency.Hi + Currency.Lo) / 2, sd = 0.001))
    #'@     }
    #'@     
    #'@     output$fxdata <- renderFormattable({
    #'@         
    #'@         rx <- refresh()
    #'@         
    #'@         rx %>% formattable(list(
    #'@             Bid.Price = formatter('span', 
    #'@                                   style = x ~ style(color = ifelse(x > (rx$Fct.Low + rx$Fct.High) / 2, 'red', 'green')), 
    #'@                                   x ~ icontext(ifelse(x > (rx$Fct.Low + rx$Fct.High) / 2, 'arrow-down', 'arrow-up'), x)), 
    #'@             Ask.Price = formatter('span', 
    #'@                                   style = x ~ style(color = ifelse(x < (rx$Fct.Low + rx$Fct.High) / 2, 'red', 'green')),
    #'@                                   x ~ icontext(ifelse(x < (rx$Fct.Low + rx$Fct.High) / 2, 'arrow-down', 'arrow-up'), x)), 
    #'@             Fct.Low = formatter('span', 
    #'@                                style = x ~ style(color = ifelse(x > 0, 'red', 'green')), 
    #'@                                x ~ icontext(ifelse(x > 0, 'arrow-down', 'arrow-up'), x)), 
    #'@             Fct.High = formatter('span',
    #'@                                 style = x ~ style(color = ifelse(x < 0, 'red', 'green')),
    #'@                                 x ~ icontext(ifelse(x < 0, 'arrow-down', 'arrow-up'), x))
    #'@         ))})
    #'@ })
    # ---------------------------------------------------------------
    
    fetchData <- reactive({
        #if(!input$pause)
            invalidateLater(750)
        ## http://webrates.truefx.com/rates/connect.html
        qtf <- QueryTrueFX() %>% mutate(TimeStamp = as.character(TimeStamp)) %>% 
            dplyr::rename(`TimeStamp (GMT)` = TimeStamp)
        qtf <- qtf[, c(6, 1:3, 5:4)] %>% filter(Symbol == 'USD/JPY')
        return(qtf)
    })
    
    refresh <- reactive({
        line <- fetchData()
        fcPR <- fcstPunterData()
        
        rx <- cbind(line, fcPR) %>% 
            mutate(Fct.High = round(Fct.High, 3), Fct.Low = round(Fct.Low, 3))
        
        rx %<>% dplyr::rename(`LatestDate (GMT)` = LatestDate.GMT, 
                              `ForecastDate (GMT)` = ForecastDate.GMT)
        
        trdDay <- str_split(rx$'TimeStamp (GMT)', ' ')[[1]][1]
        forDay <- rx$'ForecastDate (GMT)'
        
        #if((rx$Fct.Low == rx$Bid.Price) & (forDay == trdDay)){
        if(rx$Fct.Low == rx$Bid.Price){
            tr.buy <- rx %>% mutate(Price = Fct.Low, Transaction = 'Buy') %>% 
                dplyr::select(`TimeStamp (GMT)`, Price, Transaction)
            saveRDS(tr.buy, paste0('data/buy.', now('GMT'), '.rds')) }
        
        #if((rx$Fct.High == rx$Ask.Price) & (forDay == trdDay)){
        if(rx$Fct.High == rx$Ask.Price){
            tr.sell <- rx %>% mutate(Price = Fct.High, Transaction = 'Sell') %>% 
                dplyr::select(`TimeStamp (GMT)`, Price, Transaction)
            saveRDS(tr.sell, paste0('data/sell.', now('GMT'), '.rds')) }
        
        return(rx)
    })
    
    output$fxdata <- renderFormattable({
    
        rx <- refresh()
        
        rx %>% formattable(list(
            Bid.Price = formatter('span', 
                                  style = x ~ style(color = ifelse(x > (rx$Fct.Low + rx$Fct.High) / 2, 'red', 'green')), 
                                  x ~ icontext(ifelse(x > (rx$Fct.Low + rx$Fct.High) / 2, 'arrow-down', 'arrow-up'), x)), 
            Ask.Price = formatter('span', 
                                  style = x ~ style(color = ifelse(x < (rx$Fct.Low + rx$Fct.High) / 2, 'red', 'green')),
                                  x ~ icontext(ifelse(x < (rx$Fct.Low + rx$Fct.High) / 2, 'arrow-down', 'arrow-up'), x)), 
            Fct.Low = formatter('span', 
                            style = x ~ style(color = ifelse(x > 0, 'red', 'green')), 
                            x ~ icontext(ifelse(x > 0, 'arrow-down', 'arrow-up'), x)), 
            Fct.High = formatter('span',
                             style = x ~ style(color = ifelse(x < 0, 'red', 'green')),
                             x ~ icontext(ifelse(x < 0, 'arrow-down', 'arrow-up'), x))
        ))})
    
    output$transc <- DT::renderDataTable({
        
        input$refresh
        
        if(length(dir('data', pattern = 'sell|buy')) > 0) {
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
    
    ## https://shiny.rstudio.com/articles/reconnecting.html
    ## Set this to "force" instead of TRUE for testing locally (without Shiny Server)
    #session$allowReconnect(TRUE)
    })

# Run the application 
#'@ shinyApp(ui = ui, server = server)
#'@ runApp('testRealTimeTransc', display.mode = 'showcase')


