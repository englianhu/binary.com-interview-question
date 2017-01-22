## ========================== ShinyServer ================================
# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  # Simulate work being done for 1 second
  Sys.sleep(1)
  
  # Hide the loading message when the rest of the server function has executed
  shinyjs::hide(id = 'loading-content', anim = TRUE, animType = 'fade')
  shinyjs::show(id = 'app-content', anim = TRUE, animType = 'fade')
  
  onclick('toggleAdvanced', shinyjs::toggle(id = 'advanced', anim = TRUE, animType = 'fade'))    
  onclick('update', shinyjs::html('time', date()))
  
  ## Define a reactive expression for the date range
  #'@ terms <- reactive({
  #'@   ## Change when the "Update" button is pressed...
  #'@   input$updatePred
  #'@   
  #'@   ## make sure start date inside dataset
  #'@   validate(
  #'@     need(input$dataRange[1] %in% (dateRange), 
  #'@          'start date need to within dateset.'))
  #'@   
  #'@   ## make sure end date inside dataset
  #'@   validate(
  #'@     need(input$dataRange[2] %in% (dateRange), 
  #'@          'end date need to within dateset.'))
  #'@   
  #'@   ## make sure end date later than start date
  #'@   validate(
  #'@     need(input$dataRange[2] > input$dataRange[1], 
  #'@          'end date need to later than start date.'))
  #'@   
  #'@   ## make sure greater than 1 day
  #'@   validate(
  #'@     need(difftime(input$dataRange[2], input$dataRange[1], 'days') > 1, 
  #'@          'date range less the 1 days'))
  #'@   
  #'@   ## make sure maximum 365 days
  #'@   validate(
  #'@     need(input$dataRange[2] - input$dataRange[1] <= 365, 
  #'@          'maximum 365 days data for prediction.'))
  #'@   
  #'@   isolate({
  #'@     withProgress({
  #'@       setProgress(message = "Processing graph...")
  #'@       filterLAD(startDate = input$dataRange[1], endDate = input$dataRange[2])
  #'@     })
  #'@   })
  #'@ })
  
  terms <- reactive({
    ## Change when the "Update" button is pressed...
    input$updatePred
    
    ## make sure end date later than start date
    validate(
      need(input$preDate > input$dataDate, 
           'Forecast date need to later than latest data date.'))
    
    isolate({
      withProgress({
        setProgress(message = "Filtering data...")
        filterLAD(startDate = input$dataDate - 365, endDate = input$dataDate)
        })
      })
  })
  
  terms2 <- reactive({
    ## Change when the "Update" button is pressed...
    input$updatePred
    
    isolate({
      withProgress({
        setProgress(message = "Predicting stock price...")
        fundDT <- terms()$fundDT
        xy <- h(fundDT, family = 'gaussian', xy.matrix = 'h2', setform = 'l4', 
          yv = 'daily.mean2')
        
        #'@ fit <- glmnet(xy$x, xy$y, family = 'gaussian', alpha = 0.8)
        #'@ predict(fit, newx = xy$x, pred.type = 'response')
        pd <- predict(fitgaum16.alpha08, newx = xy$x, 
                      s = fitgaum16.alpha08$lambda.1se, 
                      pred.type = 'class') %>% 
          data.frame %>% tbl_df %>% rename(Pred = X1)
        
        dfm <- fundDT %>% mutate(HL.Mean = (LAD.High + LAD.Low) / 2) %>% 
          data.frame(., pd) %>% tbl_df
        return(dfm)
      })
    })
  })
  
  output$firstday <- renderText({ 
    as.character(input$dataDate - 365)
  })
  
  #'@ repeatable()
  
  #'@ output$hcontainer <- renderHighchart({
  #'@   fund <- terms()$fund
  #'@   plotChart2(fund, type = 'single', chart.type2 = input$type, 
  #'@              chart.theme = input$hc_theme, stacked = input$stacked)
  #'@ })
  
  output$distTable <- renderDataTable({
    fundDT <- terms()$fundDT
    fundDT %>% datatable(
      caption = "Table : LAD Stock Price", 
      escape = FALSE, filter = "top", rownames = FALSE, 
      extensions = list("ColReorder" = NULL, "RowReorder" = NULL, 
                        "Buttons" = NULL, "Responsive" = NULL), 
      options = list(dom = 'BRrltpi', autoWidth = TRUE, scrollX = TRUE, 
                     lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')), 
                     ColReorder = TRUE, rowReorder = TRUE, 
                     buttons = list('copy', 'print', 
                                    list(extend = 'collection', 
                                         buttons = c('csv', 'excel', 'pdf'), 
                                         text = 'Download'), I('colvis'))))
  })#, options = list(pageLength = 10))
  
  output$gsform <- renderPrint({
    read_rds(path = './data/tmpgsform.rds')
    })
  
  output$gsmse <- renderDataTable({
    tmpsumgs %>% datatable(
      #caption = "Table : LAD Stock Price", 
      escape = FALSE, filter = "top", rownames = FALSE, 
      extensions = list("ColReorder" = NULL, "RowReorder" = NULL, 
                        "Buttons" = NULL, "Responsive" = NULL), 
      options = list(dom = 'BRrltpi', autoWidth = TRUE, scrollX = TRUE, 
                     lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')), 
                     ColReorder = TRUE, rowReorder = TRUE, 
                     buttons = list('copy', 'print', 
                                    list(extend = 'collection', 
                                         buttons = c('csv', 'excel', 'pdf'), 
                                         text = 'Download'), I('colvis'))))
  })
  
  output$gsmse1 <- renderFormattable({
    mse1 <- tmpsumgs %>% filter(mse == min(mse))
    #as.htmlwidget(
    mse1 %>% formattable(list(
      .id = color_tile('white', 'darkgoldenrod'), 
      model = color_tile('white', 'darkgoldenrod'), 
      mse = formatter('span', style = x ~ 
                        formattable::style(
                          color = ifelse(rank(x) <= 3, 'blue', 'grey')), 
                      x ~ sprintf('%.6f (rank: %.0f)', x, rank(x)))
    ))#)
  })
  
  #'@ output$testTable <- renderDataTable({
  #'@   tmptable %>% datatable(
  #'@     #caption = "Table : LAD Stock Price", 
  #'@     escape = FALSE, filter = "top", rownames = FALSE, 
  #'@     extensions = list("ColReorder" = NULL, "RowReorder" = NULL, 
  #'@                       "Buttons" = NULL, "Responsive" = NULL), 
  #'@     options = list(dom = 'BRrltpi', autoWidth = TRUE, scrollX = TRUE, 
  #'@                    lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')), 
  #'@                    ColReorder = TRUE, rowReorder = TRUE, 
  #'@                    buttons = list('copy', 'print', 
  #'@                                   list(extend = 'collection', 
  #'@                                        buttons = c('csv', 'excel', 'pdf'), 
  #'@                                        text = 'Download'), I('colvis'))))
  #'@ })
  
  output$bestalpha <- renderText({
    alphaV <- tmpsumgs %>% filter(mse == min(mse)) %>% .$model %>% 
      str_replace_all('mse', '') %>% as.numeric %>% unique
    alphaV <- alphaV/10
  })
  
  output$hcmp <- renderHighchart({
    hcM <- terms2()
    plotChart2(hcM, type = 'single', chart.type2 = input$type, 
               chart.theme = input$hc_theme, stacked = input$stacked)
  })
})