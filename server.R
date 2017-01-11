## ========= Setup Options =================================
## Setup Options, Loading Required Libraries and Preparing Environment
## Loading the packages and setting adjustment


## ========= ShinyServer ================================
# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  # Simulate work being done for 1 second
  Sys.sleep(1)
  
  # Hide the loading message when the rest of the server function has executed
  #'@ hide(id = 'loading-content', anim = TRUE, animType = 'fade')   
  #'@ show(id = 'app-content', anim = TRUE, animType = 'fade')
  
  onclick('toggleAdvanced', shinyjs::toggle(id = 'advanced', anim = TRUE, animType = 'fade'))    
  onclick('update', shinyjs::html('time', date()))
  
  ## Define a reactive expression for the document term matrix
  terms <- reactive({
    ## Change when the "Update" button is pressed...
    input$updatePred
    ## ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing graph...")
        filterAAPL(AAPL, input$dataRange[1], input$dataRange[2])
      })
    })
  })
  
  output$hcontainer <- renderHighchart({
    fund <- terms()$fund
    plotChart2(fund, type = 'single', chart.type2 = input$type, 
               chart.theme = input$hc_theme, stacked = input$stacked)
    
  })
  
  output$distTable <- renderDataTable({
    fundDT <- terms()$fundDT
    fundDT %>% datatable(
      caption = "Table : AAPL Stocks Price", 
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
  
})