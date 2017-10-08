# === Setting ======================================================
library('shiny')
library('memoise')
library('magrittr')
library('queuecomputer')

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


getFOREX <- function(currency) {
  if(currency == 'USDAUD') {
    getSymbols('JPY=X', from = Sys.Date() %m-% years(1), to = Sys.Date())
    USDJPY <- `JPY=X` %>% Cl %>% na.omit; rm(`JPY=X`)
    names(USDJPY) %<>% str_replace_all('JPY=X', 'USDJPY')
  } else if(currency == 'USDEUR') {
    getSymbols('EUR=X', from = Sys.Date() %m-% years(1), to = Sys.Date())
    USDEUR <- `EUR=X` %>% Cl %>% na.omit; rm(`EUR=X`)
    names(USDEUR) %<>% str_replace_all('EUR=X', 'USDEUR')
  } else if(currency == 'USDGBP') {
    getSymbols('JPY=X', from = Sys.Date() %m-% years(1), to = Sys.Date())
    USDGBP <- `JPY=X` %>% Cl %>% na.omit; rm(`JPY=X`)
    names(USDGBP) %<>% str_replace_all('JPY=X', 'USDJPY')
  } else if(currency == 'USDCHF') {
    getSymbols('JPY=X', from = Sys.Date() %m-% years(1), to = Sys.Date())
    USDJPY <- `JPY=X` %>% Cl %>% na.omit; rm(`JPY=X`)
    names(USDJPY) %<>% str_replace_all('JPY=X', 'USDJPY')
  } else if(currency == 'USDCAD') {
    getSymbols('JPY=X', from = Sys.Date() %m-% years(1), to = Sys.Date())
    USDJPY <- `JPY=X` %>% Cl %>% na.omit; rm(`JPY=X`)
    names(USDJPY) %<>% str_replace_all('JPY=X', 'USDJPY')
  } else if(currency == 'USDCNY') {
    getSymbols('JPY=X', from = Sys.Date() %m-% years(1), to = Sys.Date())
    USDJPY <- `JPY=X` %>% Cl %>% na.omit; rm(`JPY=X`)
    names(USDJPY) %<>% str_replace_all('JPY=X', 'USDJPY')
  } else if(currency == 'USDJPY') {
    getSymbols('JPY=X', from = Sys.Date() %m-% years(1), to = Sys.Date())
    USDJPY <- `JPY=X` %>% Cl %>% na.omit; rm(`JPY=X`)
    names(USDJPY) %<>% str_replace_all('JPY=X', 'USDJPY')
  } else {
    
  }
  
}


# Using "memoise" to automatically cache the results
calC <- memoise(function(mbase) {
  
  armaOrder = armaSearch(USDJPY)
  armaOrder %<>% dplyr::filter(AIC==min(AIC)) %>% .[c('p', 'q')] %>% unlist
  
  
  return(tmp)
})

# === Shiny UI =====================================================
ui <- fluidPage(
  
  titlePanel(
    tags$a(href='http://www.binary.com', target='_blank', 
           tags$img(height = '80px', alt='binary', #align='right', 
                    src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/binary-logo-resize.jpg'))), 
  
  sidebarLayout(
    sidebarPanel(
      selectInput('curr', 'Currency :',
                  choices = c('USDAUD' = 'USDAUD', 
                              'USDEUR' = 'USDEUR', 
                              'USDGBP' = 'USDGBP', 
                              'USDCHF' = 'USDCHF', 
                              'USDCAD' = 'USDCAD', 
                              'USDCNY' = 'USDCNY', 
                              'USDJPY' = 'USDJPY'), 
                  selected = 'USDJPY')),
    
    mainPanel(
      tabsetPanel(
        tabPanel('Introduction', 
                 h3('Question II'), 
                 HTML('<iframe src=\"https://raw.githubusercontent.com/englianhu/binary.com-interview-question/ff20ee95aa60ef5cca3cf797066089103eb62acf/reference/quant-analyst-skills-test.pdf" width=\"600\" height=\"600\"></iframe>'), 
                 h3('How to Choose a Queueing Model?'), 
                 p(paste('All models in this app are Poisson arrival, ', 
                         'ininite population and FCFS. The models differ by : '), 
                   tags$ul(
                     tags$li('the service time distribution (exponential, constant or general)'), 
                     tags$li('the number of clerks (single clerk or multiple clerks)'), 
                     tags$li('waiting room capacity (unlimited waiting room or limited waiting room buffer)'))
                 ), 
                 br(), 
                 tableOutput('tbl1'), 
                 br(), 
                 p('The Erlang Loss model is a special case of M/M/s/b model where the waiting room capacity is zero.')), 
        tabPanel('Queueing Models', 
                 tabsetPanel(
                   tabPanel('M/M/1', 
                            h3('Output'), 
                            htmlOutput('txt1A'), 
                            htmlOutput('txt1B'), 
                            htmlOutput('txt1C'), 
                            br(), 
                            h3('Summary'), 
                            htmlOutput('summ1A'), 
                            htmlOutput('summ1B'), 
                            htmlOutput('summ1C'), 
                            htmlOutput('summ1D'), 
                            htmlOutput('summ1E'), 
                            htmlOutput('summ1F'), 
                            br(), 
                            h3('Distribution'), 
                            htmlOutput('dst1A'), 
                            htmlOutput('dst1B'), 
                            htmlOutput('dst1C'), 
                            htmlOutput('dst1D')), 
                   tabPanel('M/D/1 ', 
                            h3('Output'), 
                            htmlOutput('txt2A'), 
                            htmlOutput('txt2B'), 
                            htmlOutput('txt2C'), 
                            br(), 
                            h3('Summary'), 
                            htmlOutput('summ2A'), 
                            htmlOutput('summ2B'), 
                            htmlOutput('summ2C'), 
                            htmlOutput('summ2D'), 
                            htmlOutput('summ2E'), 
                            htmlOutput('summ2F')), 
                   tabPanel('M/G/1 ', 
                            h3('Output'), 
                            htmlOutput('txt3A'), 
                            htmlOutput('txt3B'), 
                            htmlOutput('txt3C'), 
                            br(), 
                            h3('Summary'), 
                            htmlOutput('summ3A'), 
                            htmlOutput('summ3B'), 
                            htmlOutput('summ3C'), 
                            htmlOutput('summ3D'), 
                            htmlOutput('summ3E'), 
                            htmlOutput('summ3F')), 
                   tabPanel('M/M/s ', 
                            h3('Output'), 
                            htmlOutput('txt4A'), 
                            htmlOutput('txt4B'), 
                            htmlOutput('txt4C'), 
                            br(), 
                            h3('Summary'), 
                            htmlOutput('summ4A'), 
                            htmlOutput('summ4B'), 
                            htmlOutput('summ4C'), 
                            htmlOutput('summ4D'), 
                            htmlOutput('summ4E'), 
                            htmlOutput('summ4F'), 
                            htmlOutput('summ4G'), 
                            htmlOutput('summ4H'), 
                            br(), 
                            h3('Distribution'), 
                            htmlOutput('dst4A'), 
                            htmlOutput('dst4B'), 
                            htmlOutput('dst4C'), 
                            htmlOutput('dst4D')), 
                   tabPanel('M/M/s/b ', 
                            h3('Output'), 
                            htmlOutput('txt5A'), 
                            htmlOutput('txt5B'), 
                            htmlOutput('txt5C'), 
                            br(), 
                            h3('Summary'), 
                            htmlOutput('summ5A'), 
                            htmlOutput('summ5B'), 
                            htmlOutput('summ5C'), 
                            htmlOutput('summ5D'), 
                            htmlOutput('summ5E'), 
                            htmlOutput('summ5F'), 
                            htmlOutput('summ5G'), 
                            htmlOutput('summ5H')))), 
        tabPanel('Appendix', 
                 tabsetPanel(
                   tabPanel('Reference', 
                            h3('Future Works'), 
                            p('Queue models are applicable to hospitals, banks, investor fund, e-commerce etc. Here I leave it for future works due to no sample dataset. Kindly refer to 11th reference or below links.', 
                              tags$ul(
                                tags$li(HTML("<a href='https://github.com/AnthonyEbert/queuecomputer'>queuecomputer</a>")), 
                                tags$li(HTML("<a href='https://cran.r-project.org/web/packages/queuecomputer/vignettes/MMk_queues.html'>M/M/k queues</a>")), 
                                tags$li(HTML("<a href='https://cran.r-project.org/web/packages/queuecomputer/vignettes/Howto.html'>Using the queuecomputer package</a>")))), 
                            br(), 
                            h3('Reference'), 
                            p('01. ', HTML("<a href='https://github.com/englianhu/binary.com-interview-question/blob/master/reference/Queue-534.xls'>Queueing model 534 in Excel</a>"), 
                              tags$a(href='https://github.com/scibrokes/owner', target='_blank', 
                                     tags$img(height = '20px', alt='hot', #align='right', 
                                              src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/hot.jpg'))), 
                            p('02. ', HTML("<a href='https://github.com/englianhu/binary.com-interview-question/blob/master/reference/QueueMacros.xls'>Queueing model macro in Excel</a>"), 
                              tags$a(href='https://github.com/scibrokes/owner', target='_blank', 
                                     tags$img(height = '20px', alt='hot', #align='right', 
                                              src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/hot.jpg'))), 
                            p('03. ', HTML("<a href='https://www.r-bloggers.com/queueing-up-in-r-continued/'>Queueing up in R, continued</a>")), 
                            p('04. ', HTML("<a href='https://www.r-bloggers.com/waiting-in-line-waiting-on-r/'>Waiting in line, waiting on R</a>")), 
                            p('05. ', HTML("<a href='https://www.r-bloggers.com/simulating-a-queue-in-r/'>Simulating a Queue in R</a>")), 
                            p('06. ', HTML("<a href='https://www.researchgate.net/post/What_is_the_queue_data_structure_in_R#59d5b01b404854fdc9168902'>What is the queue data structure in R?</a>")), 
                            p('07. ', HTML("<a href='https://www.r-bloggers.com/implementing-a-queue-as-a-reference-class/'>Implementing a Queue as a Reference Class</a>")), 
                            p('08. ', HTML("<a href='http://r.789695.n4.nabble.com/queue-implementation-td2529272.html'>queue implementation?</a>")), 
                            p('09. ', HTML("<a href='http://www.supositorio.com/rcalc'>Queueing Theory Calculator</a>"), 
                              tags$a(href='https://github.com/scibrokes/owner', target='_blank', 
                                     tags$img(height = '20px', alt='hot', #align='right', 
                                              src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/hot.jpg'))), 
                            p('10. ', HTML("<a href='http://perfdynamics.blogspot.my/2010/05/simulating-queue-in-r.html?m=1'>The Pith of Performance</a>"), 
                              tags$a(href='https://github.com/scibrokes/owner', target='_blank', 
                                     tags$img(height = '20px', alt='hot', #align='right', 
                                              src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/hot.jpg'))), 
                            p('11. ', HTML("<a href='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Computationally%20Efficient%20Simulation%20of%20Queues%20-%20The%20R%20Package%20queuecomputer.pdf'>Computationally Efficient Simulation of Queues - The R Package queuecomputer</a>"), 
                              tags$a(href='https://github.com/scibrokes/owner', target='_blank', 
                                     tags$img(height = '20px', alt='hot', #align='right', 
                                              src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/hot.jpg'))), 
                            p('12. ', HTML("<a href='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Waiting-Line%20Models.pdf'>Waiting-Line Models</a>")), 
                            p('13. ', HTML("<a href='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Queues%20with%20Breakdowns%20and%20Customer%20Discouragement.pdf'>Queues with Breakdowns and Customer Discouragement</a>"))), 
                   
                   tabPanel('Author', 
                            h3('Author'), 
                            tags$iframe(src = 'https://englianhu.github.io/2016/12/ryo-eng.html', height = 800, width = '100%', frameborder = 0))))))), 
  br(), 
  p('Powered by - Copyright® Intellectual Property Rights of ', 
    tags$a(href='http://www.scibrokes.com', target='_blank', 
           tags$img(height = '20px', alt='scibrokes', #align='right', 
                    src='https://raw.githubusercontent.com/scibrokes/betting-strategy-and-model-validation/master/regressionApps/oda-army.jpg')), 
    HTML("<a href='http://www.scibrokes.com'>Scibrokes®</a>")))





# === Shiny Server ===============================================
server <- function(input, output) {
  
  output$txt1 <- renderText({
    paste0('No of customers (in ', input$timeUnit, ')')
  })
  
  output$tbl1 <- renderTable( dtm )
  
  terms <- reactive({
    ## Change when the "update" button is pressed...
    #'@ input$update
    
    ## ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing queue calculation...")
        calC(rRatio = input$arrRate, 
             sRatio = input$srvRate, 
             mCusts = input$numCst, 
             time = input$timeT, 
             tSD = input$srvtSD, 
             mServers = input$numSrv, 
             nRoom = input$roomSize)
      })
    })
  })
  
  
  ## Output tabPanel 1 MM1
  output$txt1A <- renderText({
    paste('Mean time between arrivals :', '<font color=\"#FF0000\"><b>', 
          terms()$MM1$res1A, input$timeUnit, '</b></font>') })
  
  output$txt1B <- renderText({
    paste('Mean time per service :', '<font color=\"#FF0000\"><b>', 
          terms()$MM1$res1B, input$timeUnit, '</b></font>')})
  
  output$txt1C <- renderText({
    paste('Traffic intensity :', '<font color=\"#FF0000\"><b>', 
          terms()$MM1$res1C, '</b></font>')})
  
  ## Summary
  output$summ1A <- renderText({
    paste('Utilization rate of clerk :', '<font color=\"#FF0000\"><b>', 
          terms()$MM1$summ1A, '%</b></font>')})
  
  output$summ1B <- renderText({
    paste('Average number of customers waiting in line (Lq) :', 
          '<font color=\"#FF0000\"><b>', 
          terms()$MM1$summ1B, '</b></font>customers')})
  
  output$summ1C <- renderText({
    paste('Average number of customer in post office (L) :', 
          '<font color=\"#FF0000\"><b>', 
          terms()$MM1$summ1C, '</b></font>customers')})
  
  output$summ1D <- renderText({
    paste('Average time waiting in line (Wq) :', '<font color=\"#FF0000\"><b>', 
          terms()$MM1$summ1D, input$timeUnit, '</b></font>')})
  
  output$summ1E <- renderText({
    paste('Average time in post office (W) :', '<font color=\"#FF0000\"><b>', 
          terms()$MM1$summ1E, input$timeUnit, '</b></font>')})
  
  output$summ1F <- renderText({
    paste('Probability of no customers in post office (P0) :', 
          '<font color=\"#FF0000\"><b>', terms()$MM1$summ1F, 
          '%</b></font> (Probabilities of empty customers)')})
  
  output$dst1A <- renderText({
    paste('Distribution of number of customers in post office :', 
          '<font color=\"#FF0000\"><b>', input$numCst, '</b></font>')})
  
  output$dst1B <- renderText({
    paste('P(n in post office) :', '<font color=\"#FF0000\"><b>', 
          terms()$MM1$dstC1, '</b></font>')})
  
  output$dst1C <- renderText({
    paste('Distribution of of time in queue :', 
          '<font color=\"#FF0000\"><b>', input$timeT, '</b></font>')})
  
  output$dst1D <- renderText({
    paste('P(wait > t) :', '<font color=\"#FF0000\"><b>', 
          terms()$MM1$dstT1, '</b></font>')})
  
  
  ## output tabPanel 2 MD1
  output$txt2A <- renderText({
    paste('Mean time between arrivals :', '<font color=\"#FF0000\"><b>', 
          terms()$MD1$res2A, input$timeUnit, '</b></font>') })
  
  output$txt2B <- renderText({
    paste('Mean time per service :', '<font color=\"#FF0000\"><b>', 
          terms()$MD1$res2B, input$timeUnit, '</b></font>')})
  
  output$txt2C <- renderText({
    paste('Traffic intensity :', '<font color=\"#FF0000\"><b>', 
          terms()$MD1$res2C, '</b></font>')})
  
  ## Summary
  output$summ2A <- renderText({
    paste('Utilization rate of clerk :', '<font color=\"#FF0000\"><b>', 
          terms()$MD1$summ2A, '%</b></font>')})
  
  output$summ2B <- renderText({
    paste('Average number of customers waiting in line (Lq) :', 
          '<font color=\"#FF0000\"><b>', 
          terms()$MD1$summ2B, '</b></font>customers')})
  
  output$summ2C <- renderText({
    paste('Average number of customer in post office (L) :', 
          '<font color=\"#FF0000\"><b>', 
          terms()$MD1$summ2C, '</b></font>customers')})
  
  output$summ2D <- renderText({
    paste('Average time waiting in line (Wq) :', '<font color=\"#FF0000\"><b>', 
          terms()$MD1$summ2D, input$timeUnit, '</b></font>')})
  
  output$summ2E <- renderText({
    paste('Average time in post office (W) :', '<font color=\"#FF0000\"><b>', 
          terms()$MD1$summ2E, input$timeUnit, '</b></font>')})
  
  output$summ2F <- renderText({
    paste('Probability of no customers in post office (P0) :', 
          '<font color=\"#FF0000\"><b>', terms()$MD1$summ2F, 
          '%</b></font> (Probabilities of empty customers)')})
  
  ## output tabPanel 3 MG1
  output$txt3A <- renderText({
    paste('Mean time between arrivals :', '<font color=\"#FF0000\"><b>', 
          terms()$MG1$res3A, input$timeUnit, '</b></font>') })
  
  output$txt3B <- renderText({
    paste('Mean time per service :', '<font color=\"#FF0000\"><b>', 
          terms()$MG1$res3B, input$timeUnit, '</b></font>')})
  
  output$txt3C <- renderText({
    paste('Traffic intensity :', '<font color=\"#FF0000\"><b>', 
          terms()$MG1$res3C, '</b></font>')})
  
  ## Summary
  output$summ3A <- renderText({
    paste('Utilization rate of clerk :', '<font color=\"#FF0000\"><b>', 
          terms()$MG1$summ3A, '%</b></font>')})
  
  output$summ3B <- renderText({
    paste('Average number of customers waiting in line (Lq) :', 
          '<font color=\"#FF0000\"><b>', 
          terms()$MG1$summ3B, '</b></font>customers')})
  
  output$summ3C <- renderText({
    paste('Average number of customer in post office (L) :', 
          '<font color=\"#FF0000\"><b>', 
          terms()$MG1$summ3C, '</b></font>customers')})
  
  output$summ3D <- renderText({
    paste('Average time waiting in line (Wq) :', '<font color=\"#FF0000\"><b>', 
          terms()$MG1$summ3D, input$timeUnit, '</b></font>')})
  
  output$summ3E <- renderText({
    paste('Average time in post office (W) :', '<font color=\"#FF0000\"><b>', 
          terms()$MG1$summ3E, input$timeUnit, '</b></font>')})
  
  output$summ3F <- renderText({
    paste('Probability of no customers in post office (P0) :', 
          '<font color=\"#FF0000\"><b>', terms()$MG1$summ3F, 
          '%</b></font> (Probabilities of empty customers)')})
  
  
  ## output tabPanel 4 MMs
  output$txt4A <- renderText({
    paste('Mean time between arrivals :', '<font color=\"#FF0000\"><b>', 
          terms()$MMs$res4A, input$timeUnit, '</b></font>') })
  
  output$txt4B <- renderText({
    paste('Mean time per service :', '<font color=\"#FF0000\"><b>', 
          terms()$MMs$res4B, input$timeUnit, '</b></font>')})
  
  output$txt4C <- renderText({
    paste('Traffic intensity :', '<font color=\"#FF0000\"><b>', 
          terms()$MMs$res4C, '</b></font>')})
  
  ## Summary
  output$summ4A <- renderText({
    paste('Utilization rate of clerk :', '<font color=\"#FF0000\"><b>', 
          terms()$MMs$summ4A, '%</b></font>')})
  
  output$summ4B <- renderText({
    paste('Average number of customers waiting in line (Lq) :', 
          '<font color=\"#FF0000\"><b>', 
          terms()$MMs$summ4B, '</b></font>customers')})
  
  output$summ4C <- renderText({
    paste('Average number of customer in post office (L) :', 
          '<font color=\"#FF0000\"><b>', 
          terms()$MMs$summ4C, '</b></font>customers')})
  
  output$summ4D <- renderText({
    paste('Average time waiting in line (Wq) :', '<font color=\"#FF0000\"><b>', 
          terms()$MMs$summ4D, input$timeUnit, '</b></font>')})
  
  output$summ4E <- renderText({
    paste('Average time in post office (W) :', '<font color=\"#FF0000\"><b>', 
          terms()$MMs$summ4E, input$timeUnit, '</b></font>')})
  
  output$summ4F <- renderText({
    paste('Probability of no customers in post office (P0) :', 
          '<font color=\"#FF0000\"><b>', terms()$MMs$summ4F, 
          '%</b></font> (Probabilities of empty customers)')})
  
  output$summ4G <- renderText({
    paste('Probability that all clerks are busy :', 
          '<font color=\"#FF0000\"><b>', terms()$MMs$summ4G, 
          '%</b></font> (percentage of who wait in queue)')})
  
  output$summ4H <- renderText({
    paste('Probability that at least one clerk is idle :', 
          '<font color=\"#FF0000\"><b>', terms()$MMs$summ4H, 
          '%</b></font> (percentage of who don\'t wait in queue)')})
  
  output$dst4A <- renderText({
    paste('Distribution of number of customers in post office :', 
          '<font color=\"#FF0000\"><b>', input$numCst, '</b></font>')})
  
  output$dst4B <- renderText({
    paste('P(n in post office) :', '<font color=\"#FF0000\"><b>', 
          terms()$MMs$dstC4, '</b></font>')})
  
  output$dst4C <- renderText({
    paste('Distribution of of time in queue :', 
          '<font color=\"#FF0000\"><b>', input$timeT, '</b></font>')})
  
  output$dst4D <- renderText({
    paste('P(wait > t) :', '<font color=\"#FF0000\"><b>', 
          terms()$MMs$dstT4, '</b></font>')})
  
  
  ## output tabPanel 5 MMsb
  output$txt5A <- renderText({
    paste('Mean time between arrivals :', '<font color=\"#FF0000\"><b>', 
          terms()$MMsb$res5A, input$timeUnit, '</b></font>') })
  
  output$txt5B <- renderText({
    paste('Mean time per service :', '<font color=\"#FF0000\"><b>', 
          terms()$MMsb$res5B, input$timeUnit, '</b></font>')})
  
  output$txt5C <- renderText({
    paste('Traffic intensity :', '<font color=\"#FF0000\"><b>', 
          terms()$MMsb$res5C, '</b></font>')})
  
  ## Summary
  output$summ5A <- renderText({
    paste('Utilization rate of clerk :', '<font color=\"#FF0000\"><b>', 
          terms()$MMsb$summ5A, '%</b></font>')})
  
  output$summ5B <- renderText({
    paste('Average number of customers waiting in line (Lq) :', 
          '<font color=\"#FF0000\"><b>', 
          terms()$MMsb$summ5B, '</b></font>customers')})
  
  output$summ5C <- renderText({
    paste('Average number of customer in post office (L) :', 
          '<font color=\"#FF0000\"><b>', 
          terms()$MMsb$summ5C, '</b></font>customers')})
  
  output$summ5D <- renderText({
    paste('Average time waiting in line (Wq) :', '<font color=\"#FF0000\"><b>', 
          terms()$MMsb$summ5D, input$timeUnit, '</b></font>')})
  
  output$summ5E <- renderText({
    paste('Average time in post office (W) :', '<font color=\"#FF0000\"><b>', 
          terms()$MMsb$summ5E, input$timeUnit, '</b></font>')})
  
  output$summ5F <- renderText({
    paste('Probability of no customers in post office (P0) :', 
          '<font color=\"#FF0000\"><b>', terms()$MMsb$summ5F, 
          '%</b></font> (Probabilities of empty customers)')})
  
  output$summ5G <- renderText({
    paste('Probability of rejecting a customer (balking rate) :', 
          '<font color=\"#FF0000\"><b>', terms()$MMsb$summ5G, 
          '%</b></font> (Reject rate)')})
  
  output$summ5H <- renderText({
    paste('Effective arrival rate :', 
          '<font color=\"#FF0000\"><b>', terms()$MMsb$summ5H, 
          '%</b></font> (Entering rate)')})
}

# Run the application 
shinyApp(ui = ui, server = server)
#'@ shiny::runApp('Q2', display.mode = 'showcase')
