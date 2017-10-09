# === Setting ======================================================
library('shiny')
library('memoise')
library('magrittr')
library('queuecomputer')

# === Data =========================================================
dtm <<- data.frame(Category = c('Service time distribution', 
                               'Number of clerks', 
                               'Waiting room capacity'), 
                  `M/M/1` = c('Exponential', 'Single', 'Unlimited'), 
                  `M/D/1` = c('Constant', 'Single', 'Unlimited'), 
                  `M/G/1` = c('General', 'Single', 'Unlimited'), 
                  `M/M/s` = c('Exponential', 'Multiple', 'Unlimited'), 
                  `M/M/s/b` = c('Exponential', 'Multiple', 'Limited'))

# === Function =====================================================
P0inf <- function(mServers, rRatio){
  
  Sum = 0
	for(n in 0:(mServers - 1)){
		Sum = Sum + rRatio ^ n / factorial(n)
	}	
	Sum = Sum + rRatio ^ mServers / factorial(mServers) / 
	  (1 - rRatio / mServers)
	
	return(1 / Sum)
	}

P0buffer <- function(mServers, bbuffer, rlambdatomu){
  Sum = 0
  for(n in 0:(mServers - 1)){
    Sum = Sum + rlambdatomu ^ n / factorial(n)
  }
  Sum = Sum + rlambdatomu ^ mServers / factorial(mServers) * 
    (1 - (rlambdatomu / mServers) ^ (bbuffer + 1)) / 
    (1 - rlambdatomu / mServers)

  return(1 / Sum)
  }

LsSum <-  function(mServers, rlambdatomu){
  Sum = 0
  for(n in 0:(mServers - 1)){
    Sum = Sum + (mServers - n) * rlambdatomu ^ n / factorial(n)
  }
  return(Sum)
  }

SumLoss <-  function(mservers, rlambdatomu){
  Sum = 0
  for(n in 0:mServers){
    Sum = Sum + rlambdatomu ^ n / factorial(n)
  }
  return(Sum)
  }


# Using "memoise" to automatically cache the results
calC <- memoise(function(rRatio, sRatio, mCusts, time, 
                         tSD, mServers, nRoom) {
  
  ## M/M/1
  res1A = (1 / rRatio) %>% round(6)
  res1B = (1 / sRatio) %>% round(6)
  res1C = (rRatio / sRatio) %>% round(6)
  
  summ1A = (res1C * 100) %>% round(4)
  summ1F = (100 - summ1A) %>% round(4)
  summ1C = (summ1A / summ1F) %>% round(6)
  summ1E = (summ1C / rRatio) %>% round(6)
  summ1D = (summ1E - res1B) %>% round(6)
  summ1B = (rRatio * summ1D) %>% round(6)
  
  dstC1 = ((1 - res1C) * res1C ^ mCusts) %>% round(6)
  dstT1 = (res1C * exp(-sRatio * (1 - res1C) * time)) %>% round(6)
  
  
  ## M/D/1
  summ2B = (rRatio ^ 2 / (2 * sRatio) / 
              (sRatio - rRatio)) %>% round(6)
  summ2C = (summ2B + summ1A / 100) %>% round(4)
  summ2D = (summ2B / rRatio) %>% round(6)
  summ2E = (summ2C / rRatio) %>% round(6)
  
  
  ## M/G/1
  
  summ3B = (rRatio ^ 2 / (1 - summ1A / 100) * 
              (1 + tSD ^ 2 * sRatio ^ 2) / 2) %>% round(6)
  summ3C = (summ3B + summ1A / 100) %>% round(4)
  summ3D = (summ3B / rRatio) %>% round(6)
  summ3E = (summ3C / rRatio) %>% round(6)
  
  
  ## M/M/s
  res4C = (res1C / mServers) %>% round(6)
  
  summ4A = (res4C * 100) %>% round(4)
  summ4F = (P0inf(mServers, res1C) * 100) %>% round(4)
  summ4B = ((rRatio / sRatio) ^ (mServers + 1) / mServers / 
              factorial(mServers) / (1 - res4C) ^ 2 * summ4F / 100
  ) %>% round(6)
  summ4C = (summ4B + res1C) %>% round(4)
  summ4D = (summ4B / rRatio) %>% round(6)
  summ4E = (summ4C / rRatio) %>% round(6)
  summ4G = ((mServers * res4C) ^ mServers / factorial(mServers) / 
              (1 - res4C) * summ4F / 100) %>% round(4)
  summ4H = (100 - summ4G) %>% round(4)
  
  if(mCusts == 0) {
    dstC4 = summ4F %>% round(6)
  } else if(mCusts < (mServers + 1)) {
    dstC4 = ((mServers * res4C) ^ mCusts / factorial(mCusts) * 
               summ4F) %>% round(6)
  } else {
    dstC4 = ((mServers * res4C) ^ mCusts / factorial(mServers) / 
               mServers ^ (mCusts - mServers) * summ4F) %>% round(6)
  }
  dstT4 = (summ4G * exp(-mServers * sRatio * (1 - res4C) * time)) %>% round(6)
  
  
  ## M/M/s/b
  summ5F = (P0buffer(mServers, nRoom, res1C) * 100) %>% round(4)
  summ5B = (res1C ^ (mServers + 1) / (mServers * factorial(mServers)) / 
              (1 - res4C) ^ 2 * summ5F / 100 * 
              (1 - res4C ^ nRoom - nRoom * 
                 (1 - res4C) * res4C ^ nRoom)) %>% round(6)
  if(mServers > 1){
    summ5C = (summ5B + mServers - summ5F / 100 * LsSum(mServers, res1C)) %>% round(6)
  } else {
    summ5C = (res1C/(1 - res1C) - (mServers + nRoom + 1) * res1C ^ 
                (mServers + nRoom + 1) / (1 - res1C) ^ 
                (mServers + nRoom + 1)) %>% round(6)
  }
  if(nRoom > 0){
    summ5G = (res1C ^ (mServers * nRoom) / factorial(mServers) / 
                mServers ^ nRoom * summ5F) %>% round(4)
  } else if(nRoom == 0){
    summ5G = ((res1C ^ mServers / factorial(mServers) / 
                 SumLoss(mServers, res1C)) * 100) %>% round(4)
  } else {
    summ5G = 0 %>% round(4)
  }
  summ5H = ((100 - summ5G) * rRatio) %>% round(6)
  summ5D = (summ5B / summ5H * 100) %>% round(6)
  summ5E = (summ5C / summ5H * 100) %>% round(6)
  
  
    
  ## list
  MM1 = list(res1A = res1A, res1B = res1B, res1C = res1C, 
             summ1A = summ1A, summ1B = summ1B, summ1C = summ1C, 
             summ1D = summ1D, summ1E = summ1E, summ1F = summ1F, 
             dstC1 = dstC1, dstT1 = dstT1)
  
  MD1 = list(res2A = res1A, res2B = res1B, res2C = res1C, 
             summ2A = summ1A, summ2B = summ2B, summ2C = summ2C, 
             summ2D = summ2D, summ2E = summ2E, summ2F = summ1F)
  
  MG1 = list(res3A = res1A, res3B = res1B, res3C = res1C, 
             summ3A = summ1A, summ3B = summ3B, summ3C = summ3C, 
             summ3D = summ3D, summ3E = summ3E, summ3F = summ1F)
  
  MMs = list(res4A = res1A, res4B = res1B, res4C = res4C, 
             summ4A = summ4A, summ4B = summ4B, summ4C = summ4C, 
             summ4D = summ4D, summ4E = summ4E, summ4F = summ4F, 
             summ4G = summ4G, summ4H = summ4H, 
             dstC4 = dstC4, dstT4 = dstT4)
  
  MMsb = list(res5A = res1A, res5B = res1B, res5C = res4C, 
             summ5A = summ4A, summ5B = summ5B, summ5C = summ5C, 
             summ5D = summ5D, summ5E = summ5E, summ5F = summ5F, 
             summ5G = summ5G, summ5H = summ5H)
  
  tmp = list(MM1 = MM1, MD1 = MD1, MG1 = MG1, MMs = MMs, MMsb = MMsb)
  
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
        selectInput('timeUnit', 'Units of Time :',
                    choices = c('Year' = 'year', 
                                'Month' = 'month', 
                                'Week' = 'week', 
                                'Day' = 'day', 
                                'Hour' = 'hour', 
                                'Minute' = 'minute', 
                                'Second' = 'second'), 
                    selected = 'minute'), 
        br(), 
        h3('Customers'), 
        numericInput('arrRate', HTML('Arrival rate (&lambda; from 0.0001 to 50) :'), 
                     min = 0, max = 50, step = 0.0001, value = 1.666667), 
        numericInput('lvRate', 
                     p('Reneging ', 
                       tags$a(href='https://people.emich.edu/aross15/q/brr.html', target='_blank', 
                              tags$img(height = '20px', alt='hot', #align='right', 
                                       src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/q.jpg')), 
                       ' rate (', HTML('&rho;'), ' from 0.0001 to 50) :'), 
                     min = 0, max = 50, step = 0.0001, value = 20), 
        sliderInput('numCst', HTML('Distribution of number of customers in post office (n) :'), 
                    min = 0, max = 50, step = 1, value = 8), 
        numericInput('timeT', HTML('Distribution of time in queue (t from 0.0001 to 50) :'), 
                     min = 0, max = 50, step = 0.0001, value = 0.25), 
        br(), 
        h3('Clerks'), 
        sliderInput('numSrv', HTML('Number of identical clerks (&zeta;) :'), 
                    min = 0, max = 50, step = 1, value = 4), 
        numericInput('srvRate', HTML('Service rate (&mu; from 0.0001 to 50) :'), 
                    min = 0, max = 50, step = 0.0001, value = 4), 
        numericInput('srvtSD', HTML('Service time standard deviation (&mu; from 0.0001 to 50) :'), 
                     min = 0, max = 50, step = 0.0001, value = 0.3), 
        sliderInput('roomSize', HTML('Waiting room size (&eta;) :'), 
                    min = 0, max = 50, step = 1, value = 50)),
      
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
                              h3('Miscellaneous'), 
                              p('Queue models are applicable to hospitals, banks, investor fund, e-commerce etc. Kindly refer to reference or below links for further understanding.', 
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
                              p('13. ', HTML("<a href='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Queues%20with%20Breakdowns%20and%20Customer%20Discouragement.pdf'>Queues with Breakdowns and Customer Discouragement</a>")), 
                              p('14. ', HTML("<a href='http://people.brunel.ac.uk/~mastjjb/jeb/or/queue.html'>Queueing theory</a>"), 
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
