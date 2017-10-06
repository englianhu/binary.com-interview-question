# === Setting ======================================================
library(shiny)
library(shinydashboard)

# === Function =====================================================
P0inf <- function(mservers, rratio){
	
	Sum = 0
	for(n in 0:(mservers - 1)){
		Sum = Sum + rratio ^ n / factorial(n)
	}	
	Sum = Sum + rratio ^ mservers / factorial(mservers) / (1 - rratio / mservers)
	return(1 / Sum)
	}


# === Shiny UI =====================================================
ui <- fluidPage(
   
   titlePanel(tags$a(href='http://www.binary.com', target='_blank', 
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
                     min = 0, max = 50, step = 0.0001, value = 1.6667), 
        numericInput('lvRate', HTML('Leaving rate (&rho; from 0.0001 to 50) :'), 
                    min = 0, max = 50, step = 0.0001, value = 20), 
        br(), 
        h3('Clerks'), 
        sliderInput('numSrv', HTML('Number of identical clerks (&zeta;) :'), 
                    min = 0, max = 50, step = 1, value = 4), 
        numericInput('srvRate', HTML('Service rate (&mu; from 0.0001 to 50) :'), 
                    min = 0, max = 50, step = 0.0001, value = 4), 
        sliderInput('roomSize', HTML('Waiting room size (&eta;) :'), 
                    min = 0, max = 50, step = 1, value = 50)
      ),
      
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
                              textOutput('txt1A'), 
                              textOutput('txt1B'), 
                              textOutput('txt1C'), 
                              br(), 
                              h3('Summary'), 
                              textOutput('summ1A'), 
                              textOutput('summ1B'), 
                              textOutput('summ1C'), 
                              textOutput('summ1D'), 
                              textOutput('summ1E'), 
                              textOutput('summ1F')), 
                     tabPanel('M/C/1 ', 
                              h3('Output'), 
                              textOutput('txt2A'), 
                              textOutput('txt2B'), 
                              textOutput('txt2C'), 
                              br(), 
                              h3('Summary')), 
                     tabPanel('M/G/1 ', 
                              h3('Output'), 
                              textOutput('txt3A'), 
                              textOutput('txt3B'), 
                              textOutput('txt3C'), 
                              br(), 
                              h3('Summary')), 
                     tabPanel('M/M/s ', 
                              h3('Output'), 
                              textOutput('txt4A'), 
                              textOutput('txt4B'), 
                              textOutput('txt4C'), 
                              br(), 
                              h3('Summary')), 
                     tabPanel('M/M/s/b ', 
                              h3('Output'), 
                              textOutput('txt5A'), 
                              textOutput('txt5B'), 
                              textOutput('txt5C'), 
                              br(), 
                              h3('Summary')))), 
          tabPanel('Appendix' 
                   tabsetPanel(
                     tabPanel('Reference', 
                              h3('Reference'), 
                              p('01. ', HTML("<a href='https://github.com/englianhu/binary.com-interview-question/blob/master/reference/Stock%20Market%20Forecasting%20Using%20LASSO%20Linear%20Regression%20Model.pdf'>Stock Market Forecasting Using LASSO Linear Regression Model</a>"), 
                                tags$a(href='https://github.com/scibrokes/owner', target='_blank', 
                                       tags$img(height = '20px', alt='hot', #align='right', 
                                                src='hot.jpg'))), 
                              
                     tabPanel('Author', 
                              h3('Author'))
                   )
              )
          )
      )
   ), 
   br(), 
   p('Powered by - Copyright® Intellectual Property Rights of ', 
     tags$a(href='http://www.scibrokes.com', target='_blank', 
            tags$img(height = '20px', alt='hot', #align='right', 
                     src='https://raw.githubusercontent.com/scibrokes/betting-strategy-and-model-validation/master/regressionApps/oda-army.jpg')), 
     HTML("<a href='http://www.scibrokes.com'>Scibrokes®</a>")))





# === Shiny Server ===============================================
server <- function(input, output) {
  
  output$txt1 <- renderText({
    paste0('No of customers (in ', input$timeUnit, ')')
  })
  
  dtm <- data.frame(Category = c('Service time distribution', 
                                 'Number of clerks', 
                                 'Waiting room capacity'), 
                    `M/M/1` = c('Exponential', 'Single', 'Unlimited'), 
                    `M/D/1` = c('Constant', 'Single', 'Unlimited'), 
                    `M/G/1` = c('General', 'Single', 'Unlimited'), 
                    `M/M/s` = c('Exponential', 'Multiple', 'Unlimited'), 
                    `M/M/s/b` = c('Exponential', 'Multiple', 'Limited'))
  
  output$tbl1 <- renderTable({
    return(dtm)
  })
  
  
  ## output tabPanel 1
  output$txt1A <- renderText({
    paste('Mean time between arrivals :', round(1/input$arrRate, 6))
  })
  
  output$txt1B <- renderText({
    paste('Mean time per service :', 
          round(1/input$srvRate, 6), input$timeUnit)
  })
  
  output$txt1C <- renderText({
    paste('Traffic intensity :', 
          round(input$arrRate/input$srvRate, 6), input$timeUnit)
  })

  ## Summary
  output$summ1A <- renderText({
    paste('Utilization rate of clerk :', 
          round(input$arrRate/input$srvRate * 100, 4), '%')
  })
  
  output$summ1B <- renderText({
    paste('Average number of customers waiting in line :', 
          round(input$arrRate * (input$arrRate/input$srvRate)/
                  (1 - (input$arrRate/input$srvRate))/
                  input$arrRate - 1/input$srvRate, 6), 'customers')
  })
  
  output$summ1C <- renderText({
    paste('Average number of customer in Post office :', 
          round((input$arrRate/input$srvRate)/
                  (1 - (input$arrRate/input$srvRate)), 6), 'customers')
  })
  
  output$summ1D <- renderText({
    paste('Average time waiting in line :', 
          round((input$arrRate/input$srvRate)/
                  (1 - (input$arrRate/input$srvRate))/
                  input$arrRate - 1/input$srvRate, 6), input$timeUnit)
  })
  
  output$summ1E <- renderText({
    paste('Average time in Post office :', 
          round((input$arrRate/input$srvRate)/
                  (1 - (input$arrRate/input$srvRate))/
                  input$arrRate, 6), input$timeUnit)
  })
  
  output$summ1F <- renderText({
    paste('Probability of no customers in Post office :', 
          round((1 - input$arrRate/input$srvRate) * 100, 4), '% (Probabilities of empty customers)')
  })
  
  
  ## output tabPanel 2
  output$txt2A <- renderText({
    paste('Mean time between arrivals :', round(1/input$arrRate, 6))
  })
  
  output$txt2B <- renderText({
    paste('Mean time per service :', 
          round(1/input$srvRate, 6), input$timeUnit)
  })
  
  output$txt2C <- renderText({
    paste('Traffic intensity :', 
          round(input$arrRate/input$srvRate, 6), input$timeUnit)
  })
  
  
  ## output tabPanel 3
  output$txt3A <- renderText({
    paste('Mean time between arrivals :', round(1/input$arrRate, 6))
  })
  
  output$txt3B <- renderText({
    paste('Mean time per service :', 
          round(1/input$srvRate, 6), input$timeUnit)
  })
  
  output$txt3C <- renderText({
    paste('Traffic intensity :', 
          round(input$arrRate/input$srvRate, 6), input$timeUnit)
  })
  
  
  ## output tabPanel 4
  output$txt4A <- renderText({
    paste('Mean time between arrivals :', round(1/input$arrRate, 6))
  })
  
  output$txt4B <- renderText({
    paste('Mean time per service :', 
          round(1/input$srvRate, 6), input$timeUnit)
  })
  
  output$txt4C <- renderText({
    paste('Traffic intensity :', 
          round(input$arrRate/input$srvRate, 6), input$timeUnit)
  })
  
  
  ## output tabPanel 5
  output$txt5A <- renderText({
    paste('Mean time between arrivals :', round(1/input$arrRate, 6))
  })
  
  output$txt5B <- renderText({
    paste('Mean time per service :', 
          round(1/input$srvRate, 6), input$timeUnit)
  })
  
  output$txt5C <- renderText({
    paste('Traffic intensity :', 
          round(input$arrRate/input$srvRate, 6), input$timeUnit)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
#'@ shiny::runApp('Q2', display.mode = 'showcase')
