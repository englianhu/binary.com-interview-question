## ================== Declaration =====================================
suppressWarnings(require('shiny'))
suppressWarnings(require('formattable'))
suppressWarnings(require('DT'))
suppressWarnings(require('memoise'))
suppressWarnings(require('TFX'))
suppressWarnings(require('stringr'))
suppressWarnings(require('RCurl'))

## ===================== UI ===========================================
# Define UI for application that draws a histogram
shinyUI <- fluidPage(
  
  # Application title
  titlePanel(div(
    img(src = 'ENG.jpg', width = '40', align = 'right'), 
    img(src = 'RYO.jpg', width = '20', align = 'right'), 
    img(src = 'binary-logo-resize.jpg', width = '200'), 
    'Real Time Trading System (Trial)')),
  
  pageWithSidebar(
    mainPanel(
      tabsetPanel(
        tabPanel('Price Board', 
                 br(), 
                 p('By refer to', 
                   HTML("<a href='http://matchodds.org'>Match Odds</a>"), 
                   'I created this app to test the real-time trading system for hedge fund... ', 
                   'Once the buying/selling limit orders match the ask/bid offer by banker ', 
                   'a transaction will be closed.', strong('Fct.High'), 'is the sell limit ', 
                   'order while', strong('Fct.Low'), 'is the buy limit order. Kindly refer to', 
                   HTML("<a href='https://dailypriceaction.com/forex-beginners/forex-bid-ask-spread'>Forex Bid Ask Spread</a>"), 
                   'for more information.'), 
                 p('I will fit the real-time trading into ', 
                   HTML("<a href='https://beta.rstudioconnect.com/content/3138/'>Q1App2</a>"), 
                   'in the', strong('Punter tab'), '(Hedge Fund). Kindly refer to ', 
                   HTML("<a href='https://github.com/englianhu/binary.com-interview-question'>binary.com Interview Question</a>"), 
                   'for project details.'), 
                 tags$hr(),
                 h4('Real Time Data'), 
                 p('Real Time bid/ask price and placed orders.'), 
                 p(strong(paste0('Current time (Asia/Tokyo):')), 
                   # p(strong(paste0('Current time (', zone, '):')), 
                   textOutput('currentTime', inline = TRUE)),
                 # actionButton('calculate', 'Start Calculate', 
                 #              icon = icon('calculator'), class = 'btn-primary'), 
                 formattableOutput('fxdata'), 
                 br(), 
                 tags$hr(), 
                 h4('Transaction List'), 
                 p('Below shows the transactions done.'), 
                 actionButton('refresh', 'Refresh Data', 
                              icon = icon('refresh'), class = 'btn-primary'), 
                 br(), 
                 DT::dataTableOutput('transc')), 
        
        tabPanel('Settlement List', 
                 br(), 
                 p('The transaction list will be settled once the opposite site ', 
                   'bid/ask request met. Otherwise, the closed price will be the', 
                   'price for closed transaction.'), 
                 tags$hr()
                 ), 
        tabPanel('Profit & Lose', 
                 br(), 
                 p('The settlement list will be compounded and plot as a graph to ', 
                   'the ROI (return of investment). Therefore investors can evaluate ', 
                   'the performance of the hedge fund.'), 
                 tags$hr()
        ))), 
    
    
    br(), 
    p('Powered by - Copyright® Intellectual Property Rights of ', 
      tags$a(href='http://www.scibrokes.com', target = '_blank', 
             tags$img(height = '20px', alt = 'scibrokes', #align='right', 
                      src='https://raw.githubusercontent.com/scibrokes/betting-strategy-and-model-validation/master/regressionApps/oda-army.jpg')), 
      HTML("<a href='http://www.scibrokes.com'>Scibrokes®</a>"))))

