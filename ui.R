## ========= Setup Options =================================
## Setup Options, Loading Required Libraries and Preparing Environment
## Loading the packages and setting adjustment

appCSS <- "
#loading-content {
position: absolute;
opacity: 0.9;
z-index: 100;
top: 0;
bottom: 0;
left: 0;
right: 0;
height: 100%;
text-align: center;
background: url(loader.gif) center no-repeat #fff;
}
"

## ========= ShinyUI ================================
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
  ## this is your web page header information
  tags$head(
    ## here you include your inline styles
    tags$style(HTML("
                    body {
                    Text color: yellow;
                    background-color: darkgoldenrod;
                    }
                    "))),
  #'@ tags$audio(src = 'sound.mp3', type = 'audio/mp3', autoplay = NA, controls = 'controls'), 
  useShinyjs(),
  shinyjs::inlineCSS(appCSS), 
  
  div(id = 'loading-content'), 
  shinyjs::hidden(
  div(id = 'app-content', 
      titlePanel(
        tags$a(href='https://www.binary.com/ja/home.html', target='_blank', 
               tags$img(height = '40px', alt='hot', #align='right', 
                        src='binary-logo-resize.jpg'))), 
      navbarPage('Shiny App', 
        
      tabPanel('Job Discription', h4('Quantitative Analyst at Binary.com'), 
               tags$iframe(src='https://angel.co/binary-com-1/jobs/145277-quantitative-analyst', 
                           height = 800, width = '100%', frameborder = 0)),
      #, seamless = 'seamless')), #seamless will hide the scroller.
      
      tabPanel('Interview Questionaire', 
               tabsetPanel(
                 tabPanel('Questions', 
                          h4('Question Paper'), 
                          p('You are feel free to read the questions below.'), 
                          br(),             
                          HTML('<iframe src=\"https://raw.githubusercontent.com/englianhu/binary.com-interview-question/ff20ee95aa60ef5cca3cf797066089103eb62acf/reference/quant-analyst-skills-test.pdf" width=\"900\" height=\"600\"></iframe>'), 
                          imageOutput('imp_pdf', width = '100%')),#, height = '500px')), 
                 tabPanel('Q1', 
                          tabsetPanel(
                            tabPanel('Question', 
                                     h4('Answer the Question'), 
                                     p('Find a formula to price the following fixed-odds contract:'), 
                                     br(), 
                                     p('I wish to win $', em('B'), ' if over the next days, ', 
                                       em('i'),' the ', em('y'),' has a high-low range
                                       [exceeding/not exceeding] ', em('p'), ' points.'), 
                                     p('Implement the solution in the programming language of your choice.'), 
                                     p('Example: I wish to win $1000 if over the next 7 days the USD/JPY has 
                                       a high-low range exceeding 2 points.'), 
                                     p('For example, if the USD/JPY has a range of low=98.45 and high=100.98 
                                       over the next 7 days, I will win $1000 (because high minus 
                                       low = 2.53 > 2).'), 
                                     p('You can use Monte Carlo simulation to confirm/verify your results, 
                                       but it shouldn\'t be the primary solution. Please provide all the relevant 
                                       details about the solution.')), 
                            tabPanel('Answer (Font-End)', 
                                     h4('Under construction'), 
                                     p(tags$a(href='https://www.ladbrokescoralplc.com/', target='_blank', 
                                              tags$img(height = '40px', alt='hot', #align='right', 
                                                       src='LC.jpg'))), 
                                     p('...')
                            ), 
                            tabPanel('Answer (Back-End)', 
                              sidebarLayout(
                                sidebarPanel(
                                  bsCollapse(id = 'selectSIFund', open = 'Select Fund', 
                                             bsCollapsePanel('Select Your Data Range and Predicted Date :', 
                                                             dateRangeInput('dataRange', label = 'Data Range', 
                                                                            start = first(dateID), end = last(dateID)), 
                                                             dateInput('preDate', label = 'Predicted Date'), 
                                                             actionButton('updatePred', label = 'Update'), 
                                                             actionButton('tabBut', 'View Table'), style = 'primary'), 
                                             bsCollapsePanel('Chart Option', 
                                                             selectInput('type', label = 'Type', width = '100%', 
                                                                         choices = c(FALSE, 'line', 'column', 'spline', 'bar', 'pie'), 
                                                                         selected = 'line'), 
                                                             selectInput('stacked', label = 'Stacked',  width = '100%', 
                                                                         choices = c(FALSE, "normal", "percent"), 
                                                                         selected = 'normal'), 
                                                             selectInput('hc_theme', label = 'Theme',  width = '100%', 
                                                                         choices = c('theme' = 'hc_theme()', '538' = 'hc_theme_538()', 
                                                                                     'chalk' = 'hc_theme_chalk()', 
                                                                                     'darkunica' = 'hc_theme_darkunica()', 
                                                                                     'db' = 'hc_theme_db()', 
                                                                                     'economist' = 'hc_theme_economist()', 
                                                                                     'flat' = 'hc_theme_flat()', 
                                                                                     'flatdark' = 'hc_theme_flatdark()', 
                                                                                     'ft' = 'hc_theme_ft()', 
                                                                                     'google' = 'hc_theme_google()', 
                                                                                     'gridlight' = 'hc_theme_gridlight()', 
                                                                                     'handdrwran' = 'hc_theme_handdrawn()', 
                                                                                     'merge' = 'hc_theme_merge()', 
                                                                                     'null' = 'hc_theme_null()', 
                                                                                     'sandsignika' = 'hc_theme_sandsignika()', 
                                                                                     'smpl' = 'hc_theme_smpl()', 
                                                                                     'sparkline' = 'hc_theme_sparkline()'), 
                                                                         selected = 'hc_theme_economist()'), style = 'primary')),
                                  br(),
                                  a(id = 'toggleAdvanced', 'Show/hide advanced info', href = '#'),
                                  hidden(
                                    div(id = 'advanced', 
                                        p('- Author Profile:', HTML("<a href='https://englianhu.github.io/2016/12/ryo-eng.html'>RYO, ENG Lian Hu</a>")),
                                        p('- GitHub:', HTML("<a href='https://github.com/englianhu/binary.com-interview-question'>Source Code</a>")),
                                        br(),
                                        p('Timestamp: ', 
                                          span(id = 'time', base::date()), 
                                          a(id = 'update', 'Update', href = '#')), 
                                        actionButton('reset', 'Reset form'), style = 'info'))),
                                
                                mainPanel(
                                  h4('Observation'), 
                                  p(tags$a(href='https://www.ladbrokescoralplc.com/', target='_blank', 
                                           tags$img(height = '40px', alt='hot', #align='right', 
                                                    src='LC.jpg'))), 
                                  p('In order to predict the LAD (stock price of ', HTML("<a href='https://www.ladbrokescoralplc.com/'>LadbrokesCoral PLC</a>"), '), I get the real-time stock price from ', 
                                    strong('Yahoo'), '. Below is a chart trend of LAD stock price from ', textOutput('firstday', inline = TRUE), ' onwards.'), 
                                  br(), 
                                  highchartOutput("hcontainer", height = "500px"), 
                                  bsModal("modalExample", "Data Table", "tabBut", size = "large",
                                          dataTableOutput('distTable')), 
                                  br(), 
                                  h4('Answer'), 
                                  h4('Part I : Stock Price Modelling'), 
                                  p('Here I refer to papers in subtab ', strong('Reference') , ' under tab ', 
                                    strong('Appendix'), 'inside ', strong('MENU'), 
                                    ' bar for stock price prediction as well as the investment fund management. 
                                    You are feel free to read few articles as below for understanding the LASSO 
                                    model (if any).', 
                                    tags$ul(
                                      tags$li(HTML("<a href='http://statweb.stanford.edu/~tibs/lasso/simple.html'>A simple explanation of the Lasso and Least Angle Regression</a>")), 
                                      tags$li(HTML("<a href='http://cos.name/2016/10/data-mining-1-lasso/'>Popular model for data mining (Part I) : Lasso Regression (Chinese)</a>")), 
                                      tags$li(HTML("<a href='http://www4.stat.ncsu.edu/~post/josh/LASSO_Ridge_Elastic_Net_-_Examples.html'>LASSO, Ridge, and Elastic Net</a>")))), 
                                  p('The dataset gather from 2015-01-01 until the latest trading day (unless unable connect to Yahoo and read local saved dataset.)', 
                                    'Here I simply apply Lasso regression as below :', 
                                    withMathJax(helpText('$$Y=\\sum_{j=1}^dX_j\\beta_j+e \\cdots equation\\ 1.1.1$$')), 'Where', 
                                    tags$ul(
                                      tags$li(em('d'), ' is dimension of matrix Xj'), 
                                      tags$li(em('Xo'), ' is baseline'), 
                                      tags$li('A cost function for residuals measurement')
                                    ), 'to make above equation valid.'), 
                                  p('I dont\'t pretend to know the corrected model, the paper', 
                                    em('Sanjiban Sekhar Roy, Dishant Mittal, Avik Basu, and Ajith Abraham (2011)'), 
                                    'using a linear model (gaussian model) with above criteria but here I test 
                                    couples of models (includes 4 models which are \'gaussian\', \'poisson (log link)\', 
                                    \'binomial (logit link)\' and \'multinomial (nominal response)\' but skip 2 
                                    models which are \'cox\' and \'mgaussian\'.) from ', code('glmnet'), ' package to compare among 
                                    them and get the best fit.'), 
                                  p('Besides, I also apply ', code('caret'), ' package to do the comparison as well. You are feel 
                                    free to read through below reference or more over the ', strong('Reference'),' tab : ', 
                                    tags$ul(
                                      tags$li(HTML("<a href='http://topepo.github.io/caret/index.html'>The caret Package</a>")), 
                                      tags$li(HTML("<a href='https://rpubs.com/crossxwill/time-series-cv'>Time Series Cross Validation</a>")))), 
                                  p('Remarks : Not yet test...'), 
                                  tagList(
                                    tags$div(align = "center", 
                                             class = "bg-info", 
                                             tags$h3(class = "bg-primary", "Mean-Squared Error Summary"), 
                                             tags$h5(align = "center", class = "text-muted", 
                                                     "Table Summary of Gaussian Models (Sample)")), dataTableOutput('gsmse')), 
                                  #'@ htmlOutput('gsform'), 
                                  formattableOutput('gsmse1'), 
                                  #'@ tagList(
                                  #'@   tags$div(align = "center", 
                                  #'@            class = "bg-info", 
                                  #'@            tags$h3(class = "bg-primary", "Testing models"), 
                                  #'@            tags$h5(align = "center", class = "text-muted", 
                                  #'@                    "Fitted Value (Sample)")), dataTableOutput('testTable')), 
                                  p('Due to I checked above models and all same (with different ', code('pred.type'), ' or ', code('measure.type') , '). Here I randomly pick one to compare from Ridge to Lasso (0, 0.1, 0.2... 1.0). Above models all get the same result which is alpha ', textOutput('bestalpha', inline = TRUE), ' is the best fit.'), 
                                  highchartOutput("hcmp", height = "500px"), 
                                  br(), 
                                  h4('Part II : Prediction'), 
                                  br(), 
                                  h4('Part III : Staking Model'), 
                                  br(), 
                                  h4('Part IV : Profit & Loss'), 
                                  p('')
                                  ))))), 
                 
                 tabPanel('Q2', 
                          tabsetPanel(
                            tabPanel('Question', 
                                     h4('Answer the Question'), 
                                     p('Simulate the following situation. Attach the code as part of your submission.'), 
                                         br(), 
                                         p('At a post office, customers enter a single line waiting to be served by any one of two clerks. 
											                      Every minute there is a 60% chance that a new customer arrives. If there is no one in line and a 
											                      server is free, the customer does not wait to be served. When a customer is being served there 
											                      is a 25% chance every minute that they complete their business and leave. When the clerk is free 
											                      he will take the next customer in line, in the order that they arrived. Every minute, there is a 
											                      5% chance that a person standing in line will give up and leave. The post office is always open 
											                      (24/7/365).'), 
										                     p('Note: For simplicity you can assume customers will always arrive at the beginning of the minute 
											                      and if they leave they do so at the end of the minute.'), 
										                     br(), 
										                     p(strong('a)'), ' What is the average amount of time a customer spends in the post office (including those not 
											                     served)?'), 
										                     br(), 
										                     p(strong('b)'), ' What percentage of customers leave without being served?'), 
										                     br(), 
										                     p(strong('c)'), ' What percentage of time are the clerks idle?')), 
                            tabPanel('Answer', 
                            p('')))), 
                 
                 tabPanel('Q3', 
                          tabsetPanel(
                            tabPanel('Question', 
                                     h4('Answer the Question'), 
                                     p('Sports teams "A" and "B" are to play each other until one has four wins and is declared the series 
											                 winner. You have $100 to bet on Team A to win the series. You are, however, only allowed to bet on 
											                 individual games, not the final outcome directly, and you must bet a positive amount on each game. 
											                 So, if Team A wins the series, you must walk away with $200, but if Team A loses the series, you 
											                 must walk away with zero, and you must do so having placed a non-zero bet on every game. How do you 
											                 place your bets?')), 
                            tabPanel('Answer', 
                                     p('')))))), 
      
      tabPanel('Appendix', 
               tabsetPanel(
                 tabPanel('Reference', 
                          h4('Reference'), 
                          p('01. ', HTML("<a href='https://github.com/englianhu/binary.com-interview-question/blob/master/reference/Stock%20Market%20Forecasting%20Using%20LASSO%20Linear%20Regression%20Model.pdf'>Stock Market Forecasting Using LASSO Linear Regression Model</a>"), 
                            tags$a(href='https://github.com/scibrokes/owner', target='_blank', 
                                   tags$img(height = '20px', alt='hot', #align='right', 
                                            src='hot.jpg'))), 
                          p('02. ', HTML("<a href='http://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection?answertab=votes#tab-top'>Using LASSO from lars (or glmnet) package in R for variable selection</a>")),
                          p('03. ', HTML("<a href='http://stackoverflow.com/questions/29311323/difference-between-glmnet-and-cv-glmnet-in-r?answertab=votes#tab-top'>Difference between glmnet() and cv.glmnet() in R?</a>")), 
                          p('04. ', HTML("<a href='https://alphaism.wordpress.com/2012/04/13/testing-kelly-criterion-and-optimal-f-in-r/'>Testing Kelly Criterion and Optimal f in R</a>"), 
                            tags$a(href='https://github.com/scibrokes/owner', target='_blank', 
                                   tags$img(height = '20px', alt='hot', #align='right', 
                                            src='hot.jpg'))), 
                          p('05. ', HTML("<a href='https://github.com/scibrokes/kelly-criterion/blob/master/references/Portfolio%20Optimization%20and%20Monte%20Carlo%20Simulation.pdf'>Portfolio Optimization and Monte Carlo Simulation</a>"), 
                            tags$a(href='https://github.com/scibrokes/owner', target='_blank', 
                                   tags$img(height = '20px', alt='hot', #align='right', 
                                            src='hot.jpg'))), 
                          p('06. ', HTML("<a href='https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html'>Glmnet Vignette</a>")), 
                          p('07. ', HTML("<a href='http://cos.name/cn/topic/101533/#post-418215'>How to implement Lasso Algorithm? (Chinese Version)</a>")),
                          p('08. ', HTML("<a href='http://amunategui.github.io/sparse-matrix-glmnet/'>The Sparse Matrix and {glmnet}</a>")),
                          p('09. ', HTML("<a href='https://github.com/englianhu/binary.com-interview-question/blob/master/reference/Regularization%20and%20Variable%20Selection%20via%20the%20Elastic%20Net.pdf'>Regularization and Variable Selection via the Elastic Net</a>")),
                          p('10. ', HTML("<a href='http://www4.stat.ncsu.edu/~post/josh/LASSO_Ridge_Elastic_Net_-_Examples.html'>LASSO, Ridge, and Elastic Net</a>"), 
                            tags$a(href='https://github.com/scibrokes/owner', target='_blank', 
                                   tags$img(height = '20px', alt='hot', #align='right', 
                                            src='hot.jpg'))), 
                          p('11. ', HTML("<a href='https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html'>Popular Data Mining Models for Beginner(1) (Chinese Version)</a>")), 
                          p('12. ', HTML("<a href='http://statweb.stanford.edu/~tibs/lasso.html'>The Lasso Page</a>")), 
                          p('13. ', HTML("<a href='https://api.rpubs.com/Mariano/call'>Call_Valuation.R</a>")), 
                          p('14. ', HTML("<a href='http://zorro-trader.com/manual/en/Lecture%206.htm'>Lecture 6 - Stochastic Processes and Monte Carlo</a>")), 
                          p('15. ', HTML("<a href='http://topepo.github.io/caret/index.html'>The `caret` Package</a>"), 
                            tags$a(href='https://github.com/scibrokes/owner', target='_blank', 
                                   tags$img(height = '20px', alt='hot', #align='right', 
                                            src='hot.jpg'))), 
                          p('16. ', HTML("<a href='https://rpubs.com/crossxwill/time-series-cv'>Time Series Cross Validation</a>"),
                            tags$a(href='https://github.com/scibrokes/owner', target='_blank', 
                                   tags$img(height = '20px', alt='hot', #align='right', 
                                            src='hot.jpg')))), 
                 
                 tabPanel('Applicant', 
                          h4('Applicant\'s CV'), 
                          tags$iframe(src='https://englianhu.github.io/2016/12/ryo-eng.html', height = 800, width = '100%', frameborder = 0))))))),
	
  br(), 
  p('Powered by - Copyright® Intellectual Property Rights of ', 
    tags$a(href='http://www.scibrokes.com', target='_blank', 
           tags$img(height = '20px', alt='hot', #align='right', 
                    src='oda-army.jpg')), 
    HTML("<a href='http://www.scibrokes.com'>Scibrokes®</a>"))))


