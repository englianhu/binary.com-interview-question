## ========= Setup Options =================================
## Setup Options, Loading Required Libraries and Preparing Environment
## Loading the packages and setting adjustment

#'@ appCSS <- "
#'@ #loading-content {
#'@ position: absolute;
#'@ opacity: 0.9;
#'@ z-index: 100;
#'@ top: 0;
#'@ bottom: 0;
#'@ left: 0;
#'@ right: 0;
#'@ height: 100%;
#'@ text-align: center;
#'@ background: url(loader.gif) center no-repeat #fff;
#'@ }
#'@ "

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
#'@   inlineCSS(appCSS),
  
  # Loading message
#'@ div(id = 'loading-content'),
  
  # The main app code goes here
#'@ hidden(
  div(id = 'app-content',
      titlePanel(
        tags$a(href='https://www.binary.com/ja/home.html', target='_blank', 
               tags$img(height = '20px', alt='hot', #align='right', 
                        src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/binary-logo-resize.jpg'))), 
      navbarPage('Shiny Apps', 
                 tabPanel('Binary Questionaire', 
                          # Sidebar with a slider input for number of bins 
                          sidebarLayout(
                            sidebarPanel(
                              br(),
                              a(id = 'toggleAdvanced', 'Show/hide advanced info', href = '#'),
                              hidden(
                                div(id = 'advanced', 
                                    p('- Author Profile:', HTML("<a href='https://englianhu.github.io/2016/12/ryo-eng.html'>RYO, ENG Lian Hu</a>")),
                                    p('- GitHub:', HTML("<a href='https://github.com/scibrokes/betting-strategy-and-model-validation'>Source Code</a>")),
                                    br(),
                                    p('Timestamp: ', 
                                      span(id = 'time', base::date()), 
                                      a(id = 'update', 'Update', href = '#')), 
                                    actionButton('reset', 'Reset form'), style = 'info'))),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel('Questionaire',
                                         tabsetPanel(
                                           tabPanel('Questions', 
                                                    h4('Betting Strategy and Model Validation'), 
                                                    p('You are feel free to read the source of article through', 
                                                      HTML("<a href='https://en.wikipedia.org/wiki/Kelly_criterion'>Kelly Criterion</a>"), '. Below is the research paper wrote by John Kelly.'), 
                                                    HTML('<iframe src=\"https://render.githubusercontent.com/view/pdf?commit=700f2682cccc0fc4e41924c4a555a342adb2af79&enc_url=68747470733a2f2f7261772e67697468756275736572636f6e74656e742e636f6d2f656e676c69616e68752f62696e6172792e636f6d2d696e746572766965772d7175657374696f6e2f373030663236383263636363306663346534313932346334613535356133343261646232616637392f7265666572656e63652f7175616e742d616e616c7973742d736b696c6c732d746573742e706466&nwo=englianhu%2Fbinary.com-interview-question&path=reference%2Fquant-analyst-skills-test.pdf&repository_id=78606262#c29a9079-71e1-487c-82e4-dab75fa2942e" width=\"900\" height=\"600\"></iframe>'),
                                                    imageOutput('imp_pdf', width = '500px', height = '800px')), 
                                           tabPanel('Q1',
                                                    h4('John Larry Kelly (1956) - Kelly criterion'), 
                                                    p('In probability theory and intertemporal portfolio choice, the ', 
                                                      strong('Kelly criterion, Kelly strategy, Kelly formula'), ', or', 
                                                      strong('Kelly bet'), ', is a formula used to determine the optimal 
                                                      size of a series of bets. In most gambling scenarios, and some 
                                                      investing scenarios under some simplifying assumptions, the Kelly 
                                                      strategy will do better than any essentially different strategy in the long run (that is, over a span 
                                                      of time in which the observed fraction of bets that are 
                                                      successful equals the probability that any given bet will be successful). It was described by J. L. Kelly, Jr, a 
                                                      researcher at Bell Labs, in 1956. The practical use of the formula has been demonstrated.')),
                                           tabPanel('Q2', 
                                                    h4('Niko Marttinen (2001)'), 
                                                    p('This is a Master Degree thesis by the author. He apply few steps to test the odds modelling and application of Kelly model. The thesis is similar with my previous research ', 
                                                      HTML("<a href='https://github.com/scibrokes/odds-modelling-and-testing-inefficiency-of-sports-bookmakers'>Odds Modelling and Testing Inefficiency of Sports Bookmakers</a>"), 
                                                      ' and ', em('Dixon and Coles (1996)'), ' but more sophiscated : '), 
                                                    p('- Firstly, using few statistical algorithm calculation for odds modelling.'), 
                                                    p('- Secondly, using Kelly model for portfolio management.'), 
                                                    p('- Lastly, using Monte Carlo Markov Chain (MCMC) to test the optimal staking portfolio/model.')),
                                           tabPanel('Q3', 
                                                    h4('Fabián Enrique Moya (2012)'), 
                                                    p('The paper...')))))))),
                 
                 ## Section 2 Kelly Portfolio Management
                 tabPanel('Portfolio Management', h4('Investment Fund Portfolio Management'), 
                          tags$iframe(src='https://github.com/scibrokes/kelly-criterion', height = 800, width = 500, frameborder = 0, seamless = 'seamless')))),
  br(), 
  p('Powered by - Copyright® Intellectual Property Rights of ', 
    tags$a(href='http://www.scibrokes.com', target='_blank', 
           tags$img(height = '20px', alt='hot', #align='right', 
                    src='https://raw.githubusercontent.com/scibrokes/betting-strategy-and-model-validation/master/regressionApps/oda-army.jpg')), 
    HTML("<a href='http://www.scibrokes.com'>Scibrokes®</a>"))))


