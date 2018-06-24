library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel(
        tags$a(href='http://www.binary.com', target='_blank', 
               tags$img(height = '80px', alt='binary', #align='right', 
                        src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/binary-logo-resize.jpg'))
    ),
    mainPanel(
        p(withMathJax(
            helpText('$$I_{t-1}=
                    \\begin{cases}
                    0& \\text{if } r_{t-1} \\leq \\mu\\\\
                    1& \\text{if } r_{t-1} > \\mu
                    \\end{cases}$$')))))


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2] 
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

