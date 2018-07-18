library('shiny')
library('shinyTime')
library('lubridate')
library('magrittr')
library('stringr')
library('tcltk2')

shinyUI <- fluidPage(
    
    titlePanel(c('shinyTime Example App', 
                 div(img(src = 'ENG.jpg', width = '40', align = 'right'), 
                     img(src = 'RYO.jpg', width = '20', align = 'right')))), 
    sidebarLayout(
        sidebarPanel(
            timeInput('time_input', 'Enter time (GMT)', 
                      value = strptime('12:34:56', '%T')), 
            actionButton('to_current_time', 'Current time')),
        
        mainPanel(
            
            textOutput('time_output'), 
            textOutput('curTime'), 
            tableOutput('fTable'))))

shinyServer <- function(input, output, session) {
    
    parse_time <- function(x) {
        
        y <- x %>% str_split(' ') %>% .[[1]] %>% .[2]
        z <- str_extract_all(y, '[0-9]{2}')[[1]]
        
        if(length(z) == 2) z <- z
        if(length(z) == 3) z <- z[1:2]
        
        return()
    }
    
    tbl <- sample(1:50, 20, replace = TRUE) %>% matrix(ncol = 4)
    
    output$time_output <- renderText({
        paste(strftime(input$time_input, '%T'), '(GMT)')
        })
    
    observeEvent(input$to_current_time, {
        updateTimeInput(session, 'time_input', value = now('GMT'))
    })
    
    output$curTime <- renderText({
        invalidateLater(1000, session)
        as.character(now('GMT'))
    })
    
    #'@ observeEvent((str_split_fixed(tm, ' ', 3) %>% .[,2]) == as.character(input$time_input), {
    #'@     output$fTable <- renderTable(sample(1:50, 20) %>% matrix(ncol = 4))
    #'@ })
    
    obs <- reactive({
        invalidateLater(1000, session)
        now('GMT')
    })
    
    starttime <- now('GMT')
    
    observeEvent(TRUE, {
        invalidateLater(1000, session)
        ## as.character(obs) == as.character(input$time_input)
        ## https://stackoverflow.com/questions/36086599/how-do-i-run-a-function-every-second
        #'@ starttime <- now('GMT')
        while(TRUE){
            if(abs(as.numeric(now('GMT') - starttime) %% 1) < .001){
                # my function that takes less than a second to run
                Sys.sleep(runif(1, min = 0, max = 0.2))
                #cat(paste0('It ran ', now('GMT'), '.\n'))
                output$fTable <- renderTable(sample(1:50, 20) %>% matrix(ncol = 4))
            }
        }
        
        #as.character(obs) == as.character(input$time_input)
        #if(now('GMT') == input$time_input)
        #output$fTable <- renderTable(sample(1:50, 20) %>% matrix(ncol = 4))
    })
    
}

#'@ shinyApp(ui, server)
shinyApp(shinyUI, shinyServer)

##str_extract_all('12:23:24', '[0-9]{2}')[[1]]
