## https://stackoverflow.com/questions/33480302/creating-a-shiny-app-with-real-time-data

library(shiny)
library(magrittr)

ui <- shinyServer(fluidPage(
  plotOutput("first_column")
))

server <- shinyServer(function(input, output, session){
  # Function to get new observations
  get_new_data <- function(){
    data <- getSymbols('JPY=X', from = today('Asia/Tokyo'), 
                       auto.assign = FALSE) %>% Cl
    names(data) <- 'USDJPY'
    index(data) <- now('Asia/Tokyo')
    return(data)
  }
  
  # Initialize my_data
  my_data <<- get_new_data()
  
  # Function to update my_data
  update_data <- function(){
    my_data <<- rbind(get_new_data(), my_data)
  }
  
  # Plot the 30 most recent values
  output$first_column <- renderPlot({
    print("Render")
    invalidateLater(1000, session)
    update_data()
    print(my_data)
    #'@ plot(USDJPY ~ 1, data = my_data[((nrow(my_data) - 60):nrow(my_data)),], type = 'l')
    my_data <- data.frame(datetime = index(my_data[((nrow(my_data) - 60):nrow(my_data)),]), coredata(my_data[((nrow(my_data) - 60):nrow(my_data)),]))
    ggplot() + geom_line(aes(x = my_data[,1], y = my_data[,2]), colour = 'blue') + xlab('Time [s]') + ylab('Channel')
  })
})

shinyApp(ui=ui,server=server)