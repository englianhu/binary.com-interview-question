## https://stackoverflow.com/questions/33480302/creating-a-shiny-app-with-real-time-data

library('shiny')
library('magrittr')
library('ggplot2')
library('lubridate')
library('quantmod')
library('plyr')
library('dplyr')


ui <- shinyServer(fluidPage(
  plotOutput('realPlot')
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
  
  # Initialize realData
  realData <<- get_new_data() #ldply(1:60, get_new_data()) %>% tbl_df
  
  # Function to update realData
  update_data <- function(){
    realData <<- rbind(realData, get_new_data())
  }
  
  # Plot the 30 most recent values
  output$realPlot <- renderPlot({
    #'@ print("Render")
    invalidateLater(1000, session)
    update_data()
    #'@ print(realData)
    
    if(nrow(realData) > 60) {
      realData <- realData[((nrow(realData) - 59):nrow(realData)),]
    } else {
      realData <- realData
    }
    ggplot() + geom_line(aes(x = index(realData), y = coredata(realData)), 
                         colour = 'blue') + xlab('Time [Seconds]') + ylab('USD/JPY')
  })
})

shinyApp(ui=ui,server=server)