library('shiny')
library('rdrop2')
library('digest')
library('magrittr')
library('DT')
library('TFX')
library('ggplot2')

#'@ drop_auth()
## email : scibrokes_demo@gmail.com
## pass : trader888
#
# https://github.com/karthik/rdrop2
#
#'@ token <- drop_auth()
#'@ saveRDS(token, "droptoken.rds")
# Upload droptoken to your server
# ******** WARNING ********
# Losing this file will give anyone 
# complete control of your Dropbox account
# You can then revoke the rdrop2 app from your
# dropbox account and start over.
# ******** WARNING ********
# read it back with readRDS
#'@ token <- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
#'@ drop_acc(dtoken = token)
#'@ token <<- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
#'@ drop_acc(dtoken = token)

ui <- shinyServer(fluidPage(
  dataTableOutput('second_column'),
  actionButton('refresh', 'Refresh'), 
  tags$hr(),
  plotOutput("first_column")
))

server <- shinyServer(function(input, output, session){
  # Function to get new observations
  get_new_data <- function(){
    data <- QueryTrueFX()[2, c(2, 3, 6)]
    return(data)
  }
  
  # Initialize my_data
  if(!file.exists('my_data.rds')) {
    my_data <<- get_new_data()
  } else {
    my_data <<- readRDS('my_data.rds')
  }
  
  # Function to update my_data
  update_data <- function(){
    my_data <<- rbind(get_new_data(), my_data)
    saveRDS(my_data, 'my_data.rds')
    return(my_data)
  }
  
  # Plot the 30 most recent values
  output$first_column <- renderPlot({
    #print("Render")
    invalidateLater(1000, session)
    update_data()
    #print(my_data)
    ggplot(head(my_data, 60), aes(TimeStamp)) + 
      geom_line(aes(y = Bid.Price, colour = 'Bid.Price')) + 
      geom_line(aes(y = Ask.Price, colour = 'Ask.Price'))
  })
  
  terms <- reactive({
    input$refresh
    readRDS('my_data.rds')
  })
  
  output$second_column <- renderDataTable({
    terms() %>% datatable(
      caption = "Table : USD/JPY", 
      escape = FALSE, filter = "top", rownames = FALSE, 
      extensions = list("ColReorder" = NULL, "RowReorder" = NULL, 
                        "Buttons" = NULL, "Responsive" = NULL), 
      options = list(dom = 'BRrltpi', autoWidth = TRUE, scrollX = TRUE, 
                     lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')), 
                     ColReorder = TRUE, rowReorder = TRUE, 
                     buttons = list('copy', 'print', 
                                    list(extend = 'collection', 
                                         buttons = c('csv', 'excel', 'pdf'), 
                                         text = 'Download'), I('colvis'))))
  })
})

shinyApp(ui=ui,server=server)