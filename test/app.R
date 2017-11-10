
library('shiny')
library('rdrop2')
library('digest')

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
token <<- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
drop_acc(dtoken = token)

# Define the fields we want to save from the form
fields <- c("name", "used_shiny", "r_num_years")
outputDir <- "responses"

ui <- fluidPage(
  titlePanel("Algorithmic Trading System"),
  mainPanel(
    DT::dataTableOutput("responses", width = 300), tags$hr(),
    textInput("name", "Name", ""),
    checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
    sliderInput("r_num_years", "Number of years using R",
                0, 25, 2, ticks = FALSE),
    actionButton("submit", "Submit")
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  saveData <- function(data) {
    data <- t(data)
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), fileName)
    write.csv(data, filePath, row.names = FALSE, quote = TRUE)
    # Upload the file to Dropbox
    drop_upload(filePath, path = outputDir)
  }
  
  loadData <- function() {
    # Read all the files into a list
    filesInfo <- drop_dir(outputDir)
    filePaths <- filesInfo$path_display
    data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
    data
  }
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(formData())
  })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$responses <- DT::renderDataTable({
    input$submit
    loadData()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

