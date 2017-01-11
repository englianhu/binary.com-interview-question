## ========= Setup Options =================================
## Setup Options, Loading Required Libraries and Preparing Environment
## Loading the packages and setting adjustment


## ========= ShinyServer ================================
# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  # Simulate work being done for 1 second
  Sys.sleep(1)
  
  # Hide the loading message when the rest of the server function has executed
  #'@ hide(id = 'loading-content', anim = TRUE, animType = 'fade')   
  #'@ show(id = 'app-content', anim = TRUE, animType = 'fade')
  
  onclick('toggleAdvanced', shinyjs::toggle(id = 'advanced', anim = TRUE, animType = 'fade'))    
  onclick('update', shinyjs::html('time', date()))
  
})