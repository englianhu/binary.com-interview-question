
#pth <- .libPaths(c('/usr/lib/R/library', '/usr/local/lib/R/site-library', '/usr/lib/R/site-library'))
#.libPaths(pth[1:3])

#dir(paste0(R.home(component = "home"), '/etc'))
## https://www.jumpingrivers.com/blog/customising-your-rprofile/

.First <- function() {
  
  if(!require('BBmisc', quietly = TRUE, warn.conflicts = FALSE)) {
    install.packages('BBmisc', dependencies = TRUE, INSTALL_opts = '--no-lock', quiet = TRUE)
  }
  require('BBmisc', quietly = TRUE)
  
  if(!require('devtools', quietly = TRUE, warn.conflicts = FALSE)) {
    install.packages('devtools', dependencies = TRUE, INSTALL_opts = '--no-lock', quiet = TRUE)       
    devtools::install_github('r-lib/devtools', quiet = TRUE)
  }
  
  if(!require('startup', quietly = TRUE, warn.conflicts = FALSE)) {
    ## https://github.com/HenrikBengtsson/startup
    remotes::install_github('HenrikBengtsson/startup', ref = 'develop', quiet = TRUE)
  }
  
  if(!require('Rdym', quietly = TRUE, warn.conflicts = FALSE)) {
    devtools::install_github('wrathematics/Rdym', quiet = TRUE)
  }
  
  ## https://www.jumpingrivers.com/blog/customising-your-rprofile/
  if(!require('rprofile', quietly = TRUE, warn.conflicts = FALSE)) {
    remotes::install_github('csgillespie/rprofile', quiet = TRUE)
  }
  
  if(!require('prompt', quietly = TRUE, warn.conflicts = FALSE)) {
    # Used for nice prompts
    remotes::install_github('gaborcsardi/prompt', quiet = TRUE)
  }
  
  if(!require('colorout', quietly = TRUE, warn.conflicts = FALSE)) {
    # Used for nice colours in the terminal; not for Windows
    remotes::install_github('jalvesaq/colorout', quiet = TRUE)
  }
  
  if(!require('tidyverse', quietly = TRUE, warn.conflicts = FALSE)) {
    devtools::install_github('tidyverse/tidyverse', quiet = TRUE)
  }
  
  if(!require('lubridate', quietly = TRUE, warn.conflicts = FALSE)) {
    devtools::install_github('tidyverse/lubridate', quiet = TRUE)
  }
  
  require('BBmisc', quietly = TRUE, warn.conflicts = FALSE)
  require('devtools', quietly = TRUE, warn.conflicts = FALSE)
  require('lubridate', quietly = TRUE, warn.conflicts = FALSE)
  require('tidyverse', quietly = TRUE, warn.conflicts = FALSE)
  require('rprofile', quietly = TRUE, warn.conflicts = FALSE)
  require('prompt', quietly = TRUE, warn.conflicts = FALSE)
  require('colorout', quietly = TRUE, warn.conflicts = FALSE)
  require('Rdym', quietly = TRUE, warn.conflicts = FALSE)
  require('startup', quietly = TRUE, warn.conflicts = FALSE)
  BBmisc::suppressAll(Rdym::RdymEnable())
  BBmisc::suppressAll(startup::install())
  BBmisc::suppressAll(startup::startup())
  
  if (interactive() && requireNamespace('rprofile', quietly = TRUE)) {
    
    # Only useful if you use Makefiles
    rprofile::create_make_functions()
    
    # Startup options
    rprofile::set_startup_options()
    
    # Not RStudio console
    if (rprofile::is_terminal()) {
      rprofile::set_terminal()
    } else {
      rprofile::set_rstudio()
    }
    
    .env = rprofile::set_functions()
    attach(.env)
    # Display wifi and no of R sessions
    # Linux only
    rprofile::set_startup_info()
  }
  
  # Prints RStudio project on start-up
  setHook('rstudio.sessionInit', function(newSession) {
    active_rproj = rprofile::get_active_rproj()
    if (!is.null(active_rproj)) {
      message(glue::glue("{crayon::yellow('R-project:')} {active_rproj}"))
    }
  }, action = 'append')
}

#.First()
BBmisc::suppressAll(.First())
tryCatch(startup::startup(all = TRUE), error=function(ex) 
  message('.Rprofile error: ', conditionMessage(ex)))

