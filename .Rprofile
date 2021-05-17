## ======================== Micro Editor ===================================

## https://www.tecmint.com/micro-linuxtext-editor-with-syntax-highlighting/
## Micro – A Modern Terminal Based Text Editor with Syntax Highlighting

## ============================= PATH ======================================

#pth <- .libPaths(c('/usr/lib/R/library', '/usr/local/lib/R/site-library', '/usr/lib/R/site-library'))
#.libPaths(pth[1:3])

#dir(paste0(R.home(component = "home"), '/etc'))
## https://www.jumpingrivers.com/blog/customising-your-rprofile/

## -------------------------------------------------------------------------

## https://stackoverflow.com/a/13736073/3806250
# candidates <- c( Sys.getenv("R_PROFILE"),
# file.path(Sys.getenv("R_HOME"), "etc", "Rprofile.site"),
# Sys.getenv("R_PROFILE_USER"),
# file.path(getwd(), ".Rprofile") )
# 
# Filter(file.exists, candidates)

## https://github.com/rstudio/reticulate/issues/496#issuecomment-601446838
#Sys.setenv(RETICULATE_PYTHON = '/usr/bin/python3')
#Sys.setenv(RETICULATE_PYTHON = '~/anaconda3/bin/python')
Sys.setenv(RETICULATE_PYTHON = '~/anaconda3/bin/python3')
## -------------------------------------------------------------------------

## We set the cloud mirror, which is 'network-close' to everybody, as default
local({
  r <- getOption('repos')
  r['CRAN'] <- 'https://cloud.r-project.org'
  options(repos = r)
})

## ======================== Load Packages ==================================

if(!require('BBmisc', quietly = TRUE, warn.conflicts = FALSE)) {
  utils::install.packages('BBmisc', dependencies = TRUE, 
                          INSTALL_opts = '--no-lock')
}
require('BBmisc', quietly = TRUE)

if(!require('devtools', quietly = TRUE, warn.conflicts = FALSE)) {
  utils::install.packages('devtools', dependencies = TRUE, 
                          INSTALL_opts = '--no-lock')       
  devtools::install_github('r-lib/devtools')
}

if(!require('startup', quietly = TRUE, warn.conflicts = FALSE)) {
  ## https://github.com/HenrikBengtsson/startup
  remotes::install_github('HenrikBengtsson/startup', ref = 'develop')
}

if(!require('Rdym', quietly = TRUE, warn.conflicts = FALSE)) {
  devtools::install_github('wrathematics/Rdym')
}

## https://www.jumpingrivers.com/blog/customising-your-rprofile/
if(!require('rprofile', quietly = TRUE, warn.conflicts = FALSE)) {
  remotes::install_github('csgillespie/rprofile')
}

if(!require('prompt', quietly = TRUE, warn.conflicts = FALSE)) {
  # Used for nice prompts
  remotes::install_github('gaborcsardi/prompt')
}

if(!require('colorout', quietly = TRUE, warn.conflicts = FALSE)) {
  # Used for nice colours in the terminal; not for Windows
  remotes::install_github('jalvesaq/colorout')
}

if(!require('tidyverse', quietly = TRUE, warn.conflicts = FALSE)) {
  devtools::install_github('tidyverse/tidyverse')
}

if(!require('lubridate', quietly = TRUE, warn.conflicts = FALSE)) {
  devtools::install_github('tidyverse/lubridate')
}

require('BBmisc', quietly = TRUE, warn.conflicts = FALSE)
require('MASS', quietly = TRUE, warn.conflicts = FALSE)
require('devtools', quietly = TRUE, warn.conflicts = FALSE)
require('lubridate', quietly = TRUE, warn.conflicts = FALSE)
BBmisc::suppressAll(require('tidyverse', quietly = TRUE, warn.conflicts = FALSE))
require('rprofile', quietly = TRUE, warn.conflicts = FALSE)
require('prompt', quietly = TRUE, warn.conflicts = FALSE)
require('colorout', quietly = TRUE, warn.conflicts = FALSE)
require('Rdym', quietly = TRUE, warn.conflicts = FALSE)
require('startup', quietly = TRUE, warn.conflicts = FALSE)

## -------------------------------------------------------------------------

## https://stackoverflow.com/a/17486231/3806250
#.First()
.First <- function() {
  Rdym::RdymEnable()
  BBmisc::suppressAll(startup::install())
  BBmisc::suppressAll(startup::startup())
}

## ======================== Start Up =====================================

## https://www.jumpingrivers.com/blog/customising-your-rprofile/
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
  suppressAll(attach(.env))
  # Display wifi and no of R sessions
  # Linux only
  rprofile::set_startup_info()
}

## -------------------------------------------------------------------------

# Prints RStudio project on start-up
setHook('rstudio.sessionInit', function(newSession) {
  active_rproj = rprofile::get_active_rproj()
  if (!is.null(active_rproj)) {
    message(glue::glue("{crayon::yellow('R-project:')} {active_rproj}"))
  }
}, action = 'append')

#BBmisc::suppressAll(.First())
tryCatch(startup::startup(all = TRUE), error=function(ex) 
  message('.Rprofile error: ', conditionMessage(ex)))
