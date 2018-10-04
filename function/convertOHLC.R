convertOHLC <- function(mbase, .unit = 'minute') {
  
  require('BBmisc')
  pkgs <- c('magrittr', 'lubridate', 'tidyverse', 'tidyquant')
  lib(pkgs)
  rm(pkgs)
  
  # to.period()
  # Valid period character strings include: "seconds", "minutes", 
  #   "hours", "days", "weeks", "months", "quarters", and "quarters". 
  # These are calculated internally via endpoints. See that function's 
  #   help page for further details.
  
  # https://github.com/business-science/tidyquant/issues/92#issuecomment-426126637
  mbaseA <- suppressAll(
    mbase %>% 
      dplyr::select(DateTime, Ask) %>% 
      mutate(DateTime = as.POSIXct(mdy_hms(DateTime))) %>% 
      tk_xts(.) %>% to.period(period = .unit) %>% 
      tk_tbl() %>% dplyr::rename_all(str_replace_all, '\\.', '') %>% 
      dplyr::rename_all(tolower))
  
  mbaseB <- suppressAll(
    mbase %>% 
      dplyr::select(DateTime, Bid) %>% 
      mutate(DateTime = as.POSIXct(mdy_hms(DateTime))) %>% 
      tk_xts(.) %>% to.period(period = .unit) %>% 
      tk_tbl() %>% dplyr::rename_all(str_replace_all, '\\.', '') %>% 
      dplyr::rename_all(tolower))
  
  tmp <- list(Ask = mbaseA, Bid = mbaseB)
  return(tmp)
  }
