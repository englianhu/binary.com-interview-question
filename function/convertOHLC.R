convertOHLC <- function(mbase, combine = FALSE, mean = FALSE, 
                        tz = 'Europe/Athens', .unit = 'minute') {
  
  require('BBmisc')
  pkgs <- c('magrittr', 'lubridate', 'tidyverse', 'tidyquant')
  lib(pkgs)
  rm(pkgs)
  
  .unit2 <- .unit %>% str_replace_all('s$', '')
  
  # to.period()
  # Valid period character strings include: "seconds", "minutes", 
  #   "hours", "days", "weeks", "months", "quarters", and "quarters". 
  # These are calculated internally via endpoints. See that function's 
  #   help page for further details.
  
  # https://github.com/business-science/tidyquant/issues/92#issuecomment-426126637
  mbaseA <- suppressAll(
    mbase %>% 
      dplyr::select(DateTime, Ask) %>% 
      mutate(DateTime = .POSIXct(mdy_hms(DateTime), tz = tz)) %>% 
      tk_xts(.) %>% to.period(period = .unit) %>% tk_tbl() %>% 
      mutate(index = round_date(index, .unit2)) %>% #floor_date
      dplyr::rename_all(str_replace_all, '\\.', '') %>% 
      dplyr::rename_all(tolower))
  
  nch <- mbaseA$index[1] %>% substr(nchar(.)+2, nchar(.)+3)
  mbaseA %<>% ## https://stackoverflow.com/questions/6668963/how-to-prevent-ifelse-from-turning-date-objects-into-numeric-objects
      mutate(nch = nch, index = if_else(nch == '23', index + hours(1), index)) %>% 
      dplyr::select(-nch)
  
  mbaseB <- suppressAll(
    mbase %>% 
      dplyr::select(DateTime, Bid) %>% 
      mutate(DateTime = .POSIXct(mdy_hms(DateTime), tz = tz)) %>% 
      tk_xts(.) %>% to.period(period = .unit) %>% tk_tbl() %>% 
      mutate(index = round_date(index, .unit2)) %>% #floor_date
      dplyr::rename_all(str_replace_all, '\\.', '') %>% 
      dplyr::rename_all(tolower))
  
  nch <- mbaseB$index[1] %>% substr(nchar(.)+2, nchar(.)+3)
  mbaseB %<>% ## https://stackoverflow.com/questions/6668963/how-to-prevent-ifelse-from-turning-date-objects-into-numeric-objects
    mutate(nch = nch, index = if_else(nch == '23', index + hours(1), index)) %>% 
    dplyr::select(-nch)
  
  if (combine == TRUE) {
    
    if (mean == TRUE) {
      mbaseA %<>% cbind(Type = 'Ask', .)
      mbaseB %<>% cbind(Type = 'Bid', .)
      tmp <- rbind(mbaseA, mbaseB) %>% tbl_df %>% 
        dplyr::select(index, Type, open, high, low, close) %>% 
        arrange(index)
      
      tmp %<>% ddply(.(index), summarise, 
                     open = mean(open, na.rm=TRUE), 
                     high = mean(high, na.rm=TRUE), 
                     low = mean(low, na.rm=TRUE), 
                     close = mean(close, na.rm=TRUE)) %>% tbl_df
    } else {
      
      tmp <- data_frame(
        open = (mbaseA$open + mbaseB$open)/2, 
        high = mbaseB$high, 
        low = mbaseA$low, 
        close = (mbaseA$open + mbaseB$open)/2) %>% tbl_df
    }
    
  } else {
    tmp <- list(Ask = mbaseA, Bid = mbaseB)
  }
  
  return(tmp)
  }
