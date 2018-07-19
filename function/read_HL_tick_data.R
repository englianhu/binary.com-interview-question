read_HL_tick_data <- function(dr = 'data/USDJPY/', df.type = 'data.table') {
  
  if(!require('BBmisc')) install.packages('BBmisc')
  suppressPackageStartupMessages(require(BBmisc))
  pkgs <- c('data.table', 'plyr', 'dplyr', 'magrittr', 'purrr', 
            'tidyr', 'stringr', 'lubridate')
  
  suppressAll(lib(pkgs))
  rm(pkgs)
  
  #'@ dr <- 'data/USDJPY/'
  dr1 <- str_replace(dr, '/$', '')
  
  ## unzip dataset.
  if(file.exists(paste0(dr, 'USDJPY.zip'))) {
    #'@ unzip(paste0(dr, 'USDJPY.zip'), exdir = dr)
    unzip(paste0(dr, 'USDJPY.zip'), exdir = dr1)
  }
  
  res <- ldply(paste0(dr, dir(dr1, pattern = '_HL.rds')), readRDS)
  
  if(df.type == 'data.table') {
    res %<>% data.table
  } else if(df.type == 'tbl_df') {
    res %<>% tbl_df
  } else {
    stop("Kindly choose df.type = 'data.table' or df.type = 'tbl_df'.")
  }
  
  file.remove(paste0(dr, dir(dr1, pattern = '_HL.rds')))
  
  res %<>% select(Date, DateTime, Bid, Ask)
  
  return(unique(res))
}
