read_HL_tick_data <- function(dr = 'data/USDJPY/', df.type = 'data.table') {
  
  if(!require('BBmisc')) install.packages('BBmisc')
  suppressPackageStartupMessages(require(BBmisc))
  pkgs <- c('data.table', 'plyr', 'dplyr', 'magrittr', 'purrr', 
            'tidyr', 'stringr', 'lubridate')
  
  suppressAll(lib(pkgs))
  rm(pkgs)
  
  #'@ dr <- 'data/USDJPY/'
<<<<<<< HEAD
  
  ## unzip dataset.
  if(file.exists(paste0(dr, 'USDJPY.zip'))) {
    unzip(paste0(dr, 'USDJPY.zip'), exdir = dr)
  }
  
  res <- ldply(paste0(dr, dir(dr, pattern = '_HL.rds')), readRDS)
=======
  dr1 <- str_replace(dr, '/$', '')
  
  ## unzip dataset.
  if(file.exists(paste0(dr, 'USDJPY.zip'))) {
    #'@ unzip(paste0(dr, 'USDJPY.zip'), exdir = dr)
    unzip(paste0(dr, 'USDJPY.zip'), exdir = dr1)
  }
  
  res <- ldply(paste0(dr, dir(dr1, pattern = '_HL.rds')), readRDS)
>>>>>>> 625a9d893fbce4db5d2e5c9a40098a32d3380b07
  
  if(df.type == 'data.table') {
    res %<>% data.table
  } else if(df.type == 'tbl_df') {
    res %<>% tbl_df
  } else {
    stop("Kindly choose df.type = 'data.table' or df.type = 'tbl_df'.")
  }
  
<<<<<<< HEAD
  file.remove(paste0(dr, dir(dr, pattern = '_HL.rds')))
=======
  file.remove(paste0(dr, dir(dr1, pattern = '_HL.rds')))
  
  res %<>% select(Date, DateTime, Bid, Ask)
>>>>>>> 625a9d893fbce4db5d2e5c9a40098a32d3380b07
  
  return(unique(res))
}
