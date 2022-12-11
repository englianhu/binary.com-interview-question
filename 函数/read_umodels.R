read_umodels <- function(cr_code, gmds, mbase, .print = FALSE) {
  fx <- ldply(names(cr_code), function(x) {
    tmID <- index(mbase[[x]])
    tmID <- tmID[tmID >= ymd('2013-01-01') & 
                   tmID <= ymd('2017-08-30')]
    
    dfm <- ldply(gmds, function(y) {
      y2 <- ifelse(y == 'gjrGARCH', 'pred2', y)
      
      ldply(tmID, function(z) {
        txt <- paste0('data/fx/', x, '/', y2, '.', z, '.rds')
        tryCatch(readRDS(txt) %>% tbl_df, error = function(e) {
          if (.print == TRUE) cat(paste(txt, 'error, no such file.\n')) else NULL
          })
      }) %>% data.frame(Model = factor(y), .) %>% tbl_df
      
    }) %>% data.frame(.id = factor(x), .) %>% tbl_df
    
    names(dfm)[5:6] <- c('Price', 'Price.T1')
    dfm
  }) %>% tbl_df
  return(fx)
  }
