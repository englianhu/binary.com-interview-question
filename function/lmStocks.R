lmStocks <- function(mbaseDT) {
  ## ==================== Load Packages ===================================
  # http://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection
  library('glmnet')
  
  ## ==================== Data Validation ===================================
  if(is.data.frame(mbaseDT)) {
    mbaseDT <- mbaseDT
    
  } else if(is.xts(AAPL)) {
    mbaseDT <- mbaseDT %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
      tbl_df %>% mutate(Date = ymd(Date), 
                        AAPL.Volume = formattable::digits(
                          AAPL.Volume, 0, format = 'd', big.mark = ','))
    
  } else {
    stop('Kindly apply filterAAPL and fit the dataset into the function.')
  }
  
  dateID <- mbaseDT$Date
  
  
  
  return()
}