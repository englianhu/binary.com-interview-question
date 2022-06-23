filterFX <- function(mbase, currency = 'JPY=X', price = 'Cl') {
  
  cr_code <- c('AUDUSD=X', 'EURUSD=X', 'GBPUSD=X', 'CHF=X', 'CAD=X', 
               'CNY=X', 'JPY=X')
  
  cr_name <- c('AUDUSD', 'EURUSD', 'GBPUSD', 'USDCHF', 'USDCAD', 
               'USDCNY', 'USDJPY')
  
  names(cr_code) <- c('USDAUD', 'USDEUR', 'USDGBP', 'USDCHF', 
                      'USDCAD', 'USDCNY', 'USDJPY')
  
  price_type <- c('Op', 'Hi', 'Lo', 'Cl', 'Ad')
  
  if(!is.xts(mbase)) mbase <- xts(mbase[, -1], order.by = mbase$Date)
  
  if(currency == 'AUDUSD=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; suppressWarnings(rm(`AUDUSD=X`))
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; suppressWarnings(rm(`AUDUSD=X`))
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; suppressWarnings(rm(`AUDUSD=X`))
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; suppressWarnings(rm(`AUDUSD=X`))
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; suppressWarnings(rm(`AUDUSD=X`))
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('AUDUSD=X', 'AUDUSD')
    names(mbase) %<>% str_replace_all(
      '.Open|.High|.Low|.Close|.Volume|.Adjusted', '')
    
  } else if(currency == 'EURUSD=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; suppressWarnings(rm(`EURUSD=X`))
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; suppressWarnings(rm(`EURUSD=X`))
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; suppressWarnings(rm(`EURUSD=X`))
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; suppressWarnings(rm(`EURUSD=X`))
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; suppressWarnings(rm(`EURUSD=X`))
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('EURUSD=X', 'EURUSD')
    names(mbase) %<>% str_replace_all(
      '.Open|.High|.Low|.Close|.Volume|.Adjusted', '')
    
  } else if(currency == 'GBPUSD=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; suppressWarnings(rm(`GBPUSD=X`))
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; suppressWarnings(rm(`GBPUSD=X`))
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; suppressWarnings(rm(`GBPUSD=X`))
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; suppressWarnings(rm(`GBPUSD=X`))
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; suppressWarnings(rm(`GBPUSD=X`))
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('GBPUSD=X', 'GBPUSD')
    names(mbase) %<>% str_replace_all(
      '.Open|.High|.Low|.Close|.Volume|.Adjusted', '')
    
  } else if(currency == 'CHF=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; suppressWarnings(rm(`CHF=X`))
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; suppressWarnings(rm(`CHF=X`))
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; suppressWarnings(rm(`CHF=X`))
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; suppressWarnings(rm(`CHF=X`))
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; suppressWarnings(rm(`CHF=X`))
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('CHF=X', 'USDCHF')
    names(mbase) %<>% str_replace_all(
      '.Open|.High|.Low|.Close|.Volume|.Adjusted', '')
    
  } else if(currency == 'CAD=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; suppressWarnings(rm(`CAD=X`))
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; suppressWarnings(rm(`CAD=X`))
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; suppressWarnings(rm(`CAD=X`))
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; suppressWarnings(rm(`CAD=X`))
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; suppressWarnings(rm(`CAD=X`))
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('CAD=X', 'USDCAD')
    names(mbase) %<>% str_replace_all(
      '.Open|.High|.Low|.Close|.Volume|.Adjusted', '')
    
  } else if(currency == 'CNY=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; suppressWarnings(rm(`CNY=X`))
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; suppressWarnings(rm(`CNY=X`))
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; suppressWarnings(rm(`CNY=X`))
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; suppressWarnings(rm(`CNY=X`))
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; suppressWarnings(rm(`CNY=X`))
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('CNY=X', 'USDCNY')
    names(mbase) %<>% str_replace_all(
      '.Open|.High|.Low|.Close|.Volume|.Adjusted', '')
    
  } else if(currency == 'JPY=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; suppressWarnings(rm(`JPY=X`))
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; suppressWarnings(rm(`JPY=X`))
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; suppressWarnings(rm(`JPY=X`))
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; suppressWarnings(rm(`JPY=X`))
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; suppressWarnings(rm(`JPY=X`))
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('JPY=X', 'USDJPY')
    names(mbase) %<>% str_replace_all(
      '.Open|.High|.Low|.Close|.Volume|.Adjusted', '')
    
  } else {
    stop('Kindly choose common currencies exchange.')
  }
  
  mbase %<>% na.omit
  
  return(mbase)
}

