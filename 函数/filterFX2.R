filterFX2 <- function(mbase, currency = 'JPY=X', price = 'Cl') {
  
  cr_code <- c('AUDUSD=X', 'EURUSD=X', 'GBPUSD=X', 'CHF=X', 'CAD=X', 
               'CNY=X', 'JPY=X')
  
  cr_name <- c('AUDUSD', 'EURUSD', 'GBPUSD', 'USDCHF', 'USDCAD', 
               'USDCNY', 'USDJPY')
  
  price_type <- c('Op', 'Hi', 'Lo', 'Cl', 'Ad')
  
  names(cr_code) <- cr_name
  names(cr_name) <- cr_code
  
  select_FX <- function(mbase, type) {
    switch(type,
           Op = Op(mbase),
           Hi = Hi(mbase),
           Lo = Lo(mbase), 
           Cl = Cl(mbase), 
           Ad = Ad(mbase))
  }
  
  nm <- suppressWarnings(str_extract_all(names(mbase), cr_code)) %>% unlist
  mbase %<>% select_FX(price) %>% na.omit
  names(mbase) %<>% str_replace(cr_code[cr_code == nm], 
                                names(cr_code[cr_code == nm]))
  
  return(mbase)
  
  #> suppressWarnings(filterFX(mb1, currency = 'JPY=X', price = 'Cl'))
  #USDJPY
  #2018-07-09 110.953
  #2018-07-10 110.831
  #2018-07-11 111.958
  #2018-07-12 112.665
  #2018-07-15 112.399
  #2018-07-16 112.374
  #2018-07-17 113.012
  #2018-07-18 112.791
  #2018-07-19 112.363
  #2018-07-22 111.053
  #2018-07-23 111.460
  #2018-07-24 111.241
  #2018-07-26 110.690
  #> suppressWarnings(filterFX2(mb1, currency = 'JPY=X', price = 'Cl'))
  #USDJPY.Close
  #2018-07-09      110.953
  #2018-07-10      110.831
  #2018-07-11      111.958
  #2018-07-12      112.665
  #2018-07-15      112.399
  #2018-07-16      112.374
  #2018-07-17      113.012
  #2018-07-18      112.791
  #2018-07-19      112.363
  #2018-07-22      111.053
  #2018-07-23      111.460
  #2018-07-24      111.241
  #2018-07-26      110.690
  #> microbenchmark(suppressWarnings(filterFX(mb1, currency = 'JPY=X', price = 'Cl')))
  #Unit: milliseconds
  #expr      min       lq
  #suppressWarnings(filterFX(mb1, currency = "JPY=X", price = "Cl")) 1.189229 1.221384
  #mean   median       uq      max neval
  #1.263242 1.270477 1.281448 1.806889   100
  #> microbenchmark(suppressWarnings(filterFX2(mb1, currency = 'JPY=X', price = 'Cl')))
  #Unit: microseconds
  #expr     min      lq
  #suppressWarnings(filterFX2(mb1, currency = "JPY=X", price = "Cl")) 855.669 902.791
  #mean  median      uq      max neval
  #909.2367 908.568 914.715 1364.519   100
  
}
