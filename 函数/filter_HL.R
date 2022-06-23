## filter_HL() to get daily unique high low price.
filter_HL <- function(mbase) {
  ## filter to be unique min bid, max bid and also min ask, max ask price.
  B.Min <- ddply(mbase, .(Date), summarise, 
                 Bid = min(Bid))
  B.Min.Date <- ddply(mbase, .(Date), summarise, 
                      DateTime = DateTime[which.min(Bid)])
  B.Min <- join(B.Min, B.Min.Date, by = 'Date')
  rm(B.Min.Date)
  
  B.Max <- ddply(mbase, .(Date), summarise, 
                 Bid = max(Bid))
  B.Max.Date <- ddply(mbase, .(Date), summarise, 
                      DateTime = DateTime[which.max(Bid)])
  B.Max <- join(B.Max, B.Max.Date, by = 'Date')
  rm(B.Max.Date)
  
  A.Min <- ddply(mbase, .(Date), summarise, 
                 Ask = min(Ask))
  A.Min.Date <- ddply(mbase, .(Date), summarise, 
                      DateTime = DateTime[which.min(Ask)])
  A.Min <- join(A.Min, A.Min.Date, by = 'Date')
  rm(A.Min.Date)
  
  A.Max <- ddply(mbase, .(Date), summarise, 
                 Ask = max(Ask))
  A.Max.Date <- ddply(mbase, .(Date), summarise, 
                      DateTime = DateTime[which.max(Ask)])
  A.Max <- join(A.Max, A.Max.Date, by = 'Date')
  rm(A.Max.Date)
  
  res <- bind_rows(list(B.Min, B.Max, A.Min, A.Max)) %>% arrange(DateTime)
  rm(B.Min, B.Max, A.Min, A.Max)
  
  return(res)
}
