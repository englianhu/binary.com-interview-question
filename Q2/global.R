# ===== Parameters ======================================
nCst = 15       #number of customers
lmdCst = 1.6667 #inter arrival rate (in minute)
nSrv = 2        #number of clerk
lmdSrv = 4      #service rate (in minute)
nBalk = 1       #balk if queue length is or exceed...
rnTime = 2      #renege if waiting minutes exceed...
startTime = hms('08:00:00')

# ===== ArrRate ======================================
ArrRate <- function(nCst, lmdCst, nSrv, lmdSrv, 
                    startTime, endTime, nBalk, rnTime){
  
  mbase <- data.frame(
    intArrTime = rpois(nCst, lmdCst), 
    intSrvTime = rpois(nCst, lmdSrv)) %>% mutate(
      arrTime = startTime + minutes(cumsum(intArrTime)), 
      endTime = arrTime + minutes(intSrvTime))
  
  mbase$nextAvlTime = c(first(mbase$arrTime), mbase$endTime[-length(mbase$endTime)])
  mbase %<>% mutate(curSrv = ifelse(nextAvlTime <= arrTime, 1, 2)) #current servent
  
  nms <- llply(1:nSrv, function(x) { 
    paste0('Server', x, c('.Start', '.End'))}) %>% unlist
  svr <- matrix(rep(NA, nCst * nSrv * 2), nc = nSrv * 2, 
                dimnames = list(NULL, nms)) %>% tbl_df
  
  svr$Server1.Start = ifelse(mbase$curSrv == 1, 
                             as.character(mbase$arrTime), NA)
  svr$Server1.End = ifelse(mbase$curSrv == 1, 
                           as.character(mbase$endTime), NA)
  svr$Server2.Start = ifelse(mbase$curSrv == 2, 
                             as.character(mbase$arrTime), NA)
  svr$Server2.End = ifelse(mbase$curSrv == 2, 
                           as.character(mbase$endTime), NA)
  
  
  rnRate = rnTime / 1440#(24 hours x 60 minnutes = 1440 minutes)
  
  
  
  
  
  
  return(tmp)
  }

# ===== srvRate ======================================








