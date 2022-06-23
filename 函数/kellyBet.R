kellyBet <- function(mbase, initialFundSize = 10000){
  
  ## https://quantstrattrader.wordpress.com/2017/09/29/the-kelly-criterion-does-it-work/
  mbase %<>% data.frame
  names(mbase) <- str_replace_all(names(mbase), 'JPY.X', 'USDJPY')
  ## https://github.com/englianhu/binary.com-interview-question/blob/master/function/simStakesGarch.R
  ## 
  # pred.data %>% mutate(ProbB = pnorm(Fct.High, mean = mean(Fct.Low), 
  #                                    sd = sd(Fct.Low)), 
  #                      ProbS = 1 - ProbB, 
  #                      Fct.High = round(Fct.High, 3), 
  #                      Fct.Low = round(Fct.Low, 3)) %>% data.table
  #
  #   LatestDate.GMT Lst.High Lst.Low ForecastDate.GMT Fct.High Fct.Low     ProbB      ProbS
  #1:     2017-06-27  112.399 111.863              T+1  112.547 111.766 0.8280676 0.17193237
  #2:     2017-06-28  112.920 112.154              T+1  113.050 112.085 0.8845984 0.11540162
  #3:     2017-06-29  112.389 111.749              T+1  112.581 111.730 0.8323185 0.16768149
  #4:     2017-07-02  113.417 112.204              T+1  113.424 112.543 0.9170950 0.08290501
  #5:     2017-07-03  113.370 112.779              T+1  113.651 112.893 0.9331140 0.06688596
  #---                                                                                       
  #257:     2018-06-25  110.016 109.389              T+1  110.031 109.579 0.3766011 0.62339887
  #258:     2018-06-26  110.479 109.688              T+1  110.572 109.391 0.4827210 0.51727904
  #259:     2018-06-27  110.411 109.974              T+1  110.396 109.997 0.4478401 0.55215994
  #260:     2018-06-28  110.871 110.388              T+1  110.968 109.856 0.5616675 0.43833251
  #261:     2018-07-01  111.053 110.606              T+1  111.084 110.541 0.5845864 0.41541355
  # 
  # 
  # pred.data %>% mutate(ProbB = pnorm(Fct.Low, mean = mean(Fct.High), 
  #                                    sd = sd(Fct.High)), 
  #                      ProbS = 1 - ProbB, 
  #                      Fct.High = round(Fct.High, 3), 
  #                      Fct.Low = round(Fct.Low, 3)) %>% data.table
  #
  #   LatestDate.GMT Lst.High Lst.Low ForecastDate.GMT Fct.High Fct.Low     ProbB     ProbS
  #1:     2017-06-27  112.399 111.863              T+1  112.547 111.766 0.4753219 0.5246781
  #2:     2017-06-28  112.920 112.154              T+1  113.050 112.085 0.4753219 0.5246781
  #3:     2017-06-29  112.389 111.749              T+1  112.581 111.730 0.4753219 0.5246781
  #4:     2017-07-02  113.417 112.204              T+1  113.424 112.543 0.4753219 0.5246781
  #5:     2017-07-03  113.370 112.779              T+1  113.651 112.893 0.4753219 0.5246781
  #---                                                                                      
  #257:     2018-06-25  110.016 109.389              T+1  110.031 109.579 0.4753219 0.5246781
  #258:     2018-06-26  110.479 109.688              T+1  110.572 109.391 0.4753219 0.5246781
  #259:     2018-06-27  110.411 109.974              T+1  110.396 109.997 0.4753219 0.5246781
  #260:     2018-06-28  110.871 110.388              T+1  110.968 109.856 0.4753219 0.5246781
  #261:     2018-07-01  111.053 110.606              T+1  111.084 110.541 0.4753219 0.5246781
  # 
  # 
  # pred.data %>% mutate(
  #   ProbB = pnorm(Fct.Low, mean = mean(Lst.High), sd = sd(Lst.High)), 
  #   ProbS = 1 - ProbB, Fct.High = round(Fct.High, 3), 
  #   Fct.Low = round(Fct.Low, 3)) %>% data.table
  #
  #   LatestDate.GMT Lst.High Lst.Low ForecastDate.GMT Fct.High Fct.Low     ProbB     ProbS
  #1:     2017-06-27  112.399 111.863              T+1  112.547 111.766 0.6829508 0.3170492
  #2:     2017-06-28  112.920 112.154              T+1  113.050 112.085 0.7296487 0.2703513
  #3:     2017-06-29  112.389 111.749              T+1  112.581 111.730 0.6774416 0.3225584
  #4:     2017-07-02  113.417 112.204              T+1  113.424 112.543 0.7901496 0.2098504
  #5:     2017-07-03  113.370 112.779              T+1  113.651 112.893 0.8303839 0.1696161
  #---                                                                                      
  #257:     2018-06-25  110.016 109.389              T+1  110.031 109.579 0.3242934 0.6757066
  #258:     2018-06-26  110.479 109.688              T+1  110.572 109.391 0.2961039 0.7038961
  #259:     2018-06-27  110.411 109.974              T+1  110.396 109.997 0.3906327 0.6093673
  #260:     2018-06-28  110.871 110.388              T+1  110.968 109.856 0.3677742 0.6322258
  #261:     2018-07-01  111.053 110.606              T+1  111.084 110.541 0.4816531 0.5183469
  #
  # pred.data %>% mutate(
  #   ProbB = pnorm(Fct.Low, mean = mean(Fct.Low), sd = sd(Fct.Low)), 
  #   ProbS = 1 - ProbB, Fct.High = round(Fct.High, 3), 
  #   Fct.Low = round(Fct.Low, 3)) %>% data.table
  #
  #   LatestDate.GMT Lst.High Lst.Low ForecastDate.GMT Fct.High Fct.Low     ProbB     ProbS
  #1:     2017-06-27  112.399 111.863              T+1  112.547 111.766 0.7106333 0.2893667
  #2:     2017-06-28  112.920 112.154              T+1  113.050 112.085 0.7626865 0.2373135
  #3:     2017-06-29  112.389 111.749              T+1  112.581 111.730 0.7044035 0.2955965
  #4:     2017-07-02  113.417 112.204              T+1  113.424 112.543 0.8275657 0.1724343
  #5:     2017-07-03  113.370 112.779              T+1  113.651 112.893 0.8685511 0.1314489
  #---                                                                                      
  #257:     2018-06-25  110.016 109.389              T+1  110.031 109.579 0.2943248 0.7056752
  #258:     2018-06-26  110.479 109.688              T+1  110.572 109.391 0.2627786 0.7372214
  #259:     2018-06-27  110.411 109.974              T+1  110.396 109.997 0.3701970 0.6298030
  #260:     2018-06-28  110.871 110.388              T+1  110.968 109.856 0.3438371 0.6561629
  #261:     2018-07-01  111.053 110.606              T+1  111.084 110.541 0.4765542 0.5234458
  
  mbase %<>% mutate(
    ProbB = pnorm(Fct.Low, mean = mean(Fct.High), sd = sd(Fct.High)), 
    ProbS = 1 - ProbB, Fct.High = round(Fct.High, 3), 
    Fct.Low = round(Fct.Low, 3)) %>% data.table
  
  # Kelly criterion
  # Advantages = (prob of win * decimal odds) + (prob of lose * -1)
  # Optimal Kelly wager % = Advantages / decimal odds
  mbase$Adv <- (mbase$EMprob * mbase$COMOdds) + ((1-mbase$EMprob) * -1)
  mbase$Staking <- mbase$Adv / mbase$COMOdds
  mbase$Staking <- ifelse(mbase$Staking < 0, 0, mbase$Staking)
  
  return(res)
}
