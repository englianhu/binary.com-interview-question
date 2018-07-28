sim_staking <- function(mbase, init_br = 10000, pnorm_type = 'Bid-Lo', 
                        bid_ask = 'b1-a1', financial_bet = FALSE, 
                        Kelly = 'normal') {
  ## ===================================================================
  ## 1a) pnorm_type = 'Bid-Lo' / pnorm_type = 'Ask-Hi' : Set the Low/High 
  ##      as the x value and High/Low as baseline variance.
  ## 
  ## 1b) pnorm_type = 'Ask-Lo' / pnorm_type = 'Bid-Hi' : Set the High/Low 
  ##      as the x value and Low/High as baseline variance.
  ## 
  ## ===================================================================
  ## Sub argument for pnorm_type above.
  ## 
  ## 2a) bid_ask = 'b1-a1' : use the p_bid1 and p_ask1.
  ## 2b) bid_ask = 'b1-a2' : use the p_bid1 and p_ask2.
  ## 2c) bid_ask = 'b2-a1' : use the p_bid2 and p_ask1.
  ## 2d) bid_ask = 'b2-a2' : use the p_bid2 and p_ask2.
  ## 
  ## ===================================================================
  ## 3a) financial_bet = FALSE : normal trading market can close transaction 
  ##      before closed price.
  ## 3b) financial_bet = TRUE : financial betting market only can awaiting 
  ##      closed price for settlement once placed an oder.
  ## 
  ## ===================================================================
  ## 4a) Kelly = 'normal' : The mean value of variane of forecast highest 
  ##      price and mean value of forecast lowest price.
  ## 
  ## 4b) Kelly = 'adjusted' : Same with Kelly = 'normal' but the 
  ##      difference of both forecast price for open/close transactions.
  ## 
  ## ===================================================================
  
  source('function/sim_predict.R')
  source('function/pnorm_bid_ask.R')
  
  ## execute sim_predict() for price forecasting simulation prior to 
  ##   sim_staking().
  #'@ mbase <- sim_predict(mbase, .getFX = NULL, .from = NULL, .to = NULL, 
  #'@                      timeID0 = ymd(index(mbase)[100]), 
  #'@                      .preCat = 'Op', .preCat2 = NULL, .setPrice = 'Cl', 
  #'@                      currency = 'JPY=X', ahead = 1)
  
  ## tidy dataset.
  mbase <- pnorm_bid_ask(mbase, pnorm_type = pnorm_type, financial_bet = financial_bet)
  
  if (bid_ask == 'b1-a1') {
    mbase %<>% mutate(pB1 = p_ask1, pS1 = p_bid1, pB2 = p_ask2, pS2 = p_bid2)
    
  } else if (bid_ask == 'b1-a2') {
    mbase %<>% mutate(pB1 = p_ask1, pS1 = p_bid2, pB2 = p_ask1, pS2 = p_bid2)
    
  } else if (bid_ask == 'b2-a1') {
    mbase %<>% mutate(pB1 = p_ask2, pS1 = p_bid1, pB2 = p_ask2, pS2 = p_bid1)
    
  } else if (bid_ask == 'b2-a2') {
    mbase %<>% mutate(pB1 = p_ask2, pS1 = p_bid2, pB2 = p_ask1, pS2 = p_bid1)
    
  } else {
    stop("bid_ask = 'b1-a2', bid_ask = 'b1-a2', bid_ask = 'b2-a1', bid_ask = 'b2-a2'.")
  }
  
  ## Kelly model.
  mbase %<>% mutate(BR = init_br) %>% 
    mutate(
      fB1 = 2 * pB1 - 1, fS1 = 2 * pS1 - 1, ##buy and sell
      B1 = pB1 * log(BR * (1 + fB1)) + (1 - pB1) * log(BR * (1 - fB1)), 
      S1 = pS1 * log(BR * (1 + fS1)) + (1 - pS1) * log(BR * (1 - fS1)), 
      Edge1 = ifelse(fB1 > 0, B1, ifelse(fS1 > 0, S1, 0)), 
      
      fB2 = 2 * pB2 - 1, fS2 = 2 * pS2 - 1, ##buy and sell
      B2 = pB2 * log(BR * (1 + fB2)) + (1 - pB2) * log(BR * (1 - fB2)), 
      S2 = pS2 * log(BR * (1 + fS2)) + (1 - pS2) * log(BR * (1 - fS2)), 
      Edge2 = ifelse(fB2 > 0, B2, ifelse(fS2 > 0, S2, 0)))
  
  if(Kelly == 'normal') {
    
    mbase %<>% mutate(
      Buy = ifelse(Fct.Low <= High & Fct.Low >= Low, 1, 0), 
      Sell = ifelse(Fct.High >= Low & Fct.High <= High, 1, 0), 
      Profit = Buy * Edge1 + Sell * Edge2, Bal = BR + cumsum(Profit))
  
  } else if(Kelly == 'adjusted') {
    
    mbase %<>% mutate(
      PF = ifelse(Fct.Low >= Low & Fct.Low <= High, 
                  Fct.Low, 0), #if forecasted place-bet price doesn't 
                                      #  existing within Hi-Lo price, then 
                                      #  the buying action is not stand. 
                                      #  Assume there has no web bandwith 
                                      #  delay. 
      PF2 = ifelse(Fct.High >= Low & Fct.High <= High, 
                   Fct.High, 0), 
      FC = ifelse(Fct.Close >= Low & Fct.Close <= High, 
                  Fct.Close, Close),  #if forecasted settle price doesn't 
                                      #  existing within Hi-Lo price, 
                                      #  then the closing action at 
                                      #  closing price. Assume there has 
                                      #  no web bandwith delay. 
      Buy = ifelse(PF > 0 & FC > PF, 1, 0),  # buy action
      Sell = ifelse(PF > 0 & FC < PF, 1, 0), # sell action
      BuyS = Edge1 * Buy * (Fct.Close - PF), 
      SellS = Edge1 * Sell * (PF - Fct.Close), 
      Profit = BuyS + SellS, Bal = BR + cumsum(Profit))
    
  } else {
    stop("Kindly choose method = 'normal' or method = 'adjusted'.")
  }
  
  return(mbase)
  }

