sim_staking <- function(mbase, init_br = 10000, pnorm_type = 'Bid-Lo', 
                        bid_ask = '1-1', forex_market = TRUE, 
                        hedge = FALSE, Kelly = 'normal') {
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
  ## 3a) forex_market = TRUE : normal trading market can close transaction 
  ##      before closed price.
  ## 3b) forex_market = FALSE : financial betting market only can awaiting 
  ##      closed price for settlement once placed an oder.
  ## 
  ## ===================================================================
  ## There will be 2 limit orders placed. One for buy and one for sell, 
  ##  hedge = TRUE will stand the 2nd limit order even though the 1st 
  ##  limit order has open transaction.
  ## Applicable for both financial betting and also FOREX trading 
  ##  market but there has a criteria which is only predict by using 
  ##  fcHi-fcCl and fcLo-fcCl to minimise the risk. The Kelly ratio 
  ##  will be based on forecast-Cl but real-Cl will be settled price. 
  ## It will be .
  ## 
  ## 4a) hedge = TRUE : .
  ## 
  ## 4b) hedge = FALSE : .
  ## 
  ## ===================================================================
  ## 5a) Kelly = 'normal' : The mean value of variane of forecast highest 
  ##      price and mean value of forecast lowest price.
  ## 
  ## 5b) Kelly = 'adjusted' : Same with Kelly = 'normal' but the 
  ##      difference of both forecast price for open/close transactions.
  ## 
  ## ===================================================================
  
  source('function/pnorm_bid_ask.R')
  
  ## tidy dataset.
  mbase <- pnorm_bid_ask(mbase, pnorm_type = pnorm_type)
  
  mbase %<>% mutate(BR = init_br) %>% 
    mutate(
      fB1 = 2 * pB1 - 1, fS1 = 2 * pS1 - 1, ##
      B1 = pB1 * log(BR * (1 + fB1)) + (1 - pB1) * log(BR * (1 - fB1)), 
      S1 = pS1 * log(BR * (1 + fS1)) + (1 - pS1) * log(BR * (1 - fS1)), 
      Edge1 = ifelse(fB1 > 0, B1, ifelse(fS1 > 0, S1, 0)), 
      
      fB2 = 2 * pB2 - 1, fS2 = 2 * pS2 - 1, ##
      B2 = pB2 * log(BR * (1 + fB2)) + (1 - pB2) * log(BR * (1 - fB2)), 
      S2 = pS2 * log(BR * (1 + fS2)) + (1 - pS2) * log(BR * (1 - fS2)), 
      Edge1 = ifelse(fB1 > 0, B1, ifelse(fS1 > 0, S1, 0)))
  
  if(Kelly == 'normal') {
    
    mbase %<>% mutate(
      PF = ifelse(Point.Forecast >= USDJPY.Low & Point.Forecast <= USDJPY.High, 
                  Point.Forecast, 0), #if forecasted place-bet price doesn't 
                                      #  existing within Hi-Lo price, then 
                                      #  the buying action is not stand. 
                                      #  Assume there has no web bandwith 
                                      #  delay. 
      FC = ifelse(forClose >= USDJPY.Low & forClose <= USDJPY.High, 
                  forClose, USDJPY.Close), #if forecasted settle price doesn't 
                                           #  existing within Hi-Lo price, 
                                           #  then the closing action at 
                                           #  closing price. Assume there has 
                                           #  no web bandwith delay. 
      Buy = ifelse(PF > 0 & FC > PF, 1, 0),  # buy action
      Sell = ifelse(PF > 0 & FC < PF, 1, 0), # sell action
      BuyS = Edge * Buy * (forClose - PF), 
      SellS = Edge * Sell * (PF - forClose), 
      Profit = BuyS + SellS, Bal = BR + Profit)

  } else if(Kelly == 'adjusted') {
    
    
    mbase %<>% mutate(
      PF = ifelse(Point.Forecast >= USDJPY.Low & Point.Forecast <= USDJPY.High, 
                  Point.Forecast, 0), #if forecasted place-bet price doesn't 
      #  existing within Hi-Lo price, then 
      #  the buying action is not stand. 
      #  Assume there has no web bandwith 
      #  delay. 
      FC = ifelse(forClose >= USDJPY.Low & forClose <= USDJPY.High, 
                  forClose, USDJPY.Close), #if forecasted settle price doesn't 
      #  existing within Hi-Lo price, 
      #  then the closing action at 
      #  closing price. Assume there has 
      #  no web bandwith delay. 
      Buy = ifelse(PF > 0 & FC > PF, 1, 0),  # buy action
      Sell = ifelse(PF > 0 & FC < PF, 1, 0), # sell action
      BuyS = Edge * Buy * (forClose - PF), 
      SellS = Edge * Sell * (PF - forClose), 
      Profit = BuyS + SellS, Bal = BR + Profit)
    
  } else {
    stop("Kindly choose method = 'normal' or method = 'adjusted'.")
  }
  
  return(mbase)
  }

