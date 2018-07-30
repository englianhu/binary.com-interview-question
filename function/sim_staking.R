sim_staking <- function(mbase, init_br = 10000, pnorm_type = 'Bid-Lo', 
                        bid_ask = 'b1-a1', financial_bet = FALSE, 
                        Kelly = 'normal', lv = 1, Trans = FALSE) {
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
  ## 4a) Kelly = 'none' : use forecast price, difference of pips.
  ## 
  ## 4b) Kelly = 'mixed' : fit Kelly model into FOREX.
  ## 
  ## 4c) Kelly = 'normal' : The mean value of variane of forecast highest 
  ##      price and mean value of forecast lowest price.
  ## 
  ## 4d) Kelly = 'adjusted1' : Similar with Kelly = 'normal' but the 
  ##      difference of both forecast price for open/close transactions.
  ## 
  ## 4e) Kelly = 'adjusted2' : Similar with Kelly = 'adjust1' but the 
  ##      difference of both forecast price in leverage
  ## 
  ## 4f) Kelly = 'adjusted3' : Model used in binary-Q1 which is wrongly 
  ##      using `Edge1 = ifelse(fB1 > 0, B1, ifelse(fS1 > 0, S1, 0))`. 
  ##      Therefore the fS will be secondary edge for fB but not 0.
  ## 
  ## 4g) Kelly = 'adjusted4' : Similar with Kelly = 'adjusted3' but 
  ##      seperate the arguments PF and PF2.
  ## 
  ## 4h) Kelly = 'adjusted5' : Similar with Kelly = 'normal' but use 
  ##      `Edge1 = ifelse(fB1 > 0, B1, ifelse(fS1 > 0, S1, 0))` but 
  ##      not `Edge1a = ifelse(fB1 > 0, B1, 0)` and 
  ##      `Edge1b = ifelse(fS1 > 0, S1, 0)`.
  ## 
  ## 4i) Kelly = 'adjusted6' : Similar with Kelly = 'adjusted5' but 
  ##      use `Edge1 = ifelse(fB1 > 0, B1, ifelse(fS1 > 0, S1, 0))` 
  ##      and `Edge2 = ifelse(fS1 > 0, S1, ifelse(fB1 > 0, B1, 0))`.
  ## 
  ## ===================================================================
  ## 
  ## 
  ## 
  ## 
  ## 
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
    mbase %<>% mutate(pS1 = p_bid1, pB1 = p_ask1, pS2 = p_bid2, pB2 = p_ask2)
    
  } else if (bid_ask == 'b1-a2') {
    mbase %<>% mutate(pS1 = p_bid2, pB1 = p_ask1, pS2 = p_bid2, pB2 = p_ask1)
    
  } else if (bid_ask == 'b2-a1') {
    mbase %<>% mutate(pS1 = p_bid1, pB1 = p_ask2, pS2 = p_bid1, pB2 = p_ask2)
    
  } else if (bid_ask == 'b2-a2') {
    mbase %<>% mutate(pS1 = p_bid2, pB1 = p_ask2, pS2 = p_bid1, pB2 = p_ask1)
    
  } else {
    stop("bid_ask = 'b1-a1', bid_ask = 'b1-a2', bid_ask = 'b2-a1', bid_ask = 'b2-a2'.")
  }
  
  ## detect sell or buy first. Not really correct due to the hishest and lowest 
  ##   ask/bid price doesn't shows whole timeline unless tick-data.
  if (Trans == TRUE & !'Trans' %in% names(mbase)) {
    
    mbase <- ldply(split(mbase, mbase$Date), function(x) {
      
      x %<>% mutate(Trans = ifelse(!is.na(Bid), 'sell', 'buy'), 
                    Trans = ifelse(Sell == 1, 'sell', 
                            ifelse(Buy == 1, 'buy', 0)))
      
      if (x[1,]$Trans == 'sell'|x[1,]$Trans == 'buy') { #if open transaction.
        x[nrow(x),]$Trans <- 'close'                    #  then close transaction.
      }
      x
    }) %>% tbl_df
    
  } else if (Trans == FALSE & 'Trans' %in% names(mbase)) {
    mbase$Trans <- mbase$Trans
    
  } else {
    mbase$Trans <- 1
  }
  
  
  ## Kelly model.
  mbase %<>% mutate(BR = init_br) %>% 
    mutate(
      fB1 = 2 * pB1 - 1, fS1 = 2 * pS1 - 1, ##ask for buy and bid for sell.
      B1 = pB1 * log(BR * (1 + fB1)) + (1 - pB1) * log(BR * (1 - fB1)), 
      S1 = pS1 * log(BR * (1 + fS1)) + (1 - pS1) * log(BR * (1 - fS1)), 
      # Edge1 = ifelse(fB1 > 0, B1, ifelse(fS1 > 0, S1, 0)), 
      Edge1a = ifelse(fB1 > 0, B1, 0), 
      Edge1b = ifelse(fS1 > 0, S1, 0), 
      
      fB2 = 2 * pB2 - 1, fS2 = 2 * pS2 - 1, ##ask for buy and bid for sell.
      B2 = pB2 * log(BR * (1 + fB2)) + (1 - pB2) * log(BR * (1 - fB2)), 
      S2 = pS2 * log(BR * (1 + fS2)) + (1 - pS2) * log(BR * (1 - fS2)), 
      # Edge2 = ifelse(fB2 > 0, B2, ifelse(fS2 > 0, S2, 0)), 
      Edge2a = ifelse(fB2 > 0, B2, 0), 
      Edge2b = ifelse(fS2 > 0, S2, 0))

  if (Kelly == 'none') {
    
    mbase %<>% mutate(
      Buy = ifelse(Fct.Low <= High & Fct.Low >= Low & Trans == 'buy', 1, 0), 
      Sell = ifelse(Fct.High >= Low & Fct.High <= High & Trans == 'sell', 1, 0), 
      Cls.Buy = ifelse(Fct.High > High, Close, Fct.High), 
      Cls.Sell = ifelse(Fct.Low < Low, Close, Fct.Low), 
      Profit = Buy * (Cls.Buy - Fct.Low) * lv + 
               Sell * (Fct.High - Cls.Sell) * lv, 
      Bal = BR + cumsum(Profit))
    
    ## update bankroll
    mbase$BR <- c(init_br, mbase$Bal[1:(nrow(mbase)-1)])
    
    ## Kelly model.
    mbase %<>% mutate(
        fB1 = 2 * pB1 - 1, fS1 = 2 * pS1 - 1, ##ask for buy and bid for sell.
        B1 = pB1 * log(BR * (1 + fB1)) + (1 - pB1) * log(BR * (1 - fB1)), 
        S1 = pS1 * log(BR * (1 + fS1)) + (1 - pS1) * log(BR * (1 - fS1)), 
        # Edge1 = ifelse(fB1 > 0, B1, ifelse(fS1 > 0, S1, 0)), 
        Edge1a = ifelse(fB1 > 0, B1, 0), 
        Edge1b = ifelse(fS1 > 0, S1, 0), 
        
        fB2 = 2 * pB2 - 1, fS2 = 2 * pS2 - 1, ##ask for buy and bid for sell.
        B2 = pB2 * log(BR * (1 + fB2)) + (1 - pB2) * log(BR * (1 - fB2)), 
        S2 = pS2 * log(BR * (1 + fS2)) + (1 - pS2) * log(BR * (1 - fS2)), 
        # Edge2 = ifelse(fB2 > 0, B2, ifelse(fS2 > 0, S2, 0)), 
        Edge2a = ifelse(fB2 > 0, B2, 0), 
        Edge2b = ifelse(fS2 > 0, S2, 0), 
        
        Buy = ifelse(Fct.Low <= High & Fct.Low >= Low & Trans == 'buy', 1, 0), 
        Sell = ifelse(Fct.High >= Low & Fct.High <= High & Trans == 'sell', 1, 0), 
        Cls.Buy = ifelse(Fct.High > High, Close, Fct.High), 
        Cls.Sell = ifelse(Fct.Low < Low, Close, Fct.Low), 
        Profit = Buy * (Cls.Buy - Fct.Low) * lv + 
          Sell * (Fct.High - Cls.Sell) * lv, 
        Bal = BR + cumsum(Profit))
    
  } else if (Kelly == 'mixed') {
    
    mbase %<>% mutate(
      Buy = ifelse(Fct.Low <= High & Fct.Low >= Low & Trans == 'buy', 1, 0), 
      Sell = ifelse(Fct.High >= Low & Fct.High <= High & Trans == 'sell', 1, 0), 
      Cls.Buy = ifelse(Fct.High > High, Close, Fct.High), 
      Cls.Sell = ifelse(Fct.Low < Low, Close, Fct.Low), 
      Profit = Buy * (Cls.Buy - Fct.Low) * lv * Edge1a + 
        Sell * (Fct.High - Cls.Sell) * lv * Edge1b, 
      Bal = BR + cumsum(Profit))
    
    ## update bankroll
    mbase$BR <- c(init_br, mbase$Bal[1:(nrow(mbase)-1)])
    
    ## update formula again to get the cumsum balance.
    mbase %<>% mutate(
      fB1 = 2 * pB1 - 1, fS1 = 2 * pS1 - 1, ##ask for buy and bid for sell.
      B1 = pB1 * log(BR * (1 + fB1)) + (1 - pB1) * log(BR * (1 - fB1)), 
      S1 = pS1 * log(BR * (1 + fS1)) + (1 - pS1) * log(BR * (1 - fS1)), 
      Edge1a = ifelse(fB1 > 0, B1, 0), 
      Edge1b = ifelse(fS1 > 0, S1, 0), 
      
      fB2 = 2 * pB2 - 1, fS2 = 2 * pS2 - 1, ##ask for buy and bid for sell.
      B2 = pB2 * log(BR * (1 + fB2)) + (1 - pB2) * log(BR * (1 - fB2)), 
      S2 = pS2 * log(BR * (1 + fS2)) + (1 - pS2) * log(BR * (1 - fS2)), 
      Edge2a = ifelse(fB2 > 0, B2, 0), 
      Edge2b = ifelse(fS2 > 0, S2, 0), 
      
      Buy = ifelse(Fct.Low <= High & Fct.Low >= Low & Trans == 'buy', 1, 0), 
      Sell = ifelse(Fct.High >= Low & Fct.High <= High & Trans == 'sell', 1, 0), 
      Cls.Buy = ifelse(Fct.High > High, Close, Fct.High), 
      Cls.Sell = ifelse(Fct.Low < Low, Close, Fct.Low), 
      Profit = Buy * (Cls.Buy - Fct.Low) * lv * Edge1a + 
        Sell * (Fct.High - Cls.Sell) * lv * Edge1b, 
      Bal = BR + cumsum(Profit))
    
  } else if (Kelly == 'normal') {
    
    mbase %<>% mutate(
      Buy = ifelse(Fct.Low <= High & Fct.Low >= Low & Trans == 'buy', 1, 0), 
      Sell = ifelse(Fct.High >= Low & Fct.High <= High & Trans == 'sell', 1, 0), 
      Profit = Buy * Edge1a + Sell * Edge1b, Bal = BR + cumsum(Profit))
    
    ## update bankroll
    mbase$BR <- c(init_br, mbase$Bal[1:(nrow(mbase)-1)])
    
    ## update formula again to get the cumsum balance.
    mbase %<>% mutate(
      fB1 = 2 * pB1 - 1, fS1 = 2 * pS1 - 1, ##ask for buy and bid for sell.
      B1 = pB1 * log(BR * (1 + fB1)) + (1 - pB1) * log(BR * (1 - fB1)), 
      S1 = pS1 * log(BR * (1 + fS1)) + (1 - pS1) * log(BR * (1 - fS1)), 
      Edge1a = ifelse(fB1 > 0, B1, 0), 
      Edge1b = ifelse(fS1 > 0, S1, 0), 
      
      fB2 = 2 * pB2 - 1, fS2 = 2 * pS2 - 1, ##ask for buy and bid for sell.
      B2 = pB2 * log(BR * (1 + fB2)) + (1 - pB2) * log(BR * (1 - fB2)), 
      S2 = pS2 * log(BR * (1 + fS2)) + (1 - pS2) * log(BR * (1 - fS2)), 
      Edge2a = ifelse(fB2 > 0, B2, 0), 
      Edge2b = ifelse(fS2 > 0, S2, 0), 
      
      Buy = ifelse(Fct.Low <= High & Fct.Low >= Low & Trans == 'buy', 1, 0), 
      Sell = ifelse(Fct.High >= Low & Fct.High <= High & Trans == 'sell', 1, 0), 
      Profit = Buy * Edge1a + Sell * Edge1b, Bal = BR + cumsum(Profit))
  
  } else if(Kelly == 'adjusted1') {
    
    mbase %<>% mutate(
      PF = ifelse(Fct.Low >= Low & Fct.Low <= High, Fct.Low, 0), 
                                      #if forecasted place-bet price 
                                      #  doesn't existing within Hi-Lo 
                                      #  price, then the buying action is 
                                      #  not stand. Assume there has no 
                                      #  web bandwith delay. 
      PF2 = ifelse(Fct.High >= Low & Fct.High <= High, Fct.High, 0), 
      FC = ifelse(Fct.Close >= Low & Fct.Close <= High, 
                  Fct.Close, Close),  #if forecasted settle price 
                                      #  doesn't existing within Hi-Lo 
                                      #  price, then the closing action 
                                      #  at closing price. Assume there 
                                      #  has no web bandwith delay. 
      Buy = ifelse(PF > 0 & FC > PF & Trans == 'buy', 1, 0),     # buy action
      Sell = ifelse(PF2 > 0 & FC < PF2 & Trans == 'sell', 1, 0), # sell action
      BuyS = Edge1a * Buy * (Fct.Close - PF),    #adjusted difference in pips
      SellS = Edge1b * Sell * (PF2 - Fct.Close), # similar with financial beting.
      Profit = BuyS + SellS, Bal = BR + cumsum(Profit))
    
    ## update bankroll
    mbase$BR <- c(init_br, mbase$Bal[1:(nrow(mbase)-1)])
    
    ## update formula again to get the cumsum balance.
    mbase %<>% mutate(
      fB1 = 2 * pB1 - 1, fS1 = 2 * pS1 - 1, ##ask for buy and bid for sell.
      B1 = pB1 * log(BR * (1 + fB1)) + (1 - pB1) * log(BR * (1 - fB1)), 
      S1 = pS1 * log(BR * (1 + fS1)) + (1 - pS1) * log(BR * (1 - fS1)), 
      Edge1a = ifelse(fB1 > 0, B1, 0), 
      Edge1b = ifelse(fS1 > 0, S1, 0), 
      
      fB2 = 2 * pB2 - 1, fS2 = 2 * pS2 - 1, ##ask for buy and bid for sell.
      B2 = pB2 * log(BR * (1 + fB2)) + (1 - pB2) * log(BR * (1 - fB2)), 
      S2 = pS2 * log(BR * (1 + fS2)) + (1 - pS2) * log(BR * (1 - fS2)), 
      Edge2a = ifelse(fB2 > 0, B2, 0), 
      Edge2b = ifelse(fS2 > 0, S2, 0), 
      
      PF = ifelse(Fct.Low >= Low & Fct.Low <= High, Fct.Low, 0), 
      PF2 = ifelse(Fct.High >= Low & Fct.High <= High, Fct.High, 0), 
      FC = ifelse(Fct.Close >= Low & Fct.Close <= High, 
                  Fct.Close, Close), 
      Buy = ifelse(PF > 0 & FC > PF & Trans == 'buy', 1, 0),     # buy action
      Sell = ifelse(PF2 > 0 & FC < PF2 & Trans == 'sell', 1, 0), # sell action
      BuyS = Edge1a * Buy * (Fct.Close - PF),    #adjusted difference in pips
      SellS = Edge1b * Sell * (PF2 - Fct.Close), # similar with financial beting.
      Profit = BuyS + SellS, Bal = BR + cumsum(Profit))
    
      
  } else if(Kelly == 'adjusted2') {
      
      mbase %<>% mutate(
        PF = ifelse(Fct.Low >= Low & Fct.Low <= High, Fct.Low, 0), 
        PF2 = ifelse(Fct.High >= Low & Fct.High <= High, Fct.High, 0), 
        FC = ifelse(Fct.Close >= Low & Fct.Close <= High, 
                    Fct.Close, Close), 
        Buy = ifelse(PF > 0 & FC > PF & Trans == 'buy', 1, 0),     # buy action
        Sell = ifelse(PF2 > 0 & FC < PF2 & Trans == 'sell', 1, 0), # sell action
        BuyS = Edge1a * Buy * (PF/(Fct.Close - PF)),           #adjusted
        SellS = Edge1b * Sell * (Fct.Close/(PF2 - Fct.Close)), # leverage.
        Profit = BuyS + SellS, Bal = BR + cumsum(Profit))
    
    ## update bankroll
    mbase$BR <- c(init_br, mbase$Bal[1:(nrow(mbase)-1)])
    
    ## update formula again to get the cumsum balance.
    mbase %<>% mutate(
      fB1 = 2 * pB1 - 1, fS1 = 2 * pS1 - 1, ##ask for buy and bid for sell.
      B1 = pB1 * log(BR * (1 + fB1)) + (1 - pB1) * log(BR * (1 - fB1)), 
      S1 = pS1 * log(BR * (1 + fS1)) + (1 - pS1) * log(BR * (1 - fS1)), 
      Edge1a = ifelse(fB1 > 0, B1, 0), 
      Edge1b = ifelse(fS1 > 0, S1, 0), 
      
      fB2 = 2 * pB2 - 1, fS2 = 2 * pS2 - 1, ##ask for buy and bid for sell.
      B2 = pB2 * log(BR * (1 + fB2)) + (1 - pB2) * log(BR * (1 - fB2)), 
      S2 = pS2 * log(BR * (1 + fS2)) + (1 - pS2) * log(BR * (1 - fS2)), 
      Edge2a = ifelse(fB2 > 0, B2, 0), 
      Edge2b = ifelse(fS2 > 0, S2, 0), 
      
      PF = ifelse(Fct.Low >= Low & Fct.Low <= High, Fct.Low, 0), 
      PF2 = ifelse(Fct.High >= Low & Fct.High <= High, Fct.High, 0), 
      FC = ifelse(Fct.Close >= Low & Fct.Close <= High, 
                  Fct.Close, Close), 
      Buy = ifelse(PF > 0 & FC > PF & Trans == 'buy', 1, 0),     # buy action
      Sell = ifelse(PF2 > 0 & FC < PF2 & Trans == 'sell', 1, 0), # sell action
      BuyS = Edge1a * Buy * (PF/(Fct.Close - PF)),           #adjusted
      SellS = Edge1b * Sell * (Fct.Close/(PF2 - Fct.Close)), # leverage.
      Profit = BuyS + SellS, Bal = BR + cumsum(Profit))
    
  } else if(Kelly == 'adjusted3') {
    
    mbase %<>% mutate(
      Edge1 = ifelse(fB1 > 0, B1, ifelse(fS1 > 0, S1, 0)), 
      PF = ifelse(Fct.Low >= Low & Fct.Low <= High, Fct.Low, 0), 
      #PF2 = ifelse(Fct.High >= Low & Fct.High <= High, Fct.High, 0), 
      FC = ifelse(Fct.Close >= Low & Fct.Close <= High, 
                  Fct.Close, Close), 
      Buy = ifelse(PF > 0 & FC > PF & Trans == 'buy', 1, 0),    # buy action
      Sell = ifelse(PF > 0 & FC < PF & Trans == 'sell', 1, 0),  # sell action
      BuyS = Edge1 * Buy * (Fct.Close - PF),   #adjusted
      SellS = Edge1 * Sell * (PF - Fct.Close), 
      Profit = BuyS + SellS, Bal = BR + cumsum(Profit))
    
    ## update bankroll
    mbase$BR <- c(init_br, mbase$Bal[1:(nrow(mbase)-1)])
    
    ## update formula again to get the cumsum balance.
    mbase %<>% mutate(
      fB1 = 2 * pB1 - 1, fS1 = 2 * pS1 - 1, ##ask for buy and bid for sell.
      B1 = pB1 * log(BR * (1 + fB1)) + (1 - pB1) * log(BR * (1 - fB1)), 
      S1 = pS1 * log(BR * (1 + fS1)) + (1 - pS1) * log(BR * (1 - fS1)), 
      Edge1a = ifelse(fB1 > 0, B1, 0), 
      Edge1b = ifelse(fS1 > 0, S1, 0), 
      
      fB2 = 2 * pB2 - 1, fS2 = 2 * pS2 - 1, ##ask for buy and bid for sell.
      B2 = pB2 * log(BR * (1 + fB2)) + (1 - pB2) * log(BR * (1 - fB2)), 
      S2 = pS2 * log(BR * (1 + fS2)) + (1 - pS2) * log(BR * (1 - fS2)), 
      Edge2a = ifelse(fB2 > 0, B2, 0), 
      Edge2b = ifelse(fS2 > 0, S2, 0), 
      
      Edge1 = ifelse(fB1 > 0, B1, ifelse(fS1 > 0, S1, 0)), 
      PF = ifelse(Fct.Low >= Low & Fct.Low <= High, Fct.Low, 0), 
      #PF2 = ifelse(Fct.High >= Low & Fct.High <= High, Fct.High, 0), 
      FC = ifelse(Fct.Close >= Low & Fct.Close <= High, 
                  Fct.Close, Close), 
      Buy = ifelse(PF > 0 & FC > PF & Trans == 'buy', 1, 0),    # buy action
      Sell = ifelse(PF > 0 & FC < PF & Trans == 'sell', 1, 0),  # sell action
      BuyS = Edge1 * Buy * (Fct.Close - PF),   #adjusted
      SellS = Edge1 * Sell * (PF - Fct.Close), 
      Profit = BuyS + SellS, Bal = BR + cumsum(Profit))
    
  } else if(Kelly == 'adjusted4') {
    
    mbase %<>% mutate(
      Edge1 = ifelse(fB1 > 0, B1, ifelse(fS1 > 0, S1, 0)), 
      PF = ifelse(Fct.Low >= Low & Fct.Low <= High, Fct.Low, 0), 
      PF2 = ifelse(Fct.High >= Low & Fct.High <= High, Fct.High, 0), 
      FC = ifelse(Fct.Close >= Low & Fct.Close <= High, 
                  Fct.Close, Close), 
      Buy = ifelse(PF > 0 & FC > PF & Trans == 'buy', 1, 0),     # buy action
      Sell = ifelse(PF2 > 0 & FC < PF2 & Trans == 'sell', 1, 0), # sell action
      BuyS = Edge1 * Buy * (Fct.Close - PF),    #adjusted
      SellS = Edge1 * Sell * (PF2 - Fct.Close), 
      Profit = BuyS + SellS, Bal = BR + cumsum(Profit))
    
    ## update bankroll
    mbase$BR <- c(init_br, mbase$Bal[1:(nrow(mbase)-1)])
    
    ## update formula again to get the cumsum balance.
    mbase %<>% mutate(
      fB1 = 2 * pB1 - 1, fS1 = 2 * pS1 - 1, ##ask for buy and bid for sell.
      B1 = pB1 * log(BR * (1 + fB1)) + (1 - pB1) * log(BR * (1 - fB1)), 
      S1 = pS1 * log(BR * (1 + fS1)) + (1 - pS1) * log(BR * (1 - fS1)), 
      Edge1a = ifelse(fB1 > 0, B1, 0), 
      Edge1b = ifelse(fS1 > 0, S1, 0), 
      
      fB2 = 2 * pB2 - 1, fS2 = 2 * pS2 - 1, ##ask for buy and bid for sell.
      B2 = pB2 * log(BR * (1 + fB2)) + (1 - pB2) * log(BR * (1 - fB2)), 
      S2 = pS2 * log(BR * (1 + fS2)) + (1 - pS2) * log(BR * (1 - fS2)), 
      Edge2a = ifelse(fB2 > 0, B2, 0), 
      Edge2b = ifelse(fS2 > 0, S2, 0), 
      
      Edge1 = ifelse(fB1 > 0, B1, ifelse(fS1 > 0, S1, 0)), 
      PF = ifelse(Fct.Low >= Low & Fct.Low <= High, Fct.Low, 0), 
      PF2 = ifelse(Fct.High >= Low & Fct.High <= High, Fct.High, 0), 
      FC = ifelse(Fct.Close >= Low & Fct.Close <= High, 
                  Fct.Close, Close), 
      Buy = ifelse(PF > 0 & FC > PF & Trans == 'buy', 1, 0),     # buy action
      Sell = ifelse(PF2 > 0 & FC < PF2 & Trans == 'sell', 1, 0), # sell action
      BuyS = Edge1 * Buy * (Fct.Close - PF),                     #adjusted
      SellS = Edge1 * Sell * (PF2 - Fct.Close), 
      Profit = BuyS + SellS, Bal = BR + cumsum(Profit))
    
  } else if(Kelly == 'adjusted5') {
      
    mbase %<>% mutate(
      Edge1 = ifelse(fB1 > 0, B1, ifelse(fS1 > 0, S1, 0)), 
      Buy = ifelse(Fct.Low <= High & Fct.Low >= Low & Trans == 'buy', 1, 0), 
      Sell = ifelse(Fct.High >= Low & Fct.High <= High & Trans == 'sell', 1, 0), 
      Profit = Buy * Edge1 + Sell * Edge1, Bal = BR + cumsum(Profit))
    
    ## update bankroll
    mbase$BR <- c(init_br, mbase$Bal[1:(nrow(mbase)-1)])
    
    ## update formula again to get the cumsum balance.
    mbase %<>% mutate(
      fB1 = 2 * pB1 - 1, fS1 = 2 * pS1 - 1, ##ask for buy and bid for sell.
      B1 = pB1 * log(BR * (1 + fB1)) + (1 - pB1) * log(BR * (1 - fB1)), 
      S1 = pS1 * log(BR * (1 + fS1)) + (1 - pS1) * log(BR * (1 - fS1)), 
      Edge1a = ifelse(fB1 > 0, B1, 0), 
      Edge1b = ifelse(fS1 > 0, S1, 0), 
      
      fB2 = 2 * pB2 - 1, fS2 = 2 * pS2 - 1, ##ask for buy and bid for sell.
      B2 = pB2 * log(BR * (1 + fB2)) + (1 - pB2) * log(BR * (1 - fB2)), 
      S2 = pS2 * log(BR * (1 + fS2)) + (1 - pS2) * log(BR * (1 - fS2)), 
      Edge2a = ifelse(fB2 > 0, B2, 0), 
      Edge2b = ifelse(fS2 > 0, S2, 0), 
      
      Edge1 = ifelse(fB1 > 0, B1, ifelse(fS1 > 0, S1, 0)), 
      Buy = ifelse(Fct.Low <= High & Fct.Low >= Low & Trans == 'buy', 1, 0), 
      Sell = ifelse(Fct.High >= Low & Fct.High <= High & Trans == 'sell', 1, 0), 
      Profit = Buy * Edge1 + Sell * Edge1, Bal = BR + cumsum(Profit))
    
  } else if(Kelly == 'adjusted6') {
    
    mbase %<>% mutate(
      Edge1 = ifelse(fB1 > 0, B1, ifelse(fS1 > 0, S1, 0)), 
      Edge2 = ifelse(fS1 > 0, S1, ifelse(fB1 > 0, B1, 0)), 
      Buy = ifelse(Fct.Low <= High & Fct.Low >= Low & Trans == 'buy', 1, 0), 
      Sell = ifelse(Fct.High >= Low & Fct.High <= High & Trans == 'sell', 1, 0), 
      Profit = Buy * Edge1 + Sell * Edge2, Bal = BR + cumsum(Profit))
    
    ## update bankroll
    mbase$BR <- c(init_br, mbase$Bal[1:(nrow(mbase)-1)])
    
    ## update formula again to get the cumsum balance.
    mbase %<>% mutate(
      fB1 = 2 * pB1 - 1, fS1 = 2 * pS1 - 1, ##ask for buy and bid for sell.
      B1 = pB1 * log(BR * (1 + fB1)) + (1 - pB1) * log(BR * (1 - fB1)), 
      S1 = pS1 * log(BR * (1 + fS1)) + (1 - pS1) * log(BR * (1 - fS1)), 
      Edge1a = ifelse(fB1 > 0, B1, 0), 
      Edge1b = ifelse(fS1 > 0, S1, 0), 
      
      fB2 = 2 * pB2 - 1, fS2 = 2 * pS2 - 1, ##ask for buy and bid for sell.
      B2 = pB2 * log(BR * (1 + fB2)) + (1 - pB2) * log(BR * (1 - fB2)), 
      S2 = pS2 * log(BR * (1 + fS2)) + (1 - pS2) * log(BR * (1 - fS2)), 
      Edge2a = ifelse(fB2 > 0, B2, 0), 
      Edge2b = ifelse(fS2 > 0, S2, 0), 
      
      Edge1 = ifelse(fB1 > 0, B1, ifelse(fS1 > 0, S1, 0)), 
      Edge2 = ifelse(fS1 > 0, S1, ifelse(fB1 > 0, B1, 0)), 
      Buy = ifelse(Fct.Low <= High & Fct.Low >= Low & Trans == 'buy', 1, 0), 
      Sell = ifelse(Fct.High >= Low & Fct.High <= High & Trans == 'sell', 1, 0), 
      Profit = Buy * Edge1 + Sell * Edge2, Bal = BR + cumsum(Profit))
    
  } else {
    stop(paste("Kindly choose Kelly = 'normal', Kelly = 'adjusted1',", 
               "Kelly = 'adjusted2', Kelly = 'adjusted3', ", 
               "Kelly = 'adjusted4', Kelly = 'adjusted5' or", 
               "Kelly = 'adjusted6'."))
  }
  
  return(mbase)
  }

