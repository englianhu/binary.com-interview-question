sim_staking <- function(mbase, init_br = 10000, pnorm_type = 'Bid-Lo', 
                        method = 'HiLo-sd') {
  ## 1a) pnorm_type = 'Bid-Lo' / pnorm_type = 'Ask-Hi' : Set the Low/High 
  ##      as the x value and High/Low as baseline variance.
  ## 
  ## 1b) pnorm_type = 'Ask-Lo' / pnorm_type = 'Bid-Hi' : Set the High/Low 
  ##      as the x value and Low/High as baseline variance.
  ## 
<<<<<<< HEAD
  ## 2a) method = 'HiLo-sd' : The mean value of variane of forecast highest 
  ##      price and mean value of forecast lowest price. The method origin 
  ##      from `simStakesGarch.R` but modified a bit which is combine both 
  ##      HiLo and LoHi but also added timeline placing order criteria.
  ## 
  ## 2b) method = 'HiLo-diff' : Both highest and lowest price directly used 
  ##      as odds price.
=======
  ## 2a) method = 'HiLo-sd' : The mean value of variane of forecast highest price 
  ##      and mean value of forecast lowest price. The method origin from  
  ##      `simStakesGarch.R` but modified a bit which is combine both HiLo and 
  ##      LoHi but also added timeline placing order criteria.
  ## 
  ## 2b) method = 'HiLo-diff' : Both highest and lowest price directly used as 
  ##      odds price.
>>>>>>> 1ae30d6a5a7d53e85a3ff48a520c05102a0014aa
  ## 
  
  source('function/pnorm_bid_ask.R')
  
  ## tidy dataset.
  mbase <- pnorm_bid_ask(mbase, pnorm_type = pnorm_type)
  
  if(method == 'HiLo-sd') {
    
<<<<<<< HEAD
    mbase %<>% mutate(BR = init_br) %>% 
=======
    mbase %<>% mutate(BR = .initialFundSize) %>% 
>>>>>>> 1ae30d6a5a7d53e85a3ff48a520c05102a0014aa
      #'@ mutate(Return.Back = ifelse(Prob > 0.5, Diff * Back * stakes, 0), 
      #'@        Return.Lay = ifelse(Prob < 0.5, -Diff * Lay * stakes, 0))
      mutate(fB = 2 * ProbB - 1, fS = 2 * ProbS - 1, 
             EUB = ProbB * log(BR * (1 + fB)) + (1 - ProbB) * log(BR * (1 - fB)), 
             EUS = ProbS * log(BR * (1 + fS)) + (1 - ProbS) * log(BR * (1 - fS)), 
             #'@ Edge = ifelse(f > 0, EUB, EUS), #For f > 0 need to buy and f <= 0 need to sell.
             #need to study on the risk management on "predicted profit" and "real profit".
             Edge = ifelse(fB > 0, EUB, ifelse(fS > 0, EUS, 0)), 
             PF = ifelse(Point.Forecast >= USDJPY.Low & 
                           Point.Forecast <= USDJPY.High, 
                         Point.Forecast, 0), #if forecasted place-bet price doesn't existing within Hi-Lo price, then the buying action is not stand. Assume there has no web bandwith delay.
             FC = ifelse(forClose >= USDJPY.Low & forClose <= USDJPY.High, 
                         forClose, USDJPY.Close), #if forecasted settle price doesn't existing within Hi-Lo price, then the closing action at closing price. Assume there has no web bandwith delay.
             #'@ Diff = round(forClose - USDJPY.Close, 2),
             ##forecasted closed price minus real close price.
             
             Buy = ifelse(PF > 0 & FC > PF, 1, 0), ##buy action
             Sell = ifelse(PF > 0 & FC < PF, 1, 0), ##sell action
             BuyS = Edge * Buy * (forClose - PF), 
             SellS = Edge * Sell * (PF - forClose), 
             Profit = BuyS + SellS, Bal = BR + Profit)
    
    
  } else if(method == 'HiLo-diff') {
    
    mbase %<>% mutate(
      p1 = 1/Fct.Low, 
      Edge.H = p1 * Fct.High - (1 - p1), 
      p2 = 1/Fct.High, 
      Edge.L = p2 * Fct.Low - (1 - p2), 
      f1 = Edge.H/Fct.Low, 
      f2 = Edge.L/Fct.High, 
      f1 = ifelse(f1 >= 0, f1, 0), 
      f2 = ifelse(f2 >= 0, f2, 0))# %>% 
    #select(-USDJPY.Open, -USDJPY.High, -USDJPY.Low, -USDJPY.Close)
    
    n = nrow(mbase)
    
    x1 = mbase$p_bid1
    x2 = mbase$p_ask1
    y1 = mbase$p_bid2
    y2 = mbase$p_ask2
    
    f_bid = mbase$f1
    f_ask = mbase$f2
    bankroll <- rep(0, n); bankroll[1] <- init_br
    br_bid <- bankroll; br_bid[1] <- init_br
    br_ask <- bankroll; br_ask[1] <- init_br
    
    for (i in 2:n) {
      if (x1[i] <= mbase$p1[i] | x2[i] <= mbase$p2[i]) {
        if (x1[i] <= mbase$p1[i]) {
          #bankroll[i] = bankroll[i-1] + bankroll[i-1] * f * odds
          br_bid[i] = br_bid[i-1] + br_bid[i-1] * f_bid * mbase$Fct.Low
        }
        if (x2[i] <= mbase$p2[i]) {
          br_ask[i] = br_ask[i-1] + br_ask[i-1] * f_ask * mbase$Fct.High
        }
      } else {
        #bankroll[i] = bankroll[i-1] - bankroll[i-1] * f
        br_bid[i] = br_bid[i-1] - br_bid[i-1] * f_bid
        br_ask[i] = br_ask[i-1] - br_ask[i-1] * f_ask
      }
    }
    
    #tmp <- list(bankroll, br_bid, br_ask)
    tmp <- list(br_bid, br_ask)
    return(tmp)
    
  } else {
    stop("Kindly choose method = 'HiLo-sd' or method = 'HiLo-diff'.")
  }
  
  }

