pnorm_bid_ask <- function(mbase, pnorm_type = 'Bid-Lo', financial_bet = FALSE) {
  
  mbase %<>% tbl_df %>% 
    mutate(Fct.High = round(Fct.High, 3), Fct.Low = round(Fct.Low, 3), 
           Fct.Close = round(Fct.Close, 3))
  
  if (financial_bet == TRUE) {
    
    if (pnorm_type == 'Bid-Lo'|pnorm_type == 'Ask-Hi') {
      
      ## Set Fct.Low as x value, mean(Fct.Close) as baseline for bid prob.
      ## BFAL : Bid first Ask later. (buy-sell)
      mbase %<>% mutate(
        p_bid1 = pnorm(Fct.Low, mean = mean(Fct.Close), sd = sd(Fct.Close)), 
        p_ask1 = 1 - p_bid1) #bid price for sell, ask price for buy orders.
      
      ## Set Fct.High as x value, mean(Fct.Low) as baseline for ask prob.
      ## AFBL : Ask first Bid later. (sell-buy)
      mbase %<>% mutate(
        p_ask2 = pnorm(Fct.High, mean = mean(Fct.Close), sd = sd(Fct.Close)), 
        p_bid2 = 1 - p_ask2) #bid price for sell, ask price for buy orders.
      
    } else if(pnorm_type == 'Ask-Lo'|pnorm_type == 'Bid-Hi') {
      
      ## Set Fct.High as x value, mean(Fct.Close) as baseline for bid prob.
      ## BFAL : Bid first Ask later. (buy-sell)
      mbase %<>% mutate(
        p_bid1 = pnorm(Fct.High, mean = mean(Fct.Close), sd = sd(Fct.Close)), 
        p_ask1 = 1 - p_bid1) #bid price for sell, ask price for buy orders.
      
      ## Set Fct.Low as x value, mean(Fct.Close) as baseline for ask prob.
      ## AFBL : Ask first Bid later. (sell-buy)
      mbase %<>% mutate(
        p_ask2 = pnorm(Fct.Low, mean = mean(Fct.Close), sd = sd(Fct.Close)), 
        p_bid2 = 1 - p_ask2) #bid price for sell, ask price for buy orders.
      
    } else {
      stop(
        paste("Kindly choose pnorm_type == 'Bid-Lo'|pnorm_type == 'Ask-Hi'", 
              "or pnorm_type == 'Ask-Lo'|pnorm_type == 'Bid-Hi'."))
    }
    
  } else {
  
    if (pnorm_type == 'Bid-Lo'|pnorm_type == 'Ask-Hi') {
      
      ## Set Fct.Low as x value, mean(Fct.High) as baseline for bid prob.
      ## BFAL : Bid first Ask later. (buy-sell)
      mbase %<>% mutate(
        p_bid1 = pnorm(Fct.Low, mean = mean(Fct.High), sd = sd(Fct.High)), 
        p_ask1 = 1 - p_bid1) #bid price for sell, ask price for buy orders.
      
      ## Set Fct.High as x value, mean(Fct.Low) as baseline for ask prob.
      ## AFBL : Ask first Bid later. (sell-buy)
      mbase %<>% mutate(
        p_ask2 = pnorm(Fct.High, mean = mean(Fct.Low), sd = sd(Fct.Low)), 
        p_bid2 = 1 - p_ask2) #bid price for sell, ask price for buy orders.
      
    } else if(pnorm_type == 'Ask-Lo'|pnorm_type == 'Bid-Hi') {
      
      ## Set Fct.High as x value, mean(Fct.Low) as baseline for bid prob.
      ## BFAL : Bid first Ask later. (buy-sell)
      mbase %<>% mutate(
        p_bid1 = pnorm(Fct.High, mean = mean(Fct.Low), sd = sd(Fct.Low)), 
        p_ask1 = 1 - p_bid1) #bid price for sell, ask price for buy orders.
      
      ## Set Fct.Low as x value, mean(Fct.High) as baseline for ask prob.
      ## AFBL : Ask first Bid later. (sell-buy)
      mbase %<>% mutate(
        p_ask2 = pnorm(Fct.Low, mean = mean(Fct.High), sd = sd(Fct.High)), 
        p_bid2 = 1 - p_ask2) #bid price for sell, ask price for buy orders.
      
    } else {
      stop(
        paste("Kindly choose pnorm_type == 'Bid-Lo'|pnorm_type == 'Ask-Hi'", 
              "or pnorm_type == 'Ask-Lo'|pnorm_type == 'Bid-Hi'."))
    }
  }
  
  return(mbase)
  }

