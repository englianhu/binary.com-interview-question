optKelly <- function(win, loss, p, obs, lev) {
  # kelly formula test
  ## https://alphaism.wordpress.com/2012/04/13/testing-kelly-criterion-and-optimal-f-in-r/
  # 
  # win = payout for a win
  # loss = payout for a loss
  # p = probability of winning
  # obs = number of observations
  # lev = maximum leverage allowed
  
  # set up different bet sizes for test
  f <- seq(0, lev, 1 / (obs - 1))
  
  # generate trading results according to given win, loss and p
  ret <- rep(win, length(f))
  ret[which(rbinom(length(f), 1, 1 - p) == 1)] <- loss
  #calculate accumulative pnl for different bet sizes respectively
  pnl <- f
  for (i in 1:length(f)) {
    pnl[i] <- sum(log(1 + ret * f[i]))
  }
  # find the optimal point of f
  results <- cbind(f, pnl)
  opt.kelly <- results[which(results[, 2] == max(results[, 2])), 1]
  
  # wrap up
  output <- list(opt.kelly = opt.kelly, results = results)
  
  return(output)
}

