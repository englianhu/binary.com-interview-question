optF <- function(win, loss, p, obs, lev) {
  # optimal f test
  ## https://alphaism.wordpress.com/2012/04/13/testing-kelly-criterion-and-optimal-f-in-r/
  # 
  # similar as Kelly except using a different objective function
  f <- seq(0, lev, 1 / (obs - 1))
  
  ret <- rep(win, length(f))
  ret[which(rbinom(length(f), 1, 1 - p) == 1)] <- loss
  
  pnl <- f
  for (i in 1:length(f)) {
    pnl[i] <- prod(1 + ret / (-loss / f[i]))
  }
  
  results <- cbind(f, pnl)
  opt.f <- results[(which(results[, 2] == max(results[, 2]))), 1]
  
  output <- list(opt.f = opt.f, results = results)
  
  return(output)
}