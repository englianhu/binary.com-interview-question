# kelly formula test
kelly.opt <- function(win, loss, p, obs, lev) {
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
  output <- list()
  output$opt.kelly <- opt.kelly
  output$results <- results
  output
}
# optimal f test
opt.f <- function(win, loss, p, obs, lev) {
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
  
  output <- list()
  output$opt.f <- opt.f
  output$results <- results
  output
}

# get statistics for kelly
compare <- data.frame(kelly=1:5000, opt.f=1:5000)
for (i in 1:5000) {
  compare$kelly[i] <- kelly.opt(2, -.5, .5, 500, 1)$opt.kelly
}

# get statistics for optimal f
for (i in 1:5000) {
  compare$opt.f[i] <- opt.f(2, -.5, .5, 500, 1)$opt.f
}

# generate graph
require(ggplot2)
m <- ggplot(compare, aes(colour=compare)) + xlim(c(0, 1)) + xlab('distribution')
m + stat_density(aes(x=kelly, colour='kelly'), adjust = 2, fill=NA) +
  stat_density(aes(x=opt.f, colour='opt.f'), adjust = 2, fill=NA)


