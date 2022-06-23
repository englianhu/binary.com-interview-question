## source files' folder
## https://bitbucket.org/alexiosg/rmgarch/src/8258b6a0ad85/R/?at=beta

## source file
## https://bitbucket.org/alexiosg/rmgarch/src/8258b6a0ad85eb1f3190117f32bb11a5e18834a4/R/rmgarch-tests.R?at=beta&fileviewer=file-view-default

#################################################################################
##
##   R package rmgarch by Alexios Ghalanos Copyright (C) 2008-2013.
##   This file is part of the R package rmgarch.
##
##   The R package rmgarch is free software: you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as published by
##   the Free Software Foundation, either version 3 of the License, or
##   (at your option) any later version.
##
##   The R package rmgarch is distributed in the hope that it will be useful,
##   but WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##
#################################################################################

# DCC Test of Engle and Sheppard (using a CCC-Normal Copula rather than a CCC-Normal)
################################################################################
DCCtest = function(Data, garchOrder = c(1,1), n.lags = 1, solver = "solnp", 
                   solver.control = list(), cluster = NULL, Z = NULL)
{
  if(is.null(Z)){
    Data = as.matrix(Data)
    n = dim(Data)[1]
    m = dim(Data)[2]
    uspec = ugarchspec(mean.model = list(armaOrder=c(0,0), include.mean = TRUE))
    mspec = multispec(replicate(m, uspec))
    cspec = cgarchspec(mspec, distribution.model = list(copula = "mvnorm", 
                                                        transformation = "parametric", method = "Kendall"))
    cfit = cgarchfit(cspec, Data, fit.control = list(eval.se = FALSE, trace = FALSE),
                     cluster = cluster, solver = solver, solver.control = solver.control)
    Z = cfit@mfit$stdresid
  } else{
    n = dim(Z)[1]
    m = dim(Z)[2]
  }
  OP = NULL
  for(i in 1:m){
    if(i<m){
      for(j in (i+1):m){
        OP = cbind(OP, Z[,i]*Z[,j])
      }}
  }
  j = dim(OP)[2]
  regressors = regressand = NULL
  for(i in 1:j){
    tmp = newlagmatrix(OP[,i,drop=FALSE],n.lags,1)
    regressors = rbind(regressors, tmp$x)
    regressand = c(regressand, tmp$y)
  }
  regressors = as.matrix(regressors)
  regressand = as.matrix(regressand)
  beta = t(qr.solve(regressand, regressors))
  XpX = t(regressors)%*%regressors
  e = regressand - regressors%*%beta
  sig = t(e)%*%e/(nrow(regressors))
  stat = t(beta)%*%XpX%*%beta/sqrt(sig)
  pval = 1-pchisq(stat, n.lags+1)
  H0 = "Constant Probability"
  ans = list()
  ans$H0 = H0
  ans$p.value = as.numeric(pval)
  ans$statistic = as.numeric(stat)
  return(ans)
}

################################################################################
MardiaTest = function(X, alpha){
  n = dim(X)[1]
  m = dim(X)[2]
  difT = scale(X, scale = FALSE)
  # Variance-covariance matrix
  S = cov(X)
  # Mahalanobis' distances matrix
  D = difT %*% solve(S) %*% t(difT)
  # Multivariate skewness coefficient
  b1p = sum(apply(D, 2, FUN = function(x) sum(x^3)))/(n^2)
  # Multivariate kurtosis coefficient
  b2p = sum(diag(D^2))/n
  # Small sample correction
  k = ((m+1)*(n+1)*(n+3))/(n*(((n+1)*(m+1))-6))
  # Degrees of freedom
  v = (m*(m+1)*(m+2))/6
  # Skewness test statistic corrected for small sample (approximates to a chi-square distribution)
  g1c = (n*b1p*k)/6
  # Skewness test statistic (approximates to a chi-square distribution)
  g1 = (n*b1p)/6
  # Significance value of skewness
  P1 = 1 - pchisq(g1,v)
  # Significance value of skewness corrected for small sample
  P1c = 1 - pchisq(g1c,v)
  # Kurtosis test statistic (approximates to a unit-normal distribution)
  g2 = (b2p-(m*(m+2)))/(sqrt((8*m*(m+2))/n))
  # Significance value of kurtosis
  P2 = 1-pnorm(abs(g2))
  stats = list()
  stats$Hs  = as.logical(P1 < alpha)
  stats$Ps  = P1
  stats$Ms  = g1
  stats$CVs = qchisq(1-alpha,v)
  stats$Hsc = as.logical(P1c < alpha)
  stats$Psc = P1c
  stats$Msc = g1c
  stats$Hk  = as.logical(P2 < alpha)
  stats$Pk  = P2
  stats$Mk  = g2
  stats$CVk = qnorm(1-alpha,0,1)
  # H1 = Alternative Hypothesis (Reject Multivariate Normality)
  H1 = c(stats$Hs, stats$Hsc, stats$Hk)
  stats$H1 = H1
  return(stats)
}
