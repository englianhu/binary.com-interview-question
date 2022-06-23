## source files' folder
## https://bitbucket.org/alexiosg/rmgarch/src/ce811acd0c7b/inst/rmgarch.tests/?at=beta

## source file
## https://bitbucket.org/alexiosg/rmgarch/src/ce811acd0c7bdda4a61dda2aba4d63c506987448/inst/rmgarch.tests/rmgarch.test4.R?at=beta&fileviewer=file-view-default

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



#################################################################################
# Scenario Tests (replication)
#################################################################################

rmgarch.test4a = function(cluster = NULL)
{
  tic = Sys.time()
  data(dji30retw)
  Dat = dji30retw[, 1:15, drop = FALSE]
  
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,0)), 
                     variance.model = list(garchOrder = c(1,1), model = "csGARCH"), 
                     distribution.model = "std")
  spec1 = cgarchspec(uspec = multispec( replicate(15, uspec) ), 
                     distribution.model = list(copula = "mvt", method = "Kendall", 
                                               time.varying = TRUE, transformation = "parametric"))
  
  scen.cgarch = fscenario(Data = Dat, sim = 5000, roll = 0, model = "cgarch", 
                          fit.control=list(eval.se = FALSE),
                          spec = spec1, solver = "solnp", cluster = cluster, rseed = 120)
  
  scen.cgarch.check = fscenario(Data = Dat, sim = 200, roll = 0, 
                                model = "cgarch", fit.control=list(eval.se = FALSE),
                                spec = spec1, solver = "solnp", cluster = cluster, rseed = 120)
  
  ## Check for replication of results with rseed
  
  options(width = 120)
  zz <- file("test4a-1.txt", open="wt")
  sink(zz)
  cat("\nreplication of results (cgarch)")
  all.equal(fitted(scen.cgarch)[1:200,,1], fitted(scen.cgarch.check)[1:200,,1])
  sink(type="message")
  sink()
  close(zz)
  
  # Notice that it is quite possible to get values < -1 in the weekly scenario. 
  # These should be removed/adjusted.
  idx = which(fitted(scen.cgarch)[,,1]<=(-1), arr.ind=TRUE)
  if(NROW(idx)>0) scen.cgarch@scenario$simMu[[1]][idx[,1],idx[,2]] = -0.9
  # check extractor method (fitted):
  options(width = 120)
  zz <- file("test4a-2.txt", open="wt")
  sink(zz)
  print(dim(fitted(scen.cgarch)))
  print(head(fitted(scen.cgarch)[,,1]))
  print(apply(fitted(scen.cgarch)[,,1], 2, "summary"))
  print(apply(fitted(scen.cgarch)[,,1], 2, "sd"))
  sink(type="message")
  sink()
  close(zz)
  
  
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,0)), 
                     variance.model = list(garchOrder = c(1,1), model = "csGARCH"), 
                     distribution.model = "norm")
  spec2 = dccspec(uspec = multispec( replicate(15, uspec) ), 
                  distribution = "mvt")
  
  scen.dcc = fscenario(Data = Dat, sim = 5000, roll = 0, model = "dcc", 
                       fit.control = list(eval.se = FALSE), solver.control=list(trace=1),
                       spec = spec2, solver = "solnp", cluster = cluster, rseed = 120)
  
  scen.dcc.check = fscenario(Data = Dat, sim = 200, roll = 0, model = "dcc", 
                             fit.control = list(eval.se = FALSE), spec = spec2, solver = "solnp", 
                             cluster = cluster, rseed = 120)
  
  
  idx = which(fitted(scen.dcc)[,,1]<=(-1), arr.ind=TRUE)
  if(NROW(idx)>0) scen.dcc@scenario$simMu[[1]][idx[,1],idx[,2]] = -0.9
  
  options(width = 120)
  zz <- file("test4a-3.txt", open="wt")
  sink(zz)
  cat("\nreplication of results (dcc)")
  all.equal(fitted(scen.dcc)[1:200,,1], fitted(scen.dcc.check)[1:200,,1])
  print(dim(fitted(scen.dcc)))
  print(head(fitted(scen.dcc)[,,1]))
  print(apply(fitted(scen.dcc)[,,1], 2, "summary"))
  print(apply(fitted(scen.dcc)[,,1], 2, "sd"))
  sink(type="message")
  sink()
  close(zz)
  
  
  spec3 = gogarchspec(mean.model = list(model = "AR", lag = 2), 
                      variance.model = list(model = "csGARCH", garchOrder = c(1,1)), 
                      distribution.model = c("mvnorm", "manig", "magh")[2], 
                      ica = "fastica")
  
  scen.gg = fscenario(Data = Dat, sim = 5000, roll = 0, model = "gogarch", 
                      spec = spec3, solver = "solnp", cluster = cluster, rseed = 120,
                      gfun = "tanh", maxiter1 = 100000)
  # exact replication requires to also pass the W matrix
  spec3x = gogarchspec(mean.model = list(model = "AR", lag = 2), 
                       variance.model = list(model = "csGARCH", garchOrder = c(1,1)), 
                       distribution.model = c("mvnorm", "manig", "magh")[2], 
                       ica = "fastica", ica.fix = list(A = scen.gg@scenario$gogarch.options$A, 
                                                       K = scen.gg@scenario$gogarch.options$K,
                                                       W = scen.gg@scenario$gogarch.options$W))
  
  scen.gg.check = fscenario(Data = Dat, sim = 200, roll = 0, model = "gogarch", 
                            spec = spec3x, solver = "solnp", cluster = cluster, rseed = 120)
  
  idx = which(fitted(scen.gg)[,,1]<=(-1), arr.ind=TRUE)
  if(NROW(idx)>0) scen.gg@scenario$simMu[[1]][idx[,1],idx[,2]] = -0.9
  
  options(width = 120)
  zz <- file("test4a-4.txt", open="wt")
  sink(zz)
  cat("\nreplication of results (GO-GARCH)")
  all.equal(fitted(scen.gg)[1:200,,1], fitted(scen.gg.check)[1:200,,1])
  print(dim(fitted(scen.gg)))
  print(head(fitted(scen.gg)[,,1]))
  print(apply(fitted(scen.gg)[,,1], 2, "summary"))
  print(apply(fitted(scen.gg)[,,1], 2, "sd"))
  sink(type="message")
  sink()
  close(zz)
  
  rowMeans = function(x) t(apply(x, 1, "mean"))
  postscript("test4a-1.eps", width = 10, height = 10)
  # Linear transformation (weights) require discrete returns
  plot(density(rowMeans(exp(fitted(scen.dcc)[,,1])-1)), main = "EW Forecast Density \n(02-Feb-2009)", 
       ylim=c(0,12), xlim=c(-0.3, 0.4), col="steelblue", cex.main=0.9)
  lines(density(rowMeans(exp(fitted(scen.cgarch)[,,1])-1)), col = "tomato1")
  lines(density(rowMeans(exp(fitted(scen.gg)[,,1])-1)), col = 3)
  legend("topleft", c("DCC-T(QML)", "DCC-Copula-T", "GO-GARCH (NIG)"), col = c("steelblue", "tomato1", "green"), bty="n",
         lty = c(1,1,1))
  grid()
  dev.off()
  
  
  postscript("test4a-2.eps", width = 8, height = 12)
  par(mfrow=c(3,1))
  boxplot(scen.cgarch@scenario$simMu[[1]], ylim = c(-0.3, 0.5), names = colnames(Dat), main = "DCC-Copula-T")
  
  boxplot(scen.dcc@scenario$simMu[[1]], ylim = c(-0.3, 0.5), names = colnames(Dat), main = "DCC-T(QML)")
  points(1:15, scen.dcc@scenario$forecastMu, col = 2, pch = 3)
  
  boxplot(scen.gg@scenario$simMu[[1]], ylim = c(-0.3, 0.5), names = colnames(Dat), main = "GO-GARCH (NIG)")
  points(1:15, scen.gg@scenario$forecastMu, col = 2, pch = 3)
  
  dev.off()
  
  zz <- file("test4a-5.txt", open="wt")
  sink(zz)
  cat("\nCGARCH Summary")
  apply(fitted(scen.cgarch)[,,1], 2, "summary")
  cat("\nDCC Summary")
  apply(fitted(scen.dcc)[,,1], 2, "summary")
  scen.dcc@scenario$forecastMu	
  cat("\nGO-GARCH Summary")
  apply(fitted(scen.gg)[,,1], 2, "summary")
  
  sink(type="message")
  sink()
  close(zz)
  
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}


#################################################################################
# Moments Tests
#################################################################################

rmgarch.test4b = function(cluster = NULL)
{
  tic = Sys.time()
  data(dji30retw)
  Dat = dji30retw[, 1:15, drop = FALSE]
  
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,0)), 
                     variance.model = list(garchOrder = c(1,1), model = "gjrGARCH"), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(15, uspec) ), 
                  distribution = "mvt")
  
  # equivalent to out.sample = 2, roll = 2 (i.e. 2 rolling forecast from data in 
  # the out.sample and 1 forecast > data
  M.dcc = fmoments(spec1, Data = dji30retw[,1:15], n.ahead = 1, roll = 2, 
                   solver = "solnp", fit.control = list(eval.se=FALSE), 
                   cluster = cluster)
  
  spec2 = gogarchspec(mean.model = list(model = "AR", lag = 2), 
                      variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)), 
                      distribution.model = c("mvnorm", "manig", "magh")[2], 
                      ica = "fastica")
  M.gg = fmoments(spec2, Data = dji30retw[,1:15], n.ahead = 1, roll = 2, 
                  solver = "solnp", cluster = cluster, gfun = "tanh", maxiter1=20000, 
                  A.init = diag(15))
  
  # create a set of random weights
  w = matrix(rexp(1000*15, 2), 1000, 15)
  w = t(apply(w, 1, FUN = function(x) x/sum(x)))
  m.fun = function(x, M){ sum(x * M) }
  # fitted returns the conditional mean (analytic)
  m1dcc = fitted(M.dcc)
  m1gg = fitted(M.gg)
  
  M1.dcc = apply(w, 1, FUN = function(x) m.fun(x, m1dcc[1,]))
  M2.dcc = apply(w, 1, FUN = function(x) m.fun(x, m1dcc[2,]))
  M3.dcc = apply(w, 1, FUN = function(x) m.fun(x, m1dcc[3,]))
  
  M1.gg = apply(w, 1, FUN = function(x) m.fun(x, m1gg[1,]))
  M2.gg = apply(w, 1, FUN = function(x) m.fun(x, m1gg[2,]))
  M3.gg = apply(w, 1, FUN = function(x) m.fun(x, m1gg[3,]))
  
  # While they both have the same conditional mean dynamics (AR[2]), the DCC model
  # AR model is estimated jointly with GARCH whilst the GO-GARCH is estimated
  # in the first stage under a constant variance. DCC is therefore more efficient
  # and the two estimates are expected to be different.
  postscript("test4b-1.eps")
  par(mfrow=c(2,2))
  boxplot(cbind(M1.dcc, M1.gg), main = as.character(M.dcc@moments$index[1]))
  boxplot(cbind(M2.dcc, M2.gg), main = as.character(M.dcc@moments$index[2]))
  boxplot(cbind(M3.dcc, M3.gg), main = as.character(M.dcc@moments$index[3]))
  dev.off()
  
  s.fun = function(x, C){ sqrt(x %*% C %*% x) }
  m2dcc = rcov(M.dcc)
  m2gg = rcov(M.gg)
  S1.dcc = apply(w, 1, FUN = function(x) s.fun(x, m2dcc[,,1]))
  S2.dcc = apply(w, 1, FUN = function(x) s.fun(x, m2dcc[,,2]))
  S3.dcc = apply(w, 1, FUN = function(x) s.fun(x, m2dcc[,,3]))
  
  S1.gg = apply(w, 1, FUN = function(x) s.fun(x, m2gg[,,1]))
  S2.gg = apply(w, 1, FUN = function(x) s.fun(x, m2gg[,,2]))
  S3.gg = apply(w, 1, FUN = function(x) s.fun(x, m2gg[,,3]))
  
  postscript("test4b-2.eps")
  par(mfrow=c(2,2))
  boxplot(cbind(S1.dcc, S1.gg), main = as.character(M.dcc@moments$index[1]))
  boxplot(cbind(S2.dcc, S2.gg), main = as.character(M.dcc@moments$index[2]))
  boxplot(cbind(S3.dcc, S3.gg), main = as.character(M.dcc@moments$index[3]))
  dev.off()
  
  sk.fun = function(x, S, s){ (x %*% S %*% kronecker(x,x))/s^3 }
  m3gg = rcoskew(M.gg)
  Sk1.gg = apply(cbind(w, S1.gg), 1, FUN = function(x) sk.fun(x[1:15], m3gg[,,1], x[16]))
  Sk2.gg = apply(cbind(w, S2.gg), 1, FUN = function(x) sk.fun(x[1:15], m3gg[,,2], x[16]))
  Sk3.gg = apply(cbind(w, S3.gg), 1, FUN = function(x) sk.fun(x[1:15], m3gg[,,3], x[16]))
  
  m4gg = rcokurt(M.gg)
  ku.fun = function(x, S, s){ (x %*% S %*% kronecker(x, kronecker(x,x)))/s^4 }
  K1.gg = apply(cbind(w, S1.gg), 1, FUN = function(x) ku.fun(x[1:15], m4gg[,,1], x[16]))
  K2.gg = apply(cbind(w, S2.gg), 1, FUN = function(x) ku.fun(x[1:15], m4gg[,,2], x[16]))
  K3.gg = apply(cbind(w, S3.gg), 1, FUN = function(x) ku.fun(x[1:15], m4gg[,,3], x[16]))
  
  postscript("test4b-3.eps", width=12, height=8)
  par(mfrow=c(1,2))
  boxplot(cbind(Sk1.gg, Sk2.gg, Sk3.gg), main = "GO-GARCH(NIG) \nWeighted Skewness", cex.main=0.9)
  boxplot(cbind(K1.gg, K2.gg, K3.gg)-3, main = "GO-GARCH(NIG) \nWeighted Ex-Kurtosis", cex.main=0.9, ylim = c(0, 3))
  abline(h = M.dcc@moments$kurtosis-3, col = "tomato1")
  legend("topright", ("DCC-T (QML) Ex-Kurtosis"), col = 2, lty=2, bty="n", cex = 0.8)
  dev.off()
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
  
}
