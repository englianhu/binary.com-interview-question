## source files' folder
## https://bitbucket.org/alexiosg/rmgarch/src/ce811acd0c7b/inst/rmgarch.tests/?at=beta

## source file
## https://bitbucket.org/alexiosg/rmgarch/src/ce811acd0c7bdda4a61dda2aba4d63c506987448/inst/rmgarch.tests/rmgarch.test1.R?at=beta&fileviewer=file-view-default

#################################################################################
##
##   R package rmgarch by Alexios Ghalanos Copyright (C) 2009, 2010, 2011
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
# GO-GARCH model
#################################################################################
# Fit Tests


rmgarch.test1a = function(cluster = NULL)
{
  tic = Sys.time()
  # Mean Specification Tests
  # constant-mvnorm
  data(dji30ret)
  ex = as.matrix(cbind(apply(dji30ret[,4:8], 1, "mean"), apply(dji30ret[,12:20], 1, "mean")))
  # Lag the external data
  ex = rugarch:::.lagx(ex, n.lag = 1, pad = 0)
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[1], lag = 3), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
  fit.1 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, 
                     cluster = cluster)
  
  # AR(2)
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[2], lag = 2), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
  fit.2 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, A.init = fit.1@mfit$A, 
                     cluster = cluster)
  
  # AR(2)+EXREG
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[2], lag = 2, external.regressors = ex), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
  # some convergence problems with normal solvers so use gosolnp
  fit.3 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], solver = "gosolnp", 
                     solver.control = list(trace=1), out.sample = 0, A.init = fit.2@mfit$A, 
                     cluster = cluster)
  
  # VAR(2)
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[3], lag = 2), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
  fit.4 = gogarchfit(spec, data = dji30ret[,1:3,drop=FALSE], out.sample = 0, 
                     cluster = cluster)
  
  # VAR(2)+EXREG
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[3], lag = 2, external.regressors = ex), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
  fit.5 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, A.init = fit.4@mfit$A, 
                     cluster = cluster)
  
  # VAR(2)+EXREG+Robust
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[3], lag = 2, external.regressors = ex, robust = TRUE), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
  fit.6 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, A.init = fit.5@mfit$A, 
                     cluster = cluster)
  
  
  # Collect and Compare:
  modellik = c(0, likelihood(fit.2)-likelihood(fit.1), likelihood(fit.3)-likelihood(fit.1), 
               likelihood(fit.4)-likelihood(fit.1), likelihood(fit.5)-likelihood(fit.1), likelihood(fit.6)-likelihood(fit.1))
  postscript("test1a1.eps", width = 8, height = 5)
  barplot(modellik, names.arg = c(paste("C[",round(likelihood(fit.1),0), "]", sep=""),"AR(2)", "ARX(2)", "VAR(2)", "VARX(2)", "robVARX(2)"),
          ylab = "Diff Log-Likelihood", xlab = "Model", col = "steelblue", main = "GOGARCH with different\nconditional mean models")
  dev.off()
  
  postscript("test1a2.eps", width = 10, height = 8)
  rc = rcor(fit.1)
  D = as.POSIXct(dimnames(rc)[[3]])
  plot(xts::xts(rc[1,2,], D), ylim = c(-0.4, 1), 
       main = "GOGARCH Correlation [AA-AXP] under\ndifferent mean models", 
       lty = 2, ylab = "Correlation", xlab = "Time", minor.ticks=FALSE, 
       auto.grid=FALSE)
  rc = rcor(fit.2)
  lines(xts::xts(rc[1,2,], D), col = 2, lty = 2)
  rc = rcor(fit.3)
  lines(xts::xts(rc[1,2,], D), col = 3)
  rc = rcor(fit.4)
  lines(xts::xts(rc[1,2,], D), col = 4, lty = 2)
  rc = rcor(fit.5)
  lines(xts::xts(rc[1,2,], D), col = 5)
  rc = rcor(fit.6)
  lines(xts::xts(rc[1,2,], D), col = 6)
  legend("bottomleft", legend = c("C", "AR(2)", "ARX(2)", "VAR(2)", "VARX(2)", "robVARX(2)"), 
         col = 1:6, lty = c(2,2,1,2,1,1), cex = 0.8, bty = "n")
  dev.off()
  
  # illustrate how the external regressors change the correlations
  postscript("test1a3.eps", width = 12, height = 20)
  T = fit.1@model$modeldata$T
  D = fit.1@model$modeldata$index[1:T]
  par(mfrow = c(3,1))
  plot(xts::xts(fit.1@model$modeldata$data[1:T,1], D), 
       main = "AA Returns vs Fit under \ndifferent mean models", 
       xlab = "Time", ylab = "R_t", minor.ticks = FALSE, auto.grid=FALSE)
  # fitted method returns xts for fit/filter objects
  lines(fitted(fit.6)[,1], lty = 2, col = 3, lwd = 0.5)
  lines(fitted(fit.2)[,1], lty = 2, col = 2)
  legend("topleft", legend = c("Actual", "robVARX(2)", "AR(2)"), 
         col = c(1,3,2), lty = c(1,2,2), bty ="n")
  
  plot(xts::xts(fit.1@model$modeldata$data[1:T,2], D), 
       main = "AXP Returns vs Fit under \ndifferent mean models", 
       xlab = "Time", ylab = "R_t", minor.ticks = FALSE, auto.grid=FALSE)
  lines(fitted(fit.6)[,2], lty = 2, col = 3, lwd = 0.5)
  lines(fitted(fit.2)[,2], lty = 2, col = 2)
  legend("topleft", legend = c("Actual", "robVARX(2)", "AR(2)"), 
         col = c(1,3,2), lty = c(1,2,2), bty ="n")
  
  plot(xts::xts(fit.1@model$modeldata$data[1:T,3], D), type = "l", 
       main = "BA Returns vs Fit under \ndifferent mean models", 
       xlab = "Time", ylab = "R_t", minor.ticks = FALSE, auto.grid=FALSE)
  lines(fitted(fit.6)[,3], lty = 2, col = 3, lwd = 0.5)
  lines(fitted(fit.2)[,3], lty = 2, col = 2)
  legend("topleft", legend = c("Actual", "robVARX(2)", "AR(2)"), 
         col = c(1,3,2), lty = c(1,2,2), bty ="n")
  dev.off()
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}

rmgarch.test1b = function(cluster = NULL)
{
  # Optimal VAR
  tic = Sys.time()
  require(vars)
  # Mean Specification Tests
  # constant-mvnorm
  data(dji30ret)
  ex = as.matrix(cbind(apply(dji30ret[,4:8], 1, "mean"), apply(dji30ret[,12:20], 1, "mean")))
  ex = rugarch:::.lagx(ex, n.lag = 1, pad = 0)
  
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[3], lag = 1, lag.max = 6), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
  fit = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, 
                   cluster = cluster)
  # compare to vars
  v2 = VAR(dji30ret[,1:3,drop=FALSE], lag.max=6, ic = "AIC")
  
  options(width = 120)
  zz <- file("test1b-1.txt", open="wt")
  sink(zz)
  print(fit@mfit$varcoef)
  cat("\n")
  print(fit@model$modelinc)
  cat("\nvars package output:\n")
  print(Bcoef(v2))
  sink(type="message")
  sink()
  close(zz)
  
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[3], lag = 1, lag.max = 6, external.regressors = ex), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
  fit = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, 
                   cluster = cluster)
  
  # compare to vars
  v2 = VAR(dji30ret[,1:3,drop=FALSE], lag.max=6, ic = "AIC", exogen = ex)
  
  options(width = 120)
  zz <- file("test1b-2.txt", open="wt")
  sink(zz)
  print(fit@mfit$varcoef)
  cat("\n")
  print(fit@model$modelinc)
  cat("\nvars package output:\n")
  print(Bcoef(v2))
  sink(type="message")
  sink()
  close(zz)
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}

rmgarch.test1c = function(cluster = NULL)
{
  # ICA Tests
  tic = Sys.time()
  data(dji30ret)
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[1], lag = 3), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica","radical")[1])
  fit.1 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, 
                     cluster = cluster)
  
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[1], lag = 3), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
  fit.2 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, gfun = "tanh", 
                     cluster = cluster)
  
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[1], lag = 3), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[2])
  fit.3 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, 
                     cluster = cluster)
  
  
  modellik = data.frame(fastica_pow3=likelihood(fit.1), fastica_tanh=likelihood(fit.2), radical=likelihood(fit.3))
  rownames(modellik) = "LLH"
  
  options(width = 120)
  zz <- file("test1c.txt", open="wt")
  sink(zz)
  print(modellik)
  sink(type="message")
  sink()
  close(zz)
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}

# filter tests
rmgarch.test1d = function(cluster = NULL)
{
  tic = Sys.time()
  data(dji30ret)
  ex = as.matrix(cbind(apply(dji30ret[,4:8], 1, "mean"), apply(dji30ret[,12:20], 1, "mean")))
  # Lag the external data
  ex = rugarch:::.lagx(ex, n.lag = 1, pad = 0)
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[1], lag = 3), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
  fit.1 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, 
                     cluster = cluster)
  filt.1 = gogarchfilter(fit.1, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, 
                         cluster = cluster)
  
  options(width = 120)
  zz <- file("test1d-1.txt", open="wt")
  sink(zz)
  print(head(fitted(fit.1))==head(fitted(filt.1)))
  cat("\n")
  print(rcov(fit.1)[,,10]==rcov(filt.1)[,,10])
  sink(type="message")
  sink()
  close(zz)
  
  # AR(2)
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[2], lag = 2), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "pearson", "jade", "radical")[1])
  fit.2 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, A.init = fit.1@mfit$A, 
                     cluster = cluster)
  filt.2 = gogarchfilter(fit.2, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, 
                         cluster = cluster)
  
  
  options(width = 120)
  zz <- file("test1d-2.txt", open="wt")
  sink(zz)
  print(head(fitted(fit.2))==head(fitted(filt.2)))
  cat("\n")
  print(rcov(fit.2)[,,10]==rcov(filt.2)[,,10])
  sink(type="message")
  sink()
  close(zz)
  
  # AR(2)+EXREG
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[2], lag = 2, external.regressors = ex), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
  # some convergence problems with normal solvers so use gosolnp
  fit.3 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], solver = "hybrid", 
                     solver.control = list(trace=0), out.sample = 0, A.init = fit.2@mfit$A, 
                     cluster = cluster)
  filt.3 = gogarchfilter(fit.3, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, 
                         cluster = cluster)
  
  options(width = 120)
  zz <- file("test1d-3.txt", open="wt")
  sink(zz)
  print(head(fitted(fit.3))==head(fitted(filt.3)))
  cat("\n")
  print(rcov(fit.3)[,,10]==rcov(filt.3)[,,10])
  sink(type="message")
  sink()
  close(zz)
  
  # VAR(2)
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[3], lag = 2), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
  fit.4 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0)
  filt.4 = gogarchfilter(fit.4, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, 
                         cluster = cluster)
  filt.44 = gogarchfilter(fit.4, data =  dji30ret[1:10,1:3,drop=FALSE], out.sample = 0, 
                          cluster = cluster)
  
  options(width = 120)
  zz <- file("test1d-4.txt", open="wt")
  sink(zz)
  print(head(fitted(fit.4))==head(fitted(filt.4)))
  cat("\n")
  print(head(fitted(filt.44))==head(fitted(filt.4)))
  cat("\n")
  print(rcov(fit.4)[,,10]==rcov(filt.4)[,,10])
  sink(type="message")
  sink()
  close(zz)
  
  # VAR(2)+EXREG
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[3], lag = 2, external.regressors = ex), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
  fit.5 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, A.init = fit.4@mfit$A, 
                     cluster = cluster)
  filt.5 = gogarchfilter(fit.5, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, 
                         cluster = cluster)
  
  options(width = 120)
  zz <- file("test1d-5.txt", open="wt")
  sink(zz)
  print(head(fitted(fit.5))==head(fitted(filt.5)))
  cat("\n")
  print(rcov(fit.5)[,,10]==rcov(filt.5)[,,10])
  sink(type="message")
  sink()
  close(zz)
  
  
  # VAR(2)+EXREG+Robust
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[3], lag = 2, external.regressors = ex, robust = TRUE), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
  fit.6 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, A.init = fit.5@mfit$A, 
                     cluster = cluster)
  filt.6 = gogarchfilter(fit.6, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, 
                         cluster = cluster)
  
  options(width = 120)
  zz <- file("test1d-6.txt", open="wt")
  sink(zz)
  print(head(fitted(fit.6))==head(fitted(filt.6)))
  cat("\n")
  print(rcov(fit.6)[,,10]==rcov(filt.6)[,,10])
  sink(type="message")
  sink()
  close(zz)
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
  
}

# NIG fit and test all methods
rmgarch.test1e = function(cluster = NULL)
{
  tic = Sys.time()
  data(dji30ret)
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[2], lag = 2), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[2], ica = c("fastica", "radical")[1])
  fit = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, cluster = cluster)
  
  options(width = 120)
  zz <- file("test1e-1.txt", open="wt")
  sink(zz)
  print(likelihood(fit))
  cat("\n")
  print(as.matrix(fit, which = "A"))
  cat("\n")
  print(as.matrix(fit, which = "K"))
  cat("\n")
  print(as.matrix(fit, which = "U"))
  cat("\n")
  print(head(fitted(fit)))
  cat("\n")
  print(head(residuals(fit)))
  cat("\n")
  print(rcov(fit)[,,1])
  cat("\n")
  print(rcor(fit)[,,1])
  cat("\n")
  print(rcoskew(fit, from=1, to=2))
  cat("\n")
  print(rcokurt(fit, from=1, to=2))
  cat("\n")
  sink(type="message")
  sink()
  close(zz)
  
  # Geometric Portfolio Moments
  gp = gportmoments(fit, weights = rep(1/3,3))
  cf = convolution(fit, weights = rep(1/3,3), fft.step = 0.001, fft.by = 0.0001, fft.support = c(-1, 1),
                   use.ff = TRUE, trace = 0, support.method = c("user", "adaptive")[2], cluster = cluster)
  # semi-analytic (FFT based)
  np = nportmoments(cf, trace=1)
  postscript("test1e1.eps", width = 8, height = 12)
  par(mfrow = c(2,2))
  plot(gp[1:1000,1], type = "p", main = "Portfolio Mean", minor.ticks=FALSE, auto.grid=FALSE)
  points(np[1:1000,1], col = 2, pch = 4)
  legend("topleft", legend = c("Geometric", "Semi-Analytic (FFT)"), col = 1:2, pch = c(1,4), bty="n")
  
  plot(gp[1:1000,2], type = "o", main = "Portfolio Sigma", minor.ticks=FALSE, auto.grid=FALSE)
  lines(np[1:1000,2], col = 2)
  legend("topleft", legend = c("Geometric", "Semi-Analytic (FFT)"), col = 1:2, fill=1:2, bty="n")
  # simply decrease the fft.step to obtain more accurate results (at the expense of memory)
  plot(gp[1:1000,3], type = "o", main = "Portfolio Skewness", minor.ticks=FALSE, auto.grid=FALSE)
  lines(np[1:1000,3], col = 2)
  legend("topleft", legend = c("Geometric", "Semi-Analytic (FFT)"), col = 1:2, fill=1:2, bty="n")
  
  plot(gp[1:1000,4], type = "o", main = "Portfolio Kurtosis", minor.ticks=FALSE, auto.grid=FALSE)
  lines(np[1:1000,4], col = 2)
  legend("topleft", legend = c("Geometric", "Semi-Analytic (FFT)"), col = 1:2, fill=1:2, bty="n")
  
  dev.off()
  
  # make a quantile graph of the weighted returns
  postscript("test1e2.eps", width = 8, height = 12)
  par(mfrow = c(2,2))
  qf = qfft(cf, index = 5521)
  plot(seq(0.01, 0.99, by = 0.005), qf(seq(0.01, 0.99, by = 0.005)), type = "l", main = "Portfolio Quantile\nObs=5521",
       xlab = "Value", ylab = "Probability")
  lines(seq(0.01, 0.99, by = 0.005), qnorm(seq(0.01, 0.99, by = 0.005), gp[5521,1], gp[5521,2]), col = 2)
  legend("topleft", legend = c("FFT Portfolio", "Gaussian Portfolio"), col =  1:2, fill = 1:2, bty = "n")
  
  df = dfft(cf, index = 4823)
  plot(seq(-0.3, 0.3, by = 0.005), df(seq(-0.3, 0.3, by = 0.005)), type = "l", main = "Portfolio Density",
       xlab = "Value", ylab = "pdf")
  lines(seq(-0.3, 0.3, by = 0.005), dnorm(seq(-0.3, 0.3, by = 0.005), gp[4823,1], gp[4823,2]), col = 2)
  legend("topleft", legend = c("FFT Portfolio", "Gaussian Portfolio"), col =  1:2, fill = 1:2, bty = "n")	
  # Inverse transform sampling
  rf = qfft(cf, index = 5519)
  rfx = runif(50000)
  sx = rf(rfx)
  plot(density(sx), type = "l",  main = "Sampled Portfolio Density",
       xlab = "Value", ylab = "pdf")
  lines(density(qnorm(rfx, gp[5519,1], gp[5519,2])), col  = 2)
  legend("topleft", legend = c("FFT Portfolio", "Gaussian Portfolio"), col =  1:2, fill = 1:2, bty = "n")
  dev.off()
  
  qseq = seq(0.01, 0.99, by = 0.005)
  # Make a quantile surface
  qsurface = matrix(NA, ncol = length(qseq), nrow = 5521)
  for(i in 1:5521){
    qf = qfft(cf, index = i)
    qsurface[i,] = qf(qseq)
  }
  png("test1e3.png", width = 800, height = 1200, res = 100)
  par(mar=c(1.8,1.8,1.1,1), pty = "m")
  x1 = shape::drapecol(qsurface, col = shape::femmecol(100), NAcol = "white")
  persp(  x = 1:5521,
          y = qseq,
          z = qsurface,  col = x1, theta = 45, phi = 25, expand = 0.5,
          ltheta = 120, shade = 0.75, ticktype = "simple", xlab = "Time",
          ylab = "Quantile", zlab = "Value",cex.axis = 0.8)
  dev.off()
  
  png("test1e4.png", width = 800, height = 1200, res = 100)
  ni = nisurface(fit, type = "cor", pair = c(1,3), factor = c(2,3), plot = TRUE)
  dev.off()
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}

# NIG filter and test all methods
rmgarch.test1f = function(cluster = NULL)
{
  
  tic = Sys.time()
  data(dji30ret)
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[2], lag = 2), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), 
                          submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[2], 
    ica = c("fastica", "radical")[1])
  fit = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, cluster = cluster)
  filt = gogarchfilter(fit, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0)
  options(width = 120)
  zz <- file("test1f.txt", open="wt")
  sink(zz)
  print(likelihood(filt))
  cat("\n")
  print(as.matrix(filt, which = "A"))
  cat("\n")
  print(as.matrix(filt, which = "K"))
  cat("\n")
  print(as.matrix(filt, which = "U"))
  cat("\n")
  print(head(fitted(filt)))
  cat("\n")
  print(head(residuals(filt)))
  cat("\n")
  print(rcov(filt)[,,1])
  cat("\n")
  print(rcor(filt)[,,1])
  cat("\n")
  print(rcoskew(filt, from=1, to=2))
  print(rcokurt(filt, from=1, to=2))
  cat("\n")
  sink(type="message")
  sink()
  close(zz)
  
  # Geometric Portfolio Moments
  gp = gportmoments(filt, weights = rep(1/3,3))
  # head(gp)
  cf = convolution(filt, weights = rep(1/3,3), fft.step = 0.001, 
                   fft.by = 0.0001, fft.support = c(-1, 1), use.ff = TRUE, 
                   support.method = c("user", "adaptive")[2], cluster = cluster)
  # semi-analytic (FFT based)
  np = nportmoments(cf)	
  postscript("test1f1.eps", width = 8, height = 12)
  par(mfrow = c(2,2))
  plot(gp[1:1000,1], type = "p", main = "Portfolio Mean", minor.ticks=FALSE, auto.grid=FALSE)
  points(np[1:1000,1], col = 2, pch = 4)
  legend("topleft", legend = c("Geometric", "Semi-Analytic (FFT)"), 
         col = 1:2, pch = c(1,4), bty="n")
  
  plot(gp[1:1000,2], type = "l", main = "Portfolio Sigma", minor.ticks=FALSE, auto.grid=FALSE)
  lines(np[1:1000,2], col = 2)
  legend("topleft", legend = c("Geometric", "Semi-Analytic (FFT)"), 
         col = 1:2, fill=1:2, bty="n")
  # simply decrease the fft.step to obtain more accurate results 
  # (at the expense of memory)
  plot(gp[1:1000,3], type = "l", main = "Portfolio Skewness", minor.ticks=FALSE, auto.grid=FALSE)
  lines(np[1:1000,3], col = 2)
  legend("topleft", legend = c("Geometric", "Semi-Analytic (FFT)"), 
         col = 1:2, fill=1:2, bty="n")
  
  plot(gp[1:1000,4], type = "l", main = "Portfolio Kurtosis", minor.ticks=FALSE, auto.grid=FALSE)
  lines(np[1:1000,4], col = 2)
  legend("topleft", legend = c("Geometric", "Semi-Analytic (FFT)"), 
         col = 1:2, fill=1:2, bty="n")
  dev.off()
  
  # make a quantile graph of the weighted returns
  postscript("test1f2.eps", width = 8, height = 12)
  par(mfrow = c(2,2))
  qf = qfft(cf, index = 5521)
  plot(seq(0.01, 0.99, by = 0.005), qf(seq(0.01, 0.99, by = 0.005)), 
       type = "l", main = "Portfolio Quantile\nObs=5521", xlab = "Value", 
       ylab = "Probability")
  lines(seq(0.01, 0.99, by = 0.005), qnorm(seq(0.01, 0.99, by = 0.005), 
                                           gp[5521,1], gp[5521,2]), col = 2)
  legend("topleft", legend = c("FFT Portfolio", "Gaussian Portfolio"), 
         col =  1:2, fill = 1:2, bty = "n")
  
  df = dfft(cf, index = 4823)
  plot(seq(-0.3, 0.3, by = 0.005), df(seq(-0.3, 0.3, by = 0.005)), type = "l", 
       main = "Portfolio Density", xlab = "Value", ylab = "pdf")
  lines(seq(-0.3, 0.3, by = 0.005), dnorm(seq(-0.3, 0.3, by = 0.005), 
                                          gp[4823,1], gp[4823,2]), col = 2)
  legend("topleft", legend = c("FFT Portfolio", "Gaussian Portfolio"), 
         col =  1:2, fill = 1:2, bty = "n")	
  # Inverse transform sampling
  rf = qfft(cf, index = 5521)
  rfx = runif(50000)
  sx = rf(rfx)
  plot(density(sx), type = "l",  main = "Sampled Portfolio Density",
       xlab = "Value", ylab = "pdf")
  lines(density(qnorm(rfx, gp[5521,1], gp[5521,2])), col  = 2)
  legend("topleft", legend = c("FFT Portfolio", "Gaussian Portfolio"), 
         col =  1:2, fill = 1:2, bty = "n")
  
  dev.off()
  
  qseq = seq(0.01, 0.99, by = 0.005)
  # Make a quantile surface
  qsurface = matrix(NA, ncol = length(qseq), nrow = 5521)
  for(i in 1:5521){
    qf = qfft(cf, index = i)
    qsurface[i,] = qf(qseq)
  }
  png("test1f3.png", width = 800, height = 1200, res = 100)
  par(mar=c(1.8,1.8,1.1,1), pty = "m")
  x1 = shape::drapecol(qsurface, col = shape::femmecol(100), NAcol = "white")
  persp(  x = 1:5521,
          y = qseq,
          z = qsurface,  col = x1, theta = 45, phi = 25, expand = 0.5,
          ltheta = 120, shade = 0.75, ticktype = "simple", xlab = "Time",
          ylab = "Quantile", zlab = "Value",cex.axis = 0.8)
  dev.off()
  
  png("test1f4.png", width = 800, height = 1200, res = 100)
  ni = nisurface(filt, type = "cor", pair = c(2,3), factor = c(1,3), plot = TRUE)
  dev.off()
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
  
}

# Out of sample methods and forecasting
rmgarch.test1g = function(cluster = NULL)
{
  tic = Sys.time()
  data(dji30ret)
  cnames = colnames(dji30ret)
  # ToDo: Investigate possible problem with variance targeting and eGARCH model
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[1], lag = 2, 
                      lag.max = 6), 
    variance.model = list(model = "sGARCH", submodel = "NULL", 
                          garchOrder = c(1,1), variance.targeting = TRUE), 
    distribution.model = c("mvnorm", "manig", "magh")[2], 
    ica = c("fastica", "radical")[1])
  fit = gogarchfit(spec, data = dji30ret[,1:5,drop=FALSE], out.sample = 500, 
                   cluster = cluster, gfun="tanh", maxiter1=25000)
  
  filt = gogarchfilter(fit, data = dji30ret[,1:5,drop=FALSE], out.sample = 500, 
                       cluster = cluster)
  
  filt2 = gogarchfilter(fit, data = dji30ret[,1:5,drop=FALSE], n.old = 5521-500, 
                        cluster = cluster)
  
  forc.1 = gogarchforecast(fit, n.ahead = 500, cluster = cluster)
  forc.2 = gogarchforecast(fit, n.ahead = 1, n.roll = 499, cluster = cluster)
  
  
  zz <- file("test1g-1.txt", open="wt")
  sink(zz)
  cat("\nFilter/Forecast Roll Check:\n")
  print(all.equal(matrix(last(fitted(forc.2), 1), nrow=1), matrix(as.numeric(tail(fitted(filt2), 1)), nrow=1)))
  print(round(rcor(forc.2)[[500]][,,1], 5) == round(last(rcor(filt2))[,,1],5))
  print(matrix(last(sigma(forc.2, factors=FALSE), 1), nrow=1) - as.matrix(tail(sigma(filt2, factors=FALSE), 1)))
  print(matrix(first(sigma(forc.2, factors=FALSE), 1), nrow=1) - as.matrix(sigma(filt2, factors=FALSE)[5521-500+1,]))
  sink(type="message")
  sink()
  close(zz)
  
  
  rc1 = rcor(forc.1)[[1]]
  rc2 = rcor(forc.2)
  
  postscript("test1g1.eps", width = 12, height = 12)
  par(mfrow = c(2,2))
  D = tail(fit@model$modeldata$index, 500)
  plot(xts::xts(sapply(rc2, function(x) x[1,2,1]),D), 
       main = paste("Correlation Forecast\n", cnames[1],"-", 
                    cnames[2], sep = ""), ylab = "Correlation", xlab = "Time",
       minor.ticks = FALSE, auto.grid = FALSE)
  lines(xts::xts(rc1[1,2,], D), col = 2 )
  legend("topright", legend = c("Rolling", "Unconditional"), 
         col  =1:2, fill = 1:2, bty = "n")
  
  plot(xts::xts(sapply(rc2, function(x) x[1,3,1]), D), type = "l", 
       main = paste("Correlation Forecast\n", cnames[1],"-", 
                    cnames[3], sep = ""), ylab = "Correlation", xlab = "Time",
       minor.ticks = FALSE, auto.grid = FALSE)
  lines(xts::xts(rc1[1,3,], D), col = 2 )
  legend("topright", legend = c("Rolling", "Unconditional"), 
         col  =1:2, fill = 1:2, bty = "n")
  
  plot(xts::xts(sapply(rc2, function(x) x[2,3,1]), D), type = "l", 
       main = paste("Correlation Forecast\n", cnames[2],"-", 
                    cnames[3], sep = ""), ylab = "Correlation", xlab = "Time",
       minor.ticks = FALSE, auto.grid = FALSE)
  lines(xts::xts(rc1[2,3,], D), col = 2 )
  legend("topright", legend = c("Rolling", "Unconditional"), 
         col  =1:2, fill = 1:2, bty = "n")
  dev.off()
  
  gp1 = gportmoments(forc.1, weights = rep(1/5,5))
  gp2 = gportmoments(forc.2, weights = rep(1/5,5))
  
  postscript("test1g2.eps", width = 12, height = 12)
  par(mfrow = c(2,1))
  D = tail(fit@model$modeldata$index, 500)
  
  plot(xts::xts(tail(apply(fit@model$modeldata$data,1,"mean"), 500), D), 
       main = paste("Portfolio Return Forecast"), ylab = "returns", 
       xlab = "Time", minor.ticks = FALSE, auto.grid = FALSE)
  lines(xts::xts(gp2[,"mu", ], D), col = 2 )
  lines(xts::xts(gp1[,"mu",1], D), col = 3 )
  legend("topright", legend = c("Actual", "Rolling", "Unconditional"), 
         col  = 1:3, fill = 1:3, bty = "n")
  
  plot(xts::xts(sqrt(tail(apply(fit@model$modeldata$data^2,1,"mean"), 500)), D), 
       main = paste("Portfolio Sigma Forecast"), ylab = "sigma", 
       xlab = "Time", minor.ticks = FALSE, auto.grid = FALSE)
  lines(xts::xts(gp2[,"sigma", ], D), col = 2 )
  lines(xts::xts(gp1[,"sigma",1], D), col = 3 )
  legend("topright", legend = c("Abs Returns", "Rolling", "Unconditional"), 
         col  = 1:3, fill = 1:3, bty = "n")
  dev.off()
  
  cf1 = convolution(forc.1,  weights = rep(1/5,5), fft.step = 0.001, 
                    fft.by = 0.0001, fft.support = c(-1, 1),
                    use.ff = TRUE, trace = 0, support.method = c("user", "adaptive")[1], 
                    cluster = cluster)
  
  cf2 = convolution(forc.2,  weights = rep(1/5,5), fft.step = 0.001, 
                    fft.by = 0.0001, fft.support = c(-1, 1),
                    use.ff = TRUE, trace = 0, support.method = c("user", "adaptive")[1], 
                    cluster = cluster)
  
  np1 = nportmoments(cf1, subdivisions = 400)
  np2 = nportmoments(cf2, subdivisions = 400)
  postscript("test1g3.eps", width = 12, height = 12)
  par(mfrow = c(2,2))
  plot(xts::xts(gp2[,"mu",], D), main = paste("Portfolio Return Forecast"), ylab = "returns", 
       xlab = "Time", minor.ticks = FALSE, auto.grid = FALSE)
  lines(xts::xts(np2[,"mu",], D), col = 2 )
  legend("topright", legend = c("Geometric", "Semi-Analytic(FFT)"), 
         col  = 1:2, fill = 1:2, bty = "n")
  
  plot(xts::xts(gp2[,"sigma",], D), main = paste("Portfolio Sigma Forecast"), ylab = "sigma", 
       xlab = "Time", minor.ticks = FALSE, auto.grid = FALSE)
  lines(xts::xts(np2[,"sigma",], D), col = 2 )
  legend("topright", legend = c("Geometric", "Semi-Analytic(FFT)"), 
         col  = 1:2, fill = 1:2, bty = "n")
  
  plot(xts::xts(gp2[,"skewness",], D), main = paste("Portfolio Skew Forecast"), ylab = "skewness", 
       xlab = "Time", minor.ticks = FALSE, auto.grid = FALSE)
  lines(xts::xts(np2[,"skewness",], D), col = 2 )
  
  plot(xts::xts(gp2[,"kurtosis",], D), main = paste("Portfolio Kurtosis Forecast"), ylab = "skewness", 
       xlab = "Time", minor.ticks = FALSE, auto.grid = FALSE)
  lines(xts::xts(np2[,"kurtosis",], D), col = 2 )
  legend("topright", legend = c("Geometric", "Semi-Analytic(FFT)"), 
         col  = 1:2, fill = 1:2, bty = "n")
  
  dev.off()
  
  # Rolling forecast VaR and Tests (don't expect much...500 days without re-estimation
  # is not likely to give good VaR estimates).
  VaR = matrix(NA, ncol = 2, nrow = 500)
  for(i in 0:499){
    qx = qfft(cf2, index = i)
    VaR[i+1,1:2] = qx(c(0.01, 0.05))
  }
  
  postscript("test1g4.eps", width = 12, height = 12)
  par(mfrow = c(2,1))
  VaRplot(0.01, actual = xts::xts(tail(apply(fit@model$modeldata$data,1,"mean"), 500),D), 
          VaR = xts::xts(VaR[,1], D))
  VaRplot(0.05, actual = xts::xts(tail(apply(fit@model$modeldata$data,1,"mean"), 500),D), 
          VaR = xts::xts(VaR[,2], D))
  dev.off()
  
  zz <- file("test1g-2.txt", open="wt")
  sink(zz)
  cat("\nGOGARCH VaR:\n")
  print(VaRTest(0.01, actual = tail(apply(fit@model$modeldata$data,1,"mean"), 500), 
                VaR = VaR[,1]))
  print(VaRTest(0.05, actual = tail(apply(fit@model$modeldata$data,1,"mean"), 500), 
                VaR = VaR[,2]))
  sink(type="message")
  sink()
  close(zz)
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}


# Rolling Estimation
rmgarch.test1h = function(cluster = NULL)
{
  tic = Sys.time()
  data(dji30retw)
  cnames = colnames(dji30retw)
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[3], 
                      lag = 2), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), 
                          variance.targeting = TRUE), 
    distribution.model = c("mvnorm", "manig", "magh")[2], 
    ica = c("fastica", "radical")[1])
  # to replicate the results set the rseed for initializing the fastica
  # algorithm
  roll = gogarchroll(spec, data = dji30retw[,1:10,drop=FALSE], n.ahead = 1, 
                     forecast.length = 500, refit.every = 20, 
                     refit.window = c("recursive", "moving")[1], 
                     cluster = cluster, gfun = "tanh", maxiter1=50000, rseed = 10)
  
  
  cf = convolution(roll,  weights = rep(1/10,10), fft.step = 0.001, 
                   fft.by = 0.0001, fft.support = c(-1, 1),
                   use.ff = FALSE, trace = 0, support.method = c("user", "adaptive")[1], 
                   cluster = cluster)
  
  VaR = matrix(NA, ncol = 2, nrow = 500)
  for(i in 0:499){
    qx = qfft(cf, index = i)
    VaR[i+1,1:2] = qx(c(0.01, 0.05))
  }
  
  postscript("test1h1.eps", width = 12, height = 12)
  par(mfrow =  c(2,1))
  D =  as.POSIXct(tail(rownames(dji30retw), 500))
  Y = tail(apply(dji30retw[,1:10],1,"mean"), 500)
  VaRplot(0.01, actual = xts::xts(Y, D), VaR = xts::xts(VaR[,1], D))
  VaRplot(0.05, actual = xts::xts(Y, D), VaR = xts::xts(VaR[,2], D))
  dev.off()
  
  zz <- file("test1h-1.txt", open="wt")
  sink(zz)
  cat("\nGOGARCH Rolling Estimation VaR:\n")
  print(VaRTest(0.01, actual = tail(apply(dji30retw[,1:10],1,"mean"), 500), 
                VaR = VaR[,1]))
  print(VaRTest(0.05, actual = tail(apply(dji30retw[,1:10],1,"mean"), 500), 
                VaR = VaR[,2]))
  sink(type="message")
  sink()
  close(zz)
  
  zz <- file("test1h-2.txt", open="wt")
  sink(zz)
  cat("\nGOGARCH Rolling Methods:\n")
  # Check methods on roll:
  print(head(sigma(roll)))
  print(rcor(roll)[,,1,drop=FALSE])
  print(rcov(roll)[,,1,drop=FALSE])
  print(rcoskew(roll)[,,1])
  print(rcokurt(roll)[,,1])
  sink(type="message")
  sink()
  close(zz)
  
  # other methods
  gp = gportmoments(roll, rep(1/10,10))
  np = nportmoments(cf)
  D = as.POSIXct(rownames(tail(dji30retw, 500)))
  postscript("test1h2.eps", width = 12, height = 12)
  par(mfrow = c(2,2))
  plot(xts::xts(gp[,"mu"], D), main = paste("Portfolio Return Forecast"), ylab = "returns", 
       xlab = "Time", minor.ticks = FALSE, auto.grid = FALSE)
  lines(xts::xts(np[,"mu"], D), col = 2 )
  legend("topright", legend = c("Geometric", "Semi-Analytic(FFT)"), 
         col  = 1:2, fill = 1:2, bty = "n")
  
  plot(xts::xts(gp[,"sigma"], D), main = paste("Portfolio Sigma Forecast"), ylab = "sigma", 
       xlab = "Time", minor.ticks = FALSE, auto.grid = FALSE)
  lines(xts::xts(np[,"sigma"], D), col = 2 )
  legend("topright", legend = c("Geometric", "Semi-Analytic(FFT)"), 
         col  = 1:2, fill = 1:2, bty = "n")
  
  plot(xts::xts(gp[,"skewness"], D), main = paste("Portfolio Skew Forecast"), ylab = "skewness", 
       xlab = "Time", minor.ticks = FALSE, auto.grid = FALSE)
  lines(xts::xts(np[,"skewness"], D), col = 2 )
  
  plot(xts::xts(gp[,"kurtosis"], D), main = paste("Portfolio Kurtosis Forecast"), ylab = "skewness", 
       xlab = "Time", minor.ticks = FALSE, auto.grid = FALSE)
  lines(xts::xts(np[,"kurtosis"], D), col = 2 )
  legend("topright", legend = c("Geometric", "Semi-Analytic(FFT)"), 
         col  = 1:2, fill = 1:2, bty = "n")
  dev.off()
  
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}

# moment tests
rmgarch.test1i = function(cluster = NULL)
{
  tic = Sys.time()
  data(dji30ret)
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[1], lag = 3), 
    variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[2], ica = c("fastica", "radical")[1])
  fit = gogarchfit(spec, data =  dji30ret[,1:5,drop=FALSE], out.sample = 0, 
                   cluster = cluster, gfun = "tanh", maxiter1 = 20000, rseed = 12)
  forc = gogarchforecast(fit, n.ahead = 1)
  w = matrix(rep(1/5,5), ncol = 5)
  # Geometric (Analytical Moments by Linear Affine Transformation)
  gm = gportmoments(forc, weights = w)
  # equivalent to:
  sk = w%*%rcoskew(forc, standardize = FALSE)[,,1]%*%kronecker(t(w),t(w))/gportmoments(forc, weights = w)[1,2,1]^3
  ku = w%*%rcokurt(forc, standardize = FALSE)[,,1]%*%kronecker(t(w), kronecker(t(w),t(w)))/gportmoments(forc, weights = w)[1,2,1]^4
  # Numeric (Semi-Analytic by convolution of densities)
  cf = convolution(forc, weights = w)
  nm = nportmoments(cf, weights = w)
  # equivalent to:
  df = dfft(cf, index=0)
  m1 = gm[1,1,1]
  f = function(x) (x-m1)^4*df(x)
  nme = integrate(f, -Inf, Inf, rel.tol=1e-9, stop.on.error=FALSE)$value/gm[1,2,1]^4
  
  zz <- file("test1i.txt", open="wt")
  sink(zz)
  cat("\nGOGARCH Forecast Weighted Kurtosis Differences in Methods:\n")
  print(all.equal(gm[1,4,1], nme))
  print(all.equal(as.numeric(nm[1,4,1]), nme))
  print(all.equal(as.numeric(nm[1,4,1]), gm[1,4,1]))
  print(all.equal(as.numeric(sk), gm[1,3,1]))
  print(all.equal(as.numeric(ku), gm[1,4,1]))
  
  sink(type="message")
  sink()
  close(zz)
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}

# some simulated forecast tests based on new functionality to gogarchsim version 0.96
rmgarch.test1j = function(cluster = NULL)
{
  
  tic = Sys.time()	
  data(dji30ret)
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[2], lag = 2),
    variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
  
  fit = gogarchfit(spec = spec, data = dji30ret[1:1010,1:3], out.sample = 10, solver = "solnp",
                   gfun = "tanh", maxiter1 = 20000, rseed = 25)
  
  forc = gogarchforecast(fit, n.ahead = 1, n.roll = 10)
  fc = matrix(fitted(forc), ncol = 3, nrow=11, byrow=TRUE)
  # max lag for mean.model = 2
  p = fit@model$modelinc[2]
  simM = matrix(NA, ncol = 3, nrow = 11)
  filt = gogarchfilter(fit, data = dji30ret[1:1010,1:3], out.sample = 0, n.old = 1000)
  # T + 1 -> T + 11
  # since version 1.00 all data returned is of the same size as the dataset (i.e. not
  # truncated for any lags). This follows a subtle change in the initialization of the
  # arfima recursion which is now set to the constan/mean for the first p AR lags and
  # so the residuals are not zero, which means that the standardized residuals can be
  # calculated.
  T = fit@model$modeldata$T
  for(i in 1:11){
    preres = fit@mfit$Y[(T-p+i):(T-1+i),]
    presigma = filt@mfilter$factor.sigmas[(T-p+i):(T-1+i),]
    prereturns = fit@model$modeldata$data[(T-p+i):(T-1+i),,drop=FALSE]
    sim = gogarchsim(fit, n.sim = 1, m.sim = 500, startMethod = "sample",
                     preres = preres, presigma = presigma, prereturns = prereturns)
    simx = t(sapply(sim@msim$seriesSim, FUN = function(x) x))
    simres = t(sapply(sim@msim$residSim, FUN = function(x) x))
    # after removing the uncertainty we should get the forecast (fc)
    simM[i, ] = simx[1,] - simres[1,]
  }
  
  zz <- file("test1j1.txt", open="wt")
  sink(zz)
  cat("\nAR-GOGARCH Rolling Forecast vs Rolling Simulation Check:\n")
  print(all.equal(simM, fc))
  sink(type="message")
  sink()
  close(zz)
  
  # use VAR
  
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[3], lag = 2),
    variance.model = list(model = "sGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
  
  fit = gogarchfit(spec = spec, data = dji30ret[1:1010,1:3], out.sample = 10, solver = "solnp",
                   gfun = "tanh", maxiter1 = 20000, rseed = 25)
  
  forc = gogarchforecast(fit, n.ahead = 1, n.roll = 10)
  fc = matrix(fitted(forc), ncol = 3, nrow=11, byrow=TRUE)
  # max lag for mean.model = 2
  p = fit@model$modelinc[3]
  simM = matrix(NA, ncol = 3, nrow = 11)
  filt = gogarchfilter(fit, data = dji30ret[1:1010,1:3], out.sample = 0, n.old = 1000)
  # T + 1 -> T + 11
  T = fit@model$modeldata$T
  for(i in 1:11){
    preres = fit@mfit$Y[(T-p+i):(T-1+i),]
    presigma = filt@mfilter$factor.sigmas[(T-p+i):(T-1+i),]
    prereturns = fit@model$modeldata$data[(T-p+i):(T-1+i),,drop=FALSE]
    sim = gogarchsim(fit, n.sim = 1, m.sim = 100, startMethod = "sample",
                     preres = preres, presigma = presigma, prereturns = prereturns)
    simx = t(sapply(sim@msim$seriesSim, FUN = function(x) x))
    simres = t(sapply(sim@msim$residSim, FUN = function(x) x))
    # after removing the uncertainty we should get the forecast (fc)
    simM[i, ] = simx[1,] - simres[1,]
  }
  
  zz <- file("test1j2.txt", open="wt")
  sink(zz)
  cat("\nVAR-GOGARCH Rolling Forecast vs Rolling Simulation Check:\n")
  print(all.equal(simM, fc))
  sink(type="message")
  sink()
  close(zz)
  
  
  ##############################
  # One more test
  cnames = colnames(dji30ret[,1:3])
  spec = gogarchspec(
    mean.model = list(model = c("constant", "AR", "VAR")[2], lag = 2),
    variance.model = list(model = "sGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
    distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
  
  fit = gogarchfit(spec = spec, data = dji30ret[1:1010,1:3], out.sample = 10, solver = "solnp")
  
  forc = gogarchforecast(fit, n.ahead = 1, n.roll = 10)
  fc = matrix(fitted(forc), ncol = 3, nrow=11, byrow=TRUE)
  fs = matrix(sigma(forc, factors=FALSE), ncol = 3, nrow=11, byrow=TRUE)
  # max lag for mean.model = 2
  p = fit@model$modelinc[2]
  simX = vector(mode = "list", length = 11)
  filt = gogarchfilter(fit, data = dji30ret[1:1010,1:3], out.sample = 0, n.old = 1000)
  # T + 1 -> T + 11
  T = fit@model$modeldata$T
  for(i in 1:11){
    preres = fit@mfit$Y[(T-p+i):(T-1+i),]
    presigma = filt@mfilter$factor.sigmas[(T-p+i):(T-1+i),]
    prereturns = fit@model$modeldata$data[(T-p+i):(T-1+i),,drop=FALSE]
    sim = gogarchsim(fit, n.sim = 1, m.sim = 500, startMethod = "sample",
                     preres = preres, presigma = presigma, prereturns = prereturns)
    # after removing the uncertainty we should get the forecast (fc)
    simX[[i]] = t(sapply(sim@msim$seriesSim, FUN = function(x) x))
  }
  simM = t(sapply(simX, FUN = function(x) colMeans(x)))
  simS = t(sapply(simX, FUN = function(x) apply(x, 2, "sd")))
  
  postscript("test1j.eps", width = 12, height = 12)
  par(mfrow=c(2,2))
  boxplot(simX[[1]], main = "T+1", names = cnames)
  points(fc[1,], col = 3, lwd = 4, pch  =12)
  points(simM[1,], col = 2, lwd = 2, pch = 10)
  points(fc[1,]+3*fs[1,], col = 4, lwd = 2, pch = 14)
  points(fc[1,]-3*fs[1,], col = 4, lwd = 2, pch = 14)
  legend("bottomleft", c("Mean[sim]", "Mean[forc]", "3sd"), col = c(2,3,4), pch=c(10,12,14),
         bty="n", lwd=c(2,4,2), cex=0.7)
  
  boxplot(simX[[4]], main = "T+4", names = cnames)
  points(fc[4,], col = 3, lwd = 4, pch  =12)
  points(simM[4,], col = 2, lwd = 2, pch = 10)
  points(fc[4,]+3*fs[4,], col = 4, lwd = 2, pch = 14)
  points(fc[4,]-3*fs[4,], col = 4, lwd = 2, pch = 14)
  legend("bottomleft", c("Mean[sim]", "Mean[forc]", "3sd"), col = c(2,3,4), pch=c(10,12,14),
         bty="n", lwd=c(2,4,2), cex=0.7)
  
  boxplot(simX[[6]], main = "T+6", names = cnames)
  points(fc[6,], col = 3, lwd = 4, pch  =12)
  points(simM[6,], col = 2, lwd = 2, pch = 10)
  points(fc[6,]+3*fs[6,], col = 4, lwd = 2, pch = 14)
  points(fc[6,]-3*fs[6,], col = 4, lwd = 2, pch = 14)
  legend("bottomleft", c("Mean[sim]", "Mean[forc]", "3sd"), col = c(2,3,4), pch=c(10,12,14),
         bty="n", lwd=c(2,4,2), cex=0.7)
  
  boxplot(simX[[11]], main = "T+11", names = cnames)
  points(fc[11,], col = 3, lwd = 4, pch  =12)
  points(simM[11,], col = 2, lwd = 2, pch = 10)
  points(fc[11,]+3*fs[11,], col = 4, lwd = 2, pch = 14)
  points(fc[11,]-3*fs[11,], col = 4, lwd = 2, pch = 14)
  legend("bottomleft", c("Mean[sim]", "Mean[forc]", "3sd"), col = c(2,3,4), pch=c(10,12,14),
         bty="n", lwd=c(2,4,2), cex=0.7)
  
  dev.off()
  
  
  # ToDo: Test nportmoments and gportmoments on goGARCHsim
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}
