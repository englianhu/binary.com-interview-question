## source files' folder
## https://bitbucket.org/alexiosg/rmgarch/src/ce811acd0c7b/inst/rmgarch.tests/?at=beta

## source file
## https://bitbucket.org/alexiosg/rmgarch/src/ce811acd0c7bdda4a61dda2aba4d63c506987448/inst/rmgarch.tests/rmgarch.test2.R?at=beta&fileviewer=file-view-default

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
# DCC model
#################################################################################
# Fit Tests

rmgarch.test2a = function(cluster = NULL)
{
  # DCC under different specifications
  tic = Sys.time()
  data(dji30retw)
  Dat = dji30retw[, 1:3, drop = FALSE]
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), variance.model = list(garchOrder = c(3,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(3, uspec) ), dccOrder = c(1,1),  distribution = "mvnorm")
  fit1 = dccfit(spec1, data = Dat, fit.control = list(eval.se=FALSE))
  
  specx1 = ugarchspec(mean.model = list(armaOrder = c(2,1)), variance.model = list(garchOrder = c(3,1), model = "sGARCH",
                                                                                   variance.targeting=TRUE), 
                      distribution.model = "norm")
  specx2 = ugarchspec(mean.model = list(armaOrder = c(0,1)), variance.model = list(garchOrder = c(1,1), model = "eGARCH",
                                                                                   variance.targeting=F), 
                      distribution.model = "norm")
  specx3 = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1), model = "apARCH"), 
                      distribution.model = "norm")
  spec2 = dccspec(uspec = multispec( list(specx1, specx2, specx3) ), dccOrder = c(1,1),  distribution = "mvnorm")
  fit2 = dccfit(spec2, data = Dat, fit.control = list(eval.se=T))
  
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0),  include.mean = FALSE), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec3 = dccspec(uspec = multispec( replicate(3, uspec) ), VAR = TRUE, dccOrder = c(1,1),  distribution = "mvnorm")
  fit3 = dccfit(spec3, data = Dat, fit.control = list(eval.se=FALSE))
  
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0),  include.mean = FALSE), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec4 = dccspec(uspec = multispec( replicate(3, uspec) ), VAR = TRUE, lag = 3, dccOrder = c(1,1),  distribution = "mvnorm")
  fit4 = dccfit(spec4, data = Dat, fit.control = list(eval.se=FALSE))
  
  
  spec5 = dccspec(uspec = multispec( list(specx1, specx2, specx3) ), dccOrder = c(1,1), model="aDCC", distribution = "mvnorm")
  fit5 = dccfit(spec5, data = Dat, fit.control = list(eval.se=FALSE))
  
  np = c(
    length(fit1@mfit$matcoef[,1]),
    length(fit2@mfit$matcoef[,1]),
    length(fit3@mfit$matcoef[,1]),
    length(fit4@mfit$matcoef[,1]),
    length(fit5@mfit$matcoef[,1])) +  (3^2 - 3)/2
  aicmod = c(
    rugarch:::.information.test(fit1@mfit$llh, nObs = fit1@model$modeldata$T, 
                                nPars = np[1])[[1]],
    rugarch:::.information.test(fit2@mfit$llh, nObs = fit2@model$modeldata$T, 
                                nPars = np[2])[[1]],
    rugarch:::.information.test(fit3@mfit$llh, nObs = fit3@model$modeldata$T, 
                                nPars = np[3])[[1]],
    rugarch:::.information.test(fit4@mfit$llh, nObs = fit4@model$modeldata$T, 
                                nPars = np[4])[[1]],
    rugarch:::.information.test(fit5@mfit$llh, nObs = fit5@model$modeldata$T, 
                                nPars = np[5])[[1]])
  tmp = data.frame(n.pars = np, AIC = aicmod)
  rownames(tmp) = paste("Model", 1:5, sep = "")
  
  options(width = 120)
  zz <- file("test2a.txt", open="wt")
  sink(zz)	
  print(tmp)
  sink(type="message")
  sink()
  close(zz)
  
  rc1 = rcor(fit1)
  rc2 = rcor(fit2)
  rc3 = rcor(fit3)
  rc4 = rcor(fit4)
  rc5 = rcor(fit5)
  
  postscript("test2a.eps", width = 10, height = 8)
  plot(rc2[1,2, ], type = "l")
  lines(rc5[1,2, ], col = 2)
  legend("topleft", legend = c("DCC", "aDCC"), col= 1:2, fill = 1:2)
  dev.off()
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}

# alternative distributions and check of using fixed pars
rmgarch.test2b = function(cluster = NULL)
{
  tic = Sys.time()
  
  data(dji30retw)
  Dat = dji30retw[, 1:10, drop = FALSE]
  # Well first use the multifit function so that we obtain an object which
  # can be passed to the dccfit, thus elimimating the need to estimate
  # multiple times the first stage (Note that  by default, the dccfit method
  # will always return to the user's workspace an object called ".fitlist"
  # with the 1-stage univariate fit (as a multifit object). This is particularly 
  # useful when there are some non-convergent first stage fits which one
  # can manually check and re-run as explained below:
  
  # 2 stage estimation should usually always use Normal for 1-stage (see below for student)
  uspec = ugarchspec(mean.model = list(armaOrder = c(1,1)), 
                     variance.model = list(garchOrder = c(1,1), model = "gjrGARCH", variance.targeting=TRUE), 
                     distribution.model = "norm")
  mspec = multispec( replicate(10, uspec) )
  multf = multifit(mspec, Dat, cluster = cluster)
  spec1 = dccspec(uspec = mspec, dccOrder = c(1,1), 
                  distribution = "mvnorm")
  fit1 = dccfit(spec1, data = Dat, fit.control = list(eval.se=TRUE), 
                fit = multf, cluster = cluster)
  spec1a = dccspec(uspec = mspec, dccOrder = c(1,1), 
                   model="aDCC", distribution = "mvnorm")
  fit1a = dccfit(spec1a, data = Dat, fit.control = list(eval.se=TRUE), 
                 fit = multf, cluster = cluster, solver.control=list(trace=1))
  
  spec2 = dccspec(uspec = mspec, dccOrder = c(1,1),  
                  distribution = "mvlaplace")
  fit2 = dccfit(spec2, data = Dat, fit.control = list(eval.se=TRUE), 
                fit = multf, cluster = cluster)
  
  spec2a = dccspec(uspec = mspec, dccOrder = c(1,1), model="aDCC", 
                   distribution = "mvlaplace")
  fit2a = dccfit(spec2a, data = Dat, fit.control = list(eval.se=TRUE), 
                 fit = multf, cluster = cluster)
  
  # The proper way to estimate the DCC-Student Model in a 2-stage setup
  uspec = ugarchspec(
    mean.model = list(armaOrder = c(1,1)), 
    variance.model = list(garchOrder = c(1,1), model = "eGARCH", variance.targeting=TRUE), 
    distribution.model = "std", fixed.pars = list(shape=5))
  spec3 = dccspec(uspec = multispec( replicate(10, uspec) ), dccOrder = c(1,1), 
                  distribution = "mvt", fixed.pars = list(mshape = 5))
  fit3 = dccfit(spec3, data = Dat, solver = "solnp", 
                fit.control = list(eval.se=TRUE), cluster = cluster)
  
  # 
  multfstd = multifit(multispec( replicate(10, uspec) ), Dat)
  spec3a = dccspec(uspec = multispec( replicate(10, uspec) ), dccOrder = c(1,1), 
                   model="aDCC", distribution = "mvt", fixed.pars = list(mshape = 5))
  fit3a = dccfit(spec3a, data = Dat, solver = "solnp", fit.control = list(eval.se=TRUE),
                 fit = multfstd, cluster = cluster)
  
  
  # create a table for the DCC coefficients
  dccf = matrix(NA, ncol = 6, nrow = 4)
  dccp = matrix(NA, ncol = 6, nrow = 3)
  dccf[1:2,1] = coef(fit1, "dcc")
  dccf[4, 1] = likelihood(fit1)
  dccp[1:2, 1] = c(fit1@mfit$matcoef["[Joint]dcca1", 4], fit1@mfit$matcoef["[Joint]dccb1", 4])
  dccf[1:3,2] = coef(fit1a, "dcc")
  dccf[4, 2]  = likelihood(fit1a)
  dccp[1:3, 2] = c(fit1a@mfit$matcoef["[Joint]dcca1", 4], 
                   fit1a@mfit$matcoef["[Joint]dccb1", 4],
                   fit1a@mfit$matcoef["[Joint]dccg1", 4])
  
  dccf[1:2,3] = coef(fit2, "dcc")
  dccf[4,3] = likelihood(fit2)
  dccp[1:2, 3] = c(fit2@mfit$matcoef["[Joint]dcca1", 4], fit2@mfit$matcoef["[Joint]dccb1", 4])
  
  dccf[1:3,4] = coef(fit2a, "dcc")
  dccf[4,4] = likelihood(fit2a)
  dccp[1:3, 4] = c(fit2a@mfit$matcoef["[Joint]dcca1", 4], 
                   fit2a@mfit$matcoef["[Joint]dccb1", 4],
                   fit2a@mfit$matcoef["[Joint]dccg1", 4])
  
  dccf[1:2,5] = coef(fit3, "dcc")[1:2]
  dccf[4,5] = likelihood(fit3)
  dccp[1:2, 5] = c(fit3@mfit$matcoef["[Joint]dcca1", 4], fit3@mfit$matcoef["[Joint]dccb1", 4])
  
  dccf[1:3,6] = coef(fit3a, "dcc")[1:3]
  dccf[4,6] = likelihood(fit3a)
  dccp[1:3, 6] = c(fit3a@mfit$matcoef["[Joint]dcca1", 4], 
                   fit3a@mfit$matcoef["[Joint]dccb1", 4],
                   fit3a@mfit$matcoef["[Joint]dccg1", 4])
  
  dccfdf = as.data.frame(dccf)
  starsdf = apply(dccp, 2, FUN = function(x) rugarch:::.stars(x, levels = c(0.01, 0.05, 0.1)))
  for(i in 1:6){
    for(j in 1:3){
      if(!is.na(dccf[j,i])) 
        dccfdf[j, i] = paste(as.character(round(dccf[j, i],5)), starsdf[j,i],sep="")
      else
        dccfdf[j, i] = ""
    }
    dccfdf[4,i] = as.character(round(dccf[4,i], 2))
  }
  colnames(dccfdf) = c("DCC-MVN", "aDCC-MVN", "DCC-MVL", "aDCC-MVL", "DCC-T[5]", "aDCC-T[5]")
  rownames(dccfdf) = c("a", "b", "g", "LL")
  
  options(width = 120)
  zz <- file("test2b1.txt", open="wt")
  sink(zz)
  print(dccfdf)
  sink(type="message")
  sink()
  close(zz)
  
  rc1 = rcor(fit1)
  rc2 = rcor(fit2)
  rc3 = rcor(fit3)
  rc1a = rcor(fit1a)
  rc2a = rcor(fit2a)
  rc3a = rcor(fit3a)
  
  postscript("test2b1.eps", width = 10, height = 8)
  par(mfrow = c(2,1))
  plot(rc1[1,2, ], type = "l", main = "DCC")
  lines(rc2[1,2, ], col = 3)
  lines(rc3[1,2, ], col = 4)
  legend("topleft", legend = c("MVNORM", "MVLAPLACE", "MVT[5]"), col= c(1,3,4), fill = c(1,3,4), bty="n")
  
  plot(rc1a[1,2, ], type = "l", main = "aDCC")
  lines(rc2a[1,2, ], col = 3)
  lines(rc3a[1,2, ], col = 4)
  legend("topleft", legend = c("MVNORM", "MVLAPLACE", "MVT[5]"), col= c(1,3,4), fill = c(1,3,4), bty="n")
  dev.off()
  
  # weighted margins (elliptical distributions)
  
  # fitted returns mu of size equal to dataset#
  port1 = wmargin("mvnorm", weights = rep(1/10,10), Sigma = rcov(fit1), mean = fitted(fit1))
  qport1 = qdist("norm", 0.01, port1[,1], port1[,2], 0, 0, 0)
  
  
  port2 = wmargin("mvlaplace", weights = rep(1/10,10), Sigma = rcov(fit2), mean = fitted(fit2))
  # REM: GED (with shape = 1) == Laplace
  # qport2 = apply(port2, 1, FUN = function(x) qdist("ged", 0.01, x[1], x[2], 0, 0, 1))
  qport2 = qdist("ged", 0.01, port2[,1], port2[,2], 0, 0, 1)
  
  port3 = wmargin("mvt", weights = rep(1/10,10), Sigma = rcov(fit3), mean = fitted(fit3), shape = 5, skew =0 )
  qport3 = qdist("std", 0.01, port3[,1], port3[,2], 0, 0, port3[,4])
  actual = apply(Dat, 1, "mean")
  
  postscript("test2b2.eps", width = 10, height = 8)
  VaRplot(0.01, xts::as.xts(actual), xts::xts(qport1, as.POSIXct(rownames(Dat))))
  lines(xts::xts(qport3, as.POSIXct(rownames(Dat))), col = 3)
  lines(xts::xts(qport2, as.POSIXct(rownames(Dat))), col = 4)
  legend("topright", legend = c("MVNORM", "MVLAPLACE", "MVT[5]"), bty="n", col= c(1,3,4), fill = c(1,3,4))
  dev.off()
  
  options(width = 120)
  zz <- file("test2b2.txt", open="wt")
  sink(zz)
  rugarch:::.VaRreport("Port", "DCC", "MVNORM", 0.01, actual, qport1)
  rugarch:::.VaRreport("Port", "DCC", "MVLAPLACE", 0.01, actual, qport2)
  rugarch:::.VaRreport("Port", "DCC", "MVT[5]", 0.01, actual, qport3)
  sink(type="message")
  sink()
  close(zz)
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}

# use of VAR for 1-stage
rmgarch.test2c = function(cluster = NULL)
{
  tic = Sys.time()
  
  data(dji30retw)
  Dat = dji30retw[, 1:10, drop = FALSE]
  # We can either let dccfit fit everything (VAR and garch), else we can
  # supply the VAR.fit and garch (fit) objects which are pre-estimated (so
  # that we do not have to estimate them everytime we change 2-stage details).
  
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                     variance.model = list(garchOrder = c(1,1), model = "eGARCH"), 
                     distribution.model = "norm")
  mspec = multispec( replicate(10, uspec) )
  
  VAR.fit = varxfit(Dat, p = 2, postpad = "constant")
  resx = VAR.fit$xresiduals
  multf = multifit(mspec, data = resx, cluster = cluster)
  
  spec1 = dccspec(uspec = mspec, dccOrder = c(1,1), VAR = TRUE, lag = 2, 
                  distribution = "mvnorm")
  fit1 = dccfit(spec1, data = Dat, fit.control = list(eval.se=TRUE), 
                VAR.fit = VAR.fit, fit = multf, cluster = cluster)
  
  # check that we have the same answers when not passing pre-fitted objects:
  # (will not work for the robust version which has simulation embedded).
  fitcheck = dccfit(spec1, data = Dat, fit.control = list(eval.se=FALSE), 
                    cluster = cluster)
  
  options(width = 120)
  zz <- file("test2c1.txt", open="wt")
  sink(zz)
  print( all.equal( head(fitted(fitcheck)), head(fitted(fit1)) ) )
  print( all.equal( last(rcov(fitcheck), 1)[,,1], last(rcov(fitcheck), 1)[,,1] ) )
  print( all.equal( first(rcov(fitcheck), 1)[,,1], first(rcov(fitcheck), 1)[,,1] ) )
  sink(type="message")
  sink()
  close(zz)
  
  spec1a = dccspec(uspec = mspec, dccOrder = c(1,1), VAR = TRUE, lag = 2, 
                   model="aDCC", distribution = "mvnorm")
  fit1a = dccfit(spec1a, data = Dat, fit.control = list(eval.se=TRUE), 
                 VAR.fit = VAR.fit, fit = multf, cluster = cluster)
  
  spec2 = dccspec(uspec = mspec, dccOrder = c(1,1), VAR = TRUE, lag = 2,  
                  distribution = "mvlaplace")
  fit2 = dccfit(spec2, data = Dat, fit.control = list(eval.se=TRUE), 
                VAR.fit = VAR.fit, fit = multf, cluster = cluster)
  
  spec2a = dccspec(uspec = mspec, dccOrder = c(1,1), VAR = TRUE, lag = 2, model="aDCC", 
                   distribution = "mvlaplace")
  fit2a = dccfit(spec2a, data = Dat, fit.control = list(eval.se=TRUE), 
                 VAR.fit = VAR.fit, fit = multf, cluster = cluster)
  
  # The proper way to estimate the DCC-Student Model in a 2-stage setup
  # here we only pass the VAR.fit since the garch model is different
  uspec = ugarchspec(
    mean.model = list(armaOrder = c(1,1)), 
    variance.model = list(garchOrder = c(1,1), model = "eGARCH"), 
    distribution.model = "std", fixed.pars = list(shape=5))
  spec3 = dccspec(uspec = multispec( replicate(10, uspec) ), dccOrder = c(1,1), 
                  VAR = TRUE, lag = 2,  distribution = "mvt", fixed.pars = list(mshape = 5))
  fit3 = dccfit(spec3, data = Dat, solver = "solnp", 
                VAR.fit = VAR.fit, fit.control = list(eval.se=TRUE), parallel = parallel, 
                parallel.control = parallel.control)
  
  spec3a = dccspec(uspec = multispec( replicate(10, uspec) ), dccOrder = c(1,1), 
                   VAR = TRUE, lag = 2, model="aDCC", distribution = "mvt", fixed.pars = list(mshape = 5))
  fit3a = dccfit(spec3a, data = Dat, solver = "solnp", fit.control = list(eval.se=TRUE),
                 VAR.fit = VAR.fit, cluster = cluster)
  
  
  # create a table for the DCC coefficients
  dccf = matrix(NA, ncol = 6, nrow = 4)
  dccp = matrix(NA, ncol = 6, nrow = 3)
  dccf[1:2,1] = coef(fit1, "dcc")
  dccf[4, 1] = likelihood(fit1)
  dccp[1:2, 1] = c(fit1@mfit$matcoef["[Joint]dcca1", 4], fit1@mfit$matcoef["[Joint]dccb1", 4])
  dccf[1:3,2] = coef(fit1a, "dcc")
  dccf[4, 2]  = likelihood(fit1a)
  dccp[1:3, 2] = c(fit1a@mfit$matcoef["[Joint]dcca1", 4], 
                   fit1a@mfit$matcoef["[Joint]dccb1", 4],
                   fit1a@mfit$matcoef["[Joint]dccg1", 4])
  
  dccf[1:2,3] = coef(fit2, "dcc")
  dccf[4,3] = likelihood(fit2)
  dccp[1:2, 3] = c(fit2@mfit$matcoef["[Joint]dcca1", 4], fit2@mfit$matcoef["[Joint]dccb1", 4])
  
  dccf[1:3,4] = coef(fit2a, "dcc")
  dccf[4,4] = likelihood(fit2a)
  dccp[1:3, 4] = c(fit2a@mfit$matcoef["[Joint]dcca1", 4], 
                   fit2a@mfit$matcoef["[Joint]dccb1", 4],
                   fit2a@mfit$matcoef["[Joint]dccg1", 4])
  
  dccf[1:2,5] = coef(fit3, "dcc")[1:2]
  dccf[4,5] = likelihood(fit3)
  dccp[1:2, 5] = c(fit3@mfit$matcoef["[Joint]dcca1", 4], fit3@mfit$matcoef["[Joint]dccb1", 4])
  
  dccf[1:3,6] = coef(fit3a, "dcc")[1:3]
  dccf[4,6] = likelihood(fit3a)
  dccp[1:3, 6] = c(fit3a@mfit$matcoef["[Joint]dcca1", 4], 
                   fit3a@mfit$matcoef["[Joint]dccb1", 4],
                   fit3a@mfit$matcoef["[Joint]dccg1", 4])
  
  dccfdf = as.data.frame(dccf)
  starsdf = apply(dccp, 2, FUN = function(x) rugarch:::.stars(x, levels = c(0.01, 0.05, 0.1)))
  for(i in 1:6){
    for(j in 1:3){
      if(!is.na(dccf[j,i])) 
        dccfdf[j, i] = paste(as.character(round(dccf[j, i],5)), starsdf[j,i],sep="")
      else
        dccfdf[j, i] = ""
    }
    dccfdf[4,i] = as.character(round(dccf[4,i], 2))
  }
  colnames(dccfdf) = c("DCC-MVN", "aDCC-MVN", "DCC-MVL", "aDCC-MVL", "DCC-T[5]", "aDCC-T[5]")
  rownames(dccfdf) = c("a", "b", "g", "LL")
  
  options(width = 120)
  zz <- file("test2c1.txt", open="wt")
  sink(zz)
  print(dccfdf)
  sink(type="message")
  sink()
  close(zz)
  
  rc1 = rcor(fit1)
  rc2 = rcor(fit2)
  rc3 = rcor(fit3)
  rc1a = rcor(fit1a)
  rc2a = rcor(fit2a)
  rc3a = rcor(fit3a)
  
  postscript("test2c1.eps", width = 10, height = 8)
  par(mfrow = c(2,1))
  plot(rc1[1,2, ], type = "l", main = "DCC")
  lines(rc2[1,2, ], col = 3)
  lines(rc3[1,2, ], col = 4)
  legend("topleft", legend = c("MVNORM", "MVLAPLACE", "MVT[5]"), col= c(1,3,4), fill = c(1,3,4), bty="n")
  
  plot(rc1a[1,2, ], type = "l", main = "aDCC")
  lines(rc2a[1,2, ], col = 3)
  lines(rc3a[1,2, ], col = 4)
  legend("topleft", legend = c("MVNORM", "MVLAPLACE", "MVT[5]"), col= c(1,3,4), fill = c(1,3,4), bty="n")
  
  dev.off()
  
  # weighted margins (elliptical distributions)
  # fitted returns mu of size equal to dataset#
  port1 = wmargin("mvnorm", weights = rep(1/10,10), Sigma = rcov(fit1), mean = fitted(fit1))
  qport1 = qdist("norm", 0.01, port1[,1], port1[,2], 0, 0, 0)
  
  
  port2 = wmargin("mvlaplace", weights = rep(1/10,10), Sigma = rcov(fit2), mean = fitted(fit2))
  # REM: GED (with shape = 1) == Laplace
  # qport2 = apply(port2, 1, FUN = function(x) qdist("ged", 0.01, x[1], x[2], 0, 0, 1))
  qport2 = qdist("ged", 0.01, port2[,1], port2[,2], 0, 0, 1)
  
  port3 = wmargin("mvt", weights = rep(1/10,10), Sigma = rcov(fit3), mean = fitted(fit3), shape = 5, skew =0 )
  qport3 = qdist("std", 0.01, port3[,1], port3[,2], 0, 0, port3[,4])
  actual = apply(Dat, 1, "mean")
  
  postscript("test2c2.eps", width = 10, height = 8)
  VaRplot(0.01, xts::as.xts(actual), xts::xts(qport1, as.POSIXct(rownames(Dat))))
  lines(xts::xts(qport3, as.POSIXct(rownames(Dat))), col = 3)
  lines(xts::xts(qport2, as.POSIXct(rownames(Dat))), col = 4)
  legend("topright", legend = c("MVNORM", "MVLAPLACE", "MVT[5]"), bty="n", col= c(1,3,4), fill = c(1,3,4))
  dev.off()
  
  options(width = 120)
  zz <- file("test2c2.txt", open="wt")
  sink(zz)
  rugarch:::.VaRreport("Port", "DCC", "MVNORM", 0.01, actual, qport1)
  rugarch:::.VaRreport("Port", "DCC", "MVLAPLACE", 0.01, actual, qport2)
  rugarch:::.VaRreport("Port", "DCC", "MVT[5]", 0.01, actual, qport3)
  sink(type="message")
  sink()
  close(zz)
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}

# Filter Tests
rmgarch.test2d = function(cluster = NULL)
{
  tic = Sys.time()
  
  data(dji30retw)
  Dat = dji30retw[, 1:3, drop = FALSE]
  uspec = ugarchspec(mean.model = list(armaOrder = c(1,2)), 
                     variance.model = list(garchOrder = c(1,1), model = "sGARCH",
                                           variance.targeting=TRUE), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(3, uspec) ),  
                  dccOrder = c(1,1),  distribution = "mvnorm")
  fit1 = dccfit(spec1, data = Dat, fit.control = list(eval.se=TRUE))
  
  # for filtering you need to remove variance.targeting:
  uspec = ugarchspec(mean.model = list(armaOrder = c(1,2)), 
                     variance.model = list(garchOrder = c(1,1), model = "sGARCH",
                                           variance.targeting=FALSE), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(3, uspec) ),  
                  dccOrder = c(1,1),  distribution = "mvnorm")
  
  vspec = vector(mode = "list", length = 3)
  midx = fit1@model$midx
  mpars = fit1@model$mpars
  for(i in 1:3){
    vspec[[i]] = uspec
    setfixed(vspec[[i]])<-as.list(mpars[midx[,i]==1, i])
  }
  dccfix = as.list(coef(fit1, type="dcc"))
  spec2 = dccspec(uspec = multispec( vspec ),  
                  dccOrder = c(1,1),  distribution = "mvnorm",
                  fixed.pars = dccfix)
  filt1 = dccfilter(spec2, data = Dat)
  
  rc1 = rcor(fit1)
  rc2 = rcor(filt1)
  
  options(width = 120)
  zz <- file("test2d1.txt", open="wt")
  sink(zz)
  print( all.equal(first(rc1)[,,1], first(rc2)[,,1] ) )
  print( all.equal(last(rc1)[,,1], last(rc2)[,,1] ) )
  print( all.equal(first(rc1, 10), first(rc2, 10) ) )
  print( all.equal(head(fitted(fit1)), head(fitted(filt1))) ) 
  print( all.equal(head(residuals(fit1)), head(residuals(filt1)) )) 
  sink(type="message")
  sink()
  close(zz)
  
  
  #####################################################################
  # With VAR spec pre-fitted
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                     variance.model = list(garchOrder = c(1,1), model = "eGARCH", 
                                           variance.targeting=TRUE), 
                     distribution.model = "norm")
  # remember to pass lag length to dccspec for VAR
  spec2 = dccspec(uspec = multispec( replicate(3, uspec) ), VAR = TRUE, lag = 2, 
                  dccOrder = c(1,1),  distribution = "mvnorm")
  VAR.fit = varxfit(Dat, p = 2, postpad = "constant")
  fit2 = dccfit(spec2, data = Dat, fit.control = list(eval.se=TRUE), VAR.fit = VAR.fit)
  
  # remember to exclude the mean and ARMA if using VAR
  uspec = ugarchspec(mean.model = list(include.mean = FALSE, armaOrder = c(0,0)), 
                     variance.model = list(garchOrder = c(1,1), model = "eGARCH"), 
                     distribution.model = "norm")
  vspec = vector(mode = "list", length = 3)
  midx = fit2@model$midx
  mpars = fit2@model$mpars
  for(i in 1:3){
    vspec[[i]] = uspec
    setfixed(vspec[[i]])<-as.list(mpars[midx[,i]==1, i])
  }
  dccfix = as.list(coef(fit2, "dcc"))
  spec2 = dccspec(uspec = multispec( vspec ),  VAR = TRUE, lag = 2, 
                  dccOrder = c(1,1),  distribution = "mvnorm",
                  fixed.pars = dccfix)
  filt2 = dccfilter(spec2, data = Dat, varcoef = VAR.fit$Bcoef)
  
  rc1 = rcor(fit2)
  rc2 = rcor(filt2)
  options(width = 120)
  zz <- file("test2d2.txt", open="wt")
  sink(zz)
  print( all.equal(first(rc1)[,,1], first(rc2)[,,1]) )
  print( all.equal(last(rc1)[,,1], last(rc2)[,,1]) )
  print( all.equal(first(rc1, 10), first(rc2, 10)) )
  print( all.equal(head(fitted(fit2)), head(fitted(filt2))) ) 
  print( all.equal(head(residuals(fit2)), head(residuals(filt2))) ) 
  sink(type="message")
  sink()
  close(zz)
  
  
  # Filtering new data and keeping old spec
  # VAR and non VAR specification
  # 2 stage estimation should usually always use Normal for 1-stage (see below for student)
  uspec = ugarchspec(mean.model = list(armaOrder = c(1,1)), 
                     variance.model = list(garchOrder = c(2,1), model = "eGARCH"), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(3, uspec) ),  
                  dccOrder = c(1,1),  distribution = "mvnorm")
  fit1 = dccfit(spec1, data = Dat, fit.control = list(eval.se=FALSE), out.sample = 100)
  T = dim(Dat)[1]-100
  vspec = vector(mode = "list", length = 3)
  midx = fit1@model$midx
  mpars = fit1@model$mpars
  for(i in 1:3){
    vspec[[i]] = uspec
    setfixed(vspec[[i]])<-as.list(mpars[midx[,i]==1, i])
  }
  dccfix = as.list(coef(fit1, "dcc"))
  spec2 = dccspec(uspec = multispec( vspec ),  
                  dccOrder = c(1,1),  distribution = "mvnorm",
                  fixed.pars = dccfix)
  filt1 = dccfilter(spec2, data = Dat[1:(T+5), ], filter.control = list(n.old = T))
  
  
  rc1 = rcor(fit1)
  rc2 = rcor(filt1)
  options(width = 120)
  zz <- file("test2d3.txt", open="wt")
  sink(zz)
  print( all.equal(first(rc1)[,,1], first(rc2)[,,1]) )
  # T+5
  print( all.equal(last(rc1)[,,1], last(rc2, 6)[,,1]) )
  print( all.equal(first(rc1, 10), first(rc2, 10)) )
  print( all.equal(head(fitted(fit1)), head(fitted(filt1))) ) 
  print( all.equal(head(residuals(fit1)), head(residuals(filt1))) ) 
  sink(type="message")
  sink()
  close(zz)
  
  
  ##########################################
  # with VAR
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                     variance.model = list(garchOrder = c(1,1), model = "eGARCH"), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(3, uspec) ),  
                  dccOrder = c(1,1),  VAR = TRUE, lag = 2, distribution = "mvnorm")
  fit1 = dccfit(spec1, data = Dat, fit.control = list(eval.se=FALSE), out.sample = 100)
  
  T = dim(Dat)[1]-100
  VAR.fit = varxfilter(Dat[1:(T+5), ], p = 2, Bcoef = fit1@model$varcoef, postpad = "constant")
  
  vspec = vector(mode = "list", length = 3)
  midx = fit1@model$midx
  mpars = fit1@model$mpars
  for(i in 1:3){
    vspec[[i]] = uspec
    setfixed(vspec[[i]])<-as.list(mpars[midx[,i]==1, i])
  }
  dccfix = as.list(coef(fit1, "dcc"))
  spec2 = dccspec(uspec = multispec( vspec ),  
                  dccOrder = c(1,1),  VAR = TRUE, lag = 2, distribution = "mvnorm",
                  fixed.pars = dccfix)
  filt1 = dccfilter(spec2, data = Dat[1:(T+5), ], filter.control = list(n.old = T), varcoef = VAR.fit$Bcoef)
  
  
  rc1 = rcor(fit1)
  rc2 = rcor(filt1)
  options(width = 120)
  zz <- file("test2d4.txt", open="wt")
  sink(zz)
  print( all.equal(first(rc1)[,,1], first(rc2)[,,1]) )
  # T+5
  print( all.equal(last(rc1)[,,1], last(rc2, 6)[,,1]) )
  print( all.equal(first(rc1, 10), first(rc2, 10)) )
  print( all.equal(head(fitted(fit1)), head(fitted(filt1))) ) 
  print( all.equal(head(residuals(fit1)), head(residuals(filt1))) ) 
  sink(type="message")
  sink()
  close(zz)
  
  
  ##########################################
  # one more check
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                     variance.model = list(garchOrder = c(2,1), model = "eGARCH"), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(3, uspec) ),  
                  dccOrder = c(1,1),  VAR = TRUE, lag = 1, distribution = "mvnorm")
  fit1 = dccfit(spec1, data = Dat, fit.control = list(eval.se=FALSE), out.sample = 100)
  
  T = dim(Dat)[1]-100
  VAR.fit = varxfilter(Dat[1:(T+5), ], p = 1, Bcoef = fit1@model$varcoef, postpad = "constant")
  
  vspec = vector(mode = "list", length = 3)
  midx = fit1@model$midx
  mpars = fit1@model$mpars
  for(i in 1:3){
    vspec[[i]] = uspec
    setfixed(vspec[[i]])<-as.list(mpars[midx[,i]==1, i])
  }
  dccfix = as.list(coef(fit1, "dcc"))
  spec2 = dccspec(uspec = multispec( vspec ),  
                  dccOrder = c(1,1),  VAR = TRUE, lag = 1, distribution = "mvnorm",
                  fixed.pars = dccfix)
  filt1 = dccfilter(spec2, data = Dat[1:(T+5), ], filter.control = list(n.old = T), varcoef = VAR.fit$Bcoef)
  
  
  rc1 = rcor(fit1)
  rc2 = rcor(filt1)
  options(width = 120)
  zz <- file("test2d5.txt", open="wt")
  sink(zz)
  print( all.equal(first(rc1)[,,1], first(rc2)[,,1]) )
  # T+5
  print( all.equal(last(rc1)[,,1], last(rc2, 6)[,,1]) )
  print( all.equal(first(rc1, 10), first(rc2, 10)) )
  print( all.equal(head(fitted(fit1)), head(fitted(filt1))) ) 
  print( all.equal(head(residuals(fit1)), head(residuals(filt1))) ) 
  sink(type="message")
  sink()
  close(zz)
  
  ##########################################
  # and one more dcc(2,1)
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                     variance.model = list(garchOrder = c(1,1), model = "eGARCH"), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(3, uspec) ),  
                  dccOrder = c(2,1),  VAR = TRUE, lag = 1, distribution = "mvnorm")
  fit1 = dccfit(spec1, data = Dat, fit.control = list(eval.se=FALSE), out.sample = 100)
  
  T = dim(Dat)[1]-100
  VAR.fit = varxfilter(Dat[1:(T+5), ], p = 1, Bcoef = fit1@model$varcoef, postpad = "constant")
  
  vspec = vector(mode = "list", length = 3)
  midx = fit1@model$midx
  mpars = fit1@model$mpars
  for(i in 1:3){
    vspec[[i]] = uspec
    setfixed(vspec[[i]])<-as.list(mpars[midx[,i]==1, i])
  }
  dccfix = as.list(coef(fit1, "dcc"))
  spec2 = dccspec(uspec = multispec( vspec ),  
                  dccOrder = c(2,1),  VAR = TRUE, lag = 1, distribution = "mvnorm",
                  fixed.pars = dccfix)
  filt1 = dccfilter(spec2, data = Dat[1:(T+5), ], filter.control = list(n.old = T), varcoef = VAR.fit$Bcoef)
  
  
  rc1 = rcor(fit1)
  rc2 = rcor(filt1)
  options(width = 120)
  zz <- file("test2d6.txt", open="wt")
  sink(zz)
  print( all.equal(first(rc1)[,,1], first(rc2)[,,1]) )
  # T+5
  print( all.equal(last(rc1)[,,1], last(rc2, 6)[,,1]) )
  print( all.equal(first(rc1, 10), first(rc2, 10)) )
  print( all.equal(head(fitted(fit1)), head(fitted(filt1))) ) 
  print( all.equal(head(residuals(fit1)), head(residuals(filt1))) ) 
  sink(type="message")
  sink()
  close(zz)
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}

# Simulation Tests (check that sim from fit and spec return same results)
rmgarch.test2e = function(cluster = NULL)
{
  tic = Sys.time()
  
  data(dji30retw)
  Dat = dji30retw[, 1:3, drop = FALSE]
  cnames = colnames(Dat)
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), variance.model = list(garchOrder = c(1,1), model = "eGARCH"), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(3, uspec) ), dccOrder = c(1,1), 
                  distribution = "mvnorm")
  fit1 = dccfit(spec1, data = Dat, fit.control = list(eval.se=FALSE))
  
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), 
                     variance.model = list(garchOrder = c(1,1), model = "eGARCH"), 
                     distribution.model = "norm")
  vspec = vector(mode = "list", length = 3)
  midx = fit1@model$midx
  mpars = fit1@model$mpars
  for(i in 1:3){
    vspec[[i]] = uspec
    setfixed(vspec[[i]])<-as.list(mpars[midx[,i]==1, i])
  }
  dccfix = as.list(coef(fit1, "dcc"))
  spec2 = dccspec(uspec = multispec( vspec ), 
                  dccOrder = c(1,1),  distribution = "mvnorm",
                  fixed.pars = dccfix)
  
  presigma = tail( sigma(fit1 ), 2 )
  preresiduals = tail( residuals(fit1), 2 )
  prereturns = tail( as.matrix(Dat), 2 )
  sim1 = dccsim(fitORspec = fit1, n.sim = 1000, n.start = 100, m.sim = 2, startMethod = "unconditional", 
                presigma = presigma, preresiduals = preresiduals, prereturns = prereturns, 
                preQ = last(rcor(fit1, type = "Q"))[,,1], Qbar = fit1@mfit$Qbar, 
                preZ = tail(fit1@mfit$stdresid, 1),
                rseed = c(100, 200), mexsimdata = NULL, vexsimdata = NULL)
  
  
  sim2 = dccsim(fitORspec = spec2, n.sim = 1000, n.start = 100, m.sim = 2,
                presigma = presigma, preresiduals = preresiduals, prereturns = prereturns, 
                preQ = last(rcor(fit1, type = "Q"))[,,1], Qbar = fit1@mfit$Qbar, 
                preZ = tail(fit1@mfit$stdresid, 1),
                rseed = c(100, 200), mexsimdata = NULL, vexsimdata = NULL)
  
  sim3 = dccsim(fitORspec = fit1, n.sim = 1000, n.start = 100, m.sim = 2, startMethod = "sample", 
                rseed = c(100, 200), mexsimdata = NULL, vexsimdata = NULL)
  
  
  rc1 = rcor(sim1, sim = 1)
  rc2 = rcor(sim2, sim = 1)
  rc3 = rcor(sim3, sim = 1)
  rh1 = rcov(sim1, sim = 2)
  rh2 = rcov(sim2, sim = 2)
  rh3 = rcov(sim2, sim = 2)
  
  options(width = 120)
  zz <- file("test2e1.txt", open="wt")
  sink(zz)
  print( all.equal(first(rc1)[,,1], first(rc2)[,,1], first(rc3)[,,1], check.attributes = FALSE))
  print( all.equal(last(rc1)[,,1], last(rc2)[,,1], last(rc3)[,,1], check.attributes = FALSE))
  print( all.equal(first(rh1)[,,1], first(rh2)[,,1], first(rh3)[,,1], check.attributes = FALSE))
  print( all.equal(last(rh1)[,,1], last(rh2)[,,1], last(rh3)[,,1], check.attributes = FALSE))
  print( all.equal(head(fitted(sim1)), head(fitted(sim2)), head(fitted(sim3)), check.attributes = FALSE))
  print( all.equal(head(fitted(sim1, sim=2)), head(fitted(sim2, sim=2)), head(fitted(sim3, sim=2)), check.attributes = FALSE))
  sink(type="message")
  sink()
  close(zz)
  
  # Now some 1-ahead rolling tests
  
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), 
                     variance.model = list(garchOrder = c(1,1), model = "apARCH"), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(3, uspec) ), dccOrder = c(1,1), 
                  distribution = "mvnorm")
  fit1 = dccfit(spec1, data = Dat, out.sample = 100, fit.control = list(eval.se=FALSE))
  
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), 
                     variance.model = list(garchOrder = c(1,1), model = "apARCH"), 
                     distribution.model = "norm")
  vspec = vector(mode = "list", length = 3)
  midx = fit1@model$midx
  mpars = fit1@model$mpars
  for(i in 1:3){
    vspec[[i]] = uspec
    setfixed(vspec[[i]])<-as.list(mpars[midx[,i]==1, i])
  }
  dccfix = as.list(coef(fit1, "dcc"))
  spec2 = dccspec(uspec = multispec( vspec ), 
                  dccOrder = c(1,1),  distribution = "mvnorm",
                  fixed.pars = dccfix)
  
  rcovfilt = rcorfilt = rcovsim = rcorsim = array(NA, dim = c(3,3,100))
  T = dim(Dat)[1] - 100
  p = 2
  preQ = last(rcor(fit1, type = "Q"))[,,1]
  presigma = tail( sigma(fit1 ), 2 )
  preresiduals = tail( residuals(fit1), 2 )
  prereturns = tail( as.matrix(Dat[1:T,]), 2 )
  
  filt = dccfilter(spec2, data = Dat[1:T,], filter.control = list(n.old = T))
  for(i in 1:100){
    preQ = last(rcor(filt, type = "Q"))[,,1]
    presigma = tail( sigma(filt), 2 )
    preresiduals = tail( residuals(filt), 2 )
    prereturns = tail( as.matrix(Dat[1:(T+i-1),]), 2 )
    preZ = tail(filt@mfilter$stdresid, 1)
    # when (i == 1) equivalent to fit1
    if(i == 1){
      print( all.equal(last(rcor(filt))[,,1], last(rcor(fit1))[,,1]) )
      print( all.equal(first(rcov(filt))[,,1], first(rcov(fit1))[,,1]) )
      print( all.equal(tail(residuals(filt)), tail(residuals(fit1))) )
    }
    sim = dccsim(fitORspec = spec2, n.sim = 1, n.start = 0, m.sim = 100, 
                 presigma = presigma, preresiduals = preresiduals, 
                 prereturns = prereturns, preQ = preQ, preZ = preZ, Qbar = fit1@mfit$Qbar)
    filt = dccfilter(spec2, data = Dat[1:(T+i),], filter.control = list(n.old = T))
    tmp = matrix(0, 3, 3)
    for(j in 1:100) tmp = tmp + sim@msim$simH[[j]][,,1]
    tmp = tmp/100
    rcovsim[,,i] = tmp
    rcorsim[,,i] = cov2cor(tmp)
    rcovfilt[,,i] = last(rcov(filt), 1)[,,1]
    rcorfilt[,,i] = last(rcor(filt), 1)[,,1]
    # check upto point T that it agress with fit:
    #print(all.equal(last(rcov(filt), 2)[,,1], last(rcov(fit1), 1)[,,1]))
    #print(all.equal(first(rcov(filt), 1)[,,1],first(rcov(fit1), 1)[,,1]))
    # check upto point T that it agress with fit:
    print(i)
  }
  
  postscript("test2e1.eps", width = 16, height = 14)
  par(mfrow = c(2,2))
  plot(rcovfilt[1,2,], type = "l", main = paste(cnames[1], "-", cnames[2], sep = ""),
       ylab = "covariance", xlab = "periods [out of sample]")
  lines(rcovsim[1,2,], col = 2, lty = 2)
  legend("topleft", legend = c("Filtered Covariance", "Simulated Covariance"), col = 1:2, lty = 1:2,
         bty = "n")
  
  plot(rcovfilt[2,3,], type = "l", main = paste(cnames[2], "-", cnames[3], sep = ""),
       ylab = "covariance", xlab = "periods [out of sample]")
  lines(rcovsim[2,3,], col = 2, lty = 2)
  legend("topleft", legend = c("Filtered Covariance", "Simulated Covariance"), col = 1:2, lty = 1:2,
         bty = "n")
  
  plot(rcorfilt[1,2,], type = "l", main = paste(cnames[1], "-", cnames[2], sep = ""),
       ylab = "correlation", xlab = "periods [out of sample]")
  lines(rcorsim[1,2,], col = 2, lty = 2)
  legend("topleft", legend = c("Filtered Correlation", "Simulated Correlation"), col = 1:2, lty = 1:2,
         bty = "n")
  plot(rcorfilt[2,3,], type = "l", main = paste(cnames[2], "-", cnames[3], sep = ""),
       ylab = "correlation", xlab = "periods [out of sample]")
  lines(rcorsim[2,3,], col = 2, lty = 2)
  legend("topleft", legend = c("Filtered Correlation", "Simulated Correlation"), col = 1:2, lty = 1:2,
         bty = "n")
  dev.off()
  
  
  
  # Check Sim by fit and Sim by Spec using VAR:
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                     variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(3, uspec) ), dccOrder = c(1,1), 
                  distribution = "mvnorm", VAR = TRUE, lag = 2)
  fit1 = dccfit(spec1, data = Dat, out.sample = 100, fit.control = list(eval.se=FALSE))
  
  T = dim(Dat)[1] - 100
  VAR.fit = varxfit(Dat[1:T,], p = 2, postpad = "constant")
  vspec = vector(mode = "list", length = 3)
  midx = fit1@model$midx
  mpars = fit1@model$mpars
  for(i in 1:3){
    vspec[[i]] = uspec
    setfixed(vspec[[i]])<-as.list(mpars[midx[,i]==1, i])
  }
  dccfix = as.list(coef(fit1, "dcc"))
  spec2 = dccspec(uspec = multispec( vspec ), VAR = TRUE, lag = 2, 
                  dccOrder = c(1,1),  distribution = "mvnorm",
                  fixed.pars = dccfix)
  
  presigma = tail( sigma(fit1 ), 2 )
  prereturns = as.matrix(tail(Dat[1:T,], 2))
  preresiduals = as.matrix(tail(residuals(fit1), 2))
  sim1 = dccsim(fitORspec = fit1, n.sim = 1, n.start = 0, m.sim = 100, startMethod = "sample", 
                presigma = presigma, prereturns = prereturns, rseed = 1:100)
  
  preQ = last(rcor(fit1, type = "Q"))[,,1]
  preZ = tail(fit1@mfit$stdresid, 1)
  sim2 = dccsim(fitORspec = spec2, n.sim = 1, n.start = 0, m.sim = 100, startMethod = "sample", 
                presigma = presigma, Qbar = fit1@mfit$Qbar, preQ = preQ, preZ = preZ, 
                prereturns = prereturns, preresiduals = preresiduals, rseed = 1:100, VAR.fit = VAR.fit)
  
  
  rc1 = rcor(sim1, sim = 1)
  rc2 = rcor(sim2, sim = 1)
  rh1 = rcov(sim1, sim = 2)
  rh2 = rcov(sim2, sim = 2)
  
  options(width = 120)
  zz <- file("test2e2.txt", open="wt")
  sink(zz)
  print( all.equal(first(rc1)[,,1], first(rc2)[,,1], check.attributes=FALSE) )
  print( all.equal(last(rc1)[,,1], last(rc2)[,,1], check.attributes=FALSE) )
  print( all.equal(first(rh1)[,,1], first(rh2)[,,1], check.attributes=FALSE) )
  print( all.equal(last(rh1)[,,1], last(rh2)[,,1], check.attributes=FALSE) )
  print( all.equal(head(fitted(sim1)), head(fitted(sim2)), check.attributes=FALSE) )
  print( all.equal(head(fitted(sim1, sim=2)), head(fitted(sim2, sim=2)), check.attributes=FALSE) )
  sink(type="message")
  sink()
  close(zz)
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}

# Forecast Tests 
rmgarch.test2f = function(cluster = NULL)
{
  tic = Sys.time()
  
  data(dji30retw)
  Dat = dji30retw[, 1:3, drop = FALSE]
  
  #---------------------------------------------------------------------------------------------------------------
  # ARMA-GARCH-DCC
  cnames = colnames(Dat)
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), variance.model = list(garchOrder = c(1,1), model = "gjrGARCH"), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(3, uspec) ), dccOrder = c(1,1), 
                  distribution = "mvnorm")
  fit1 = dccfit(spec1, data = Dat, out.sample = 100, fit.control = list(eval.se=FALSE))
  T = dim(Dat)[1]-100
  
  forc = dccforecast(fit1, n.ahead = 11, n.roll = 0)
  forc2 = dccforecast(fit1, n.ahead = 1, n.roll = 10)
  
  # forcx = dccforecast(fit1, n.ahead = 1, n.roll = 99)
  # REM: n.roll = 10 == 11 roll (1 roll is the standard + 10 beyond)
  # Now filter
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), 
                     variance.model = list(garchOrder = c(1,1), model = "gjrGARCH"), 
                     distribution.model = "norm")
  vspec = vector(mode = "list", length = 3)
  midx = fit1@model$midx
  mpars = fit1@model$mpars
  for(i in 1:3){
    vspec[[i]] = uspec
    setfixed(vspec[[i]])<-as.list(mpars[midx[,i]==1, i])
  }
  dccfix = as.list(coef(fit1, "dcc"))
  spec2 = dccspec(uspec = multispec( vspec ), 
                  dccOrder = c(1,1),  distribution = "mvnorm",
                  fixed.pars = dccfix)
  
  # 1-step ahead forecast on out-sample data is equivalent to filtering
  # Note the use of n.old so that filtering assumptions are observed based on
  # the size of the original dataset
  filt1 = dccfilter(spec2, data = Dat[1:(T+100),], filter.control = list(n.old = T))
  
  # check mean and sigma
  options(width = 120)
  zz <- file("test2f1.txt", open="wt")
  sink(zz)
  print(all.equal(fitted(forc)[1,,1], filt1@model$mu[T+1,], fitted(filt1)[T+1,], check.attributes = FALSE))	
  print(all.equal(sigma(forc)[1,,1], filt1@model$sigma[T+1,], sigma(filt1)[T+1,], check.attributes = FALSE))	
  print(all.equal(fitted(forc2)[1,,11], filt1@model$mu[T+11,], fitted(filt1)[T+11,], check.attributes = FALSE))	
  print(all.equal(rcor(forc)[[1]][,,1], rcor(filt1)[,,T+1], check.attributes = FALSE))	
  print(all.equal(rcor(forc2)[[11]][,,1], rcor(filt1)[,,T+11], check.attributes = FALSE))	
  print(all.equal(sigma(forc2)[,,11], filt1@model$sigma[T+11,], sigma(filt1)[T+11,], check.attributes = FALSE))	
  sink(type="message")
  sink()
  close(zz)
  
  
  #---------------------------------------------------------------------------------------------------------------
  # ARMA-GARCH-DCC-2
  cnames = colnames(Dat)
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(3, uspec) ), dccOrder = c(1,1), 
                  distribution = "mvnorm")
  fit1 = dccfit(spec1, data = Dat, out.sample = 500, fit.control = list(eval.se=FALSE))
  T = dim(Dat)[1]-500
  
  forc = dccforecast(fit1, n.ahead = 1, n.roll = 499)
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), 
                     variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  vspec = vector(mode = "list", length = 3)
  midx = fit1@model$midx
  mpars = fit1@model$mpars
  for(i in 1:3){
    vspec[[i]] = uspec
    setfixed(vspec[[i]])<-as.list(mpars[midx[,i]==1, i])
  }
  dccfix = as.list(coef(fit1, "dcc"))
  spec2 = dccspec(uspec = multispec( vspec ), 
                  dccOrder = c(1,1),  distribution = "mvnorm",
                  fixed.pars = dccfix)
  filt1 = dccfilter(spec2, data = Dat[1:(T+500),], filter.control = list(n.old = T))
  
  ###########################################################################
  # DCC Filter vs rolling Forecast
  # The reason for the build up of the difference between the filter and rolling
  # forecast methods is that the filter uses a fixed estimate of the covariance
  # matrix initialization at time T (e.g. n.old), whilst the rolling forecast method
  # updates the initialization value at each iteration (T+1) to include the data upto 
  # point T.
  # plot(rcor(filt1)[1,2,(T+1):(T+500)], type = "l")
  # lines(sapply(rcor(forc), FUN = function(x) x[1,2,1]), col = 2)
  
  
  #---------------------------------------------------------------------------------------------------------------
  # VAR-GARCH-DCC
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                     variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(3, uspec) ), VAR = TRUE, lag = 2, dccOrder = c(1,1), 
                  distribution = "mvnorm")
  fit1 = dccfit(spec1, data = Dat, out.sample = 100, fit.control = list(eval.se=FALSE))
  T = dim(Dat)[1]-100
  
  forc = dccforecast(fit1, n.ahead = 1, n.roll = 10)
  # REM: n.roll = 10 == 11 roll (1 roll is the standard + 10 beyond)
  # Now filter (first the VAR so it is the same as in fit)
  VAR.filt = varxfilter(Dat[1:(T+100),], p = 2, Bcoef = fit1@model$varcoef, postpad = "constant")
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                     variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  vspec = vector(mode = "list", length = 3)
  midx = fit1@model$midx
  mpars = fit1@model$mpars
  for(i in 1:3){
    vspec[[i]] = uspec
    setfixed(vspec[[i]])<-as.list(mpars[midx[,i]==1, i])
  }
  dccfix = as.list(coef(fit1, "dcc"))
  spec2 = dccspec(uspec = multispec( vspec ), VAR = TRUE, lag = 2, 
                  dccOrder = c(1,1),  distribution = "mvnorm",
                  fixed.pars = dccfix)
  
  
  filt1 = dccfilter(spec2, data = Dat[1:(T+100),], filter.control = list(n.old = T), varcoef = VAR.filt$Bcoef)
  
  # check mean and sigma
  options(width = 120)
  zz <- file("test2f2.txt", open="wt")
  sink(zz)
  print(all.equal(as.numeric(fitted(forc)[,,1]), as.numeric(fitted(filt1)[T+1,])))
  print(all.equal(as.numeric(sigma(forc)[,,1]), as.numeric(sigma(filt1)[T+1,])))
  print(all.equal(as.numeric(fitted(forc)[,,11]), as.numeric(fitted(filt1)[T+11,])))
  print(all.equal(as.numeric(sigma(forc)[,,11]), as.numeric(sigma(filt1)[T+11,])))
  print(all.equal(rcor(forc)[[1]][,,1], rcor(filt1)[,,T+1]))
  # the difference is explained in the note above
  print(all.equal(rcor(forc)[[11]][,,1], rcor(filt1)[,,T+11]))
  
  sink(type="message")
  sink()
  close(zz)
  
  
  
  #---------------------------------------------------------------------------------------------------------------
  # VAR(1)-GARCH(1,1)-aDCC(1,2) n.ahead = 1 n.roll = 99
  Dat = dji30retw[,1:10]
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                     variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(10, uspec) ), VAR = TRUE, lag = 1, 
                  dccOrder = c(1,2), model="aDCC", distribution = "mvnorm")
  fit1 = dccfit(spec1, data = Dat, out.sample = 100, solver = "solnp", fit.control = list(eval.se=FALSE))
  
  
  T = dim(Dat)[1]-100
  
  forc = dccforecast(fit1, n.ahead = 1, n.roll = 99)
  # REM: n.roll = 99 == 100 rolls (1 roll is the standard + 99 beyond)
  # Now filter (first the VAR so it is the same as in fit)
  VAR.filt = varxfilter(Dat[1:(T+100),], p = 1, Bcoef = fit1@model$varcoef, postpad = "constant")
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                     variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  vspec = vector(mode = "list", length = 10)
  midx = fit1@model$midx
  mpars = fit1@model$mpars
  for(i in 1:10){
    vspec[[i]] = uspec
    setfixed(vspec[[i]])<-as.list(mpars[midx[,i]==1, i])
  }
  dccfix = as.list(coef(fit1, "dcc"))
  spec2 = dccspec(uspec = multispec( vspec ), VAR = TRUE, lag = 1, 
                  dccOrder = c(1,2), model="aDCC", distribution = "mvnorm",
                  fixed.pars = dccfix)
  
  filt1 = dccfilter(spec2, data = Dat[1:(T+100), ], filter.control = list(n.old = T), varcoef = VAR.filt$Bcoef)
  
  # check mean and sigma
  options(width = 120)
  zz <- file("test2f3.txt", open="wt")
  sink(zz)
  print(all.equal(fitted(forc)[,,1], as.numeric(filt1@model$mu[T+1,]), check.attributes = FALSE))	
  print(all.equal(fitted(forc)[,,1], as.numeric(fitted(filt1)[T+1,]), check.attributes = FALSE))	
  print(all.equal(sigma(forc)[,,1], filt1@model$sigma[T+1,], check.attributes = FALSE))	
  print(all.equal(sigma(forc)[,,1], as.numeric(sigma(filt1)[T+1,]), check.attributes = FALSE))	
  print(all.equal(fitted(forc)[,,11], as.numeric(filt1@model$mu[T+11,]), check.attributes = FALSE))	
  print(all.equal(fitted(forc)[,,11], as.numeric(fitted(filt1)[T+11,]), check.attributes = FALSE))	
  print(all.equal(sigma(forc)[,,11], filt1@model$sigma[T+11,], check.attributes = FALSE))	
  print(all.equal(sigma(forc)[,,11], as.numeric(sigma(filt1)[T+11,]), check.attributes = FALSE))	
  print(all.equal(rcor(forc)[[1]][,,1], rcor(filt1)[,,T+1], check.attributes = FALSE))	
  print(all.equal(rcor(forc)[[11]][,,1], rcor(filt1)[,,T+11], check.attributes = FALSE))	
  
  sink(type="message")
  sink()
  close(zz)
  
  #################################
  data(dji30ret)
  Dat = dji30ret[,1:10]
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                     variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(10, uspec) ), VAR = TRUE, lag = 1, 
                  dccOrder = c(1,2),  distribution = "mvnorm")
  fit1 = dccfit(spec1, data = Dat, out.sample = 100, solver = "solnp", fit.control = list(eval.se=FALSE))	
  
  forc1 = dccforecast(fit1, n.ahead = 1500, n.roll = 0)
  
  forc2 = dccforecast(fit1, n.ahead = 1, n.roll = 99)
  
  
  #plot(forc1, which = 1, series = 1:3)
  #plot(forc2, which = 1, series = 1:3)
  #plot(forc2, which = 4, series = 1:3)
  #plot(forc1, which = 4, series = 2:3)
  #plot(forc2, which = 5)
  #plot(forc1, which = 5)
  
  options(width = 120)
  zz <- file("test2f4.txt", open="wt")
  sink(zz)
  # forecast R --> unconditional R (Rbar) as N --> large
  UQ = fit1@mfit$Qbar*(1-sum(coef(fit1, "dcc")))
  Rbar = UQ/(sqrt(diag(UQ)) %*% t(sqrt(diag(UQ))))
  print(all.equal(Rbar, forc1@mforecast$R[[1]][,,1500]))
  #############################################
  sink(type="message")
  sink()
  close(zz)
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}

# Roliing Tests 
rmgarch.test2g = function(cluster = NULL)
{
  tic = Sys.time()
  
  data(dji30ret)	
  Dat = dji30ret[, 1:5, drop = FALSE]
  uspec1 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(model = "apARCH"), 
                      distribution.model = "norm")
  uspec2 = ugarchspec(mean.model = list(armaOrder = c(2,0)), variance.model = list(model = "gjrGARCH"), 
                      distribution.model = "norm")
  uspec3 = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(model = "sGARCH"), 
                      distribution.model = "norm")
  uspec4 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(model = "sGARCH",
                                                                                   garchOrder = c(2, 1)), 
                      distribution.model = "norm")
  uspec5 = ugarchspec(mean.model = list(armaOrder = c(1,1)), variance.model = list(model = "eGARCH",
                                                                                   garchOrder = c(1, 1)), 
                      distribution.model = "norm")
  
  uspec = c( uspec1, uspec2, uspec3, uspec4, uspec5 )
  spec = dccspec(uspec = multispec( uspec ), dccOrder = c(1,1), distribution = "mvlaplace")
  
  roll = dccroll(spec, data = Dat, n.ahead = 1, forecast.length = 100, refit.every = 24, 
                 refit.window = "moving", solver = "solnp", 
                 fit.control = list(eval.se = FALSE), solver.control = list(), 
                 cluster = cluster, 
                 save.fit = FALSE, save.wdir = NULL)
  
  options(width = 120)
  zz <- file("test2g1.txt", open="wt")
  sink(zz)
  show(roll)
  cat("\nAverage Log-Likelihood Across Rolls:\n")
  print( round( likelihood(roll)/sapply(roll@model$rollind, FUN = function(x) length(x)), 2) )
  sink(type="message")
  sink()
  close(zz)
  
  postscript("test2g1.eps", width = 12, height = 12)
  plot(roll, which = 4, series=c(1,2,3,4,5))
  dev.off()
  
  postscript("test2g2.eps", width = 12, height = 12)
  plot(roll, which = 5)
  dev.off()
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}

# simulation tests
rmgarch.test2h = function(cluster = NULL)
{
  tic = Sys.time()
  data(dji30ret)
  
  # Normal
  Dat = dji30ret[, 1:5, drop = FALSE]
  uspec1 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(model = "sGARCH"), 
                      distribution.model = "norm")
  uspec2 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(model = "sGARCH"), 
                      distribution.model = "norm")
  uspec3 = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(model = "sGARCH"), 
                      distribution.model = "norm")
  uspec4 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(model = "sGARCH",
                                                                                   garchOrder = c(1, 1)), 
                      distribution.model = "norm")
  uspec5 = ugarchspec(mean.model = list(armaOrder = c(1,1)), variance.model = list(model = "sGARCH",
                                                                                   garchOrder = c(1, 1)), 
                      distribution.model = "norm")
  
  uspec = c( uspec1, uspec2, uspec3, uspec4, uspec5 )
  spec1 = dccspec(uspec = multispec( uspec ), dccOrder = c(1,1), distribution = "mvnorm")
  
  fit1 = dccfit(spec1, data = Dat, solver = "solnp", fit.control = list(eval.se=FALSE), cluster = NULL)
  
  forc = dccforecast(fit1, n.ahead = 1)
  
  sim1 = dccsim(fit1, n.sim = 1, m.sim = 1000, startMethod = "sample", cluster = cluster, rseed = 1:1000)
  
  uspec1 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(model = "sGARCH"), 
                      distribution.model = "ged", fixed.pars=list(shape=1))
  uspec2 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(model = "sGARCH"), 
                      distribution.model = "ged", fixed.pars=list(shape=1))
  uspec3 = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(model = "sGARCH"), 
                      distribution.model = "ged", fixed.pars=list(shape=1))
  uspec4 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(model = "sGARCH",
                                                                                   garchOrder = c(1, 1)), 
                      distribution.model = "ged", fixed.pars=list(shape=1))
  uspec5 = ugarchspec(mean.model = list(armaOrder = c(1,1)), variance.model = list(model = "sGARCH",
                                                                                   garchOrder = c(1, 1)), 
                      distribution.model = "ged", fixed.pars=list(shape=1))
  spec2 = dccspec(uspec = multispec( uspec ), dccOrder = c(1,1), distribution = "mvlaplace")
  
  fit2 = dccfit(spec2, data = Dat, solver = "solnp", fit.control = list(eval.se=FALSE), cluster = cluster)
  
  sim2 = dccsim(fit2, n.sim = 1, m.sim = 1000, startMethod = "sample", cluster = cluster, rseed = 1:1000)
  
  # gauge the uncertainty around 1-ahead
  forcM = fitted(forc)[,,1]
  
  sim1M = t(sapply(sim1@msim$simX, FUN = function(x) x))
  # equivalent: t(sapply(1:1000, FUN = function(i) fitted(sim1, sim=i)))
  sim2M = t(sapply(sim2@msim$simX, FUN = function(x) x))
  
  cnames = fit1@model$modeldata$asset.names
  mvnH = apply(sim1M, 2, FUN = function(x) hist(x, breaks="fd", plot=FALSE))
  mvlH = apply(sim2M, 2, FUN = function(x) hist(x, breaks="fd", plot=FALSE))
  minX = maxX = rep(0, 5)
  maxY = rep(0, 5)
  for(i in 1:5){
    maxX[i] = max(c(mvnH[[i]]$breaks, mvlH[[i]]$breaks))
    minX[i] = min(c(mvnH[[i]]$breaks, mvlH[[i]]$breaks))
    maxY[i] = max(c(mvnH[[i]]$counts, mvlH[[i]]$counts))
  }
  
  postscript("test2h1.eps", width = 16, height = 14)
  par(mfrow = c(2,3))
  for(i in 1:5){
    plot(mvnH[[i]], xlim = c(minX[i], maxX[i]), ylim = c(0, maxY[i]), border = "steelblue", xlab = "", main = cnames[i])
    plot(mvlH[[i]], border = "tomato", add = TRUE)
    abline(v = forcM[i], col = "black", lwd=1)
    legend("topleft", c("MVN", "MVL"), col = c("steelblue", "tomato"), bty="n", lty=c(1,1))
    legend("topright", c("Forecast"), col = c("black"), bty="n", lty=1)
  }
  dev.off()
  
  # one more with Student (QML first stage)
  uspec1 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(model = "sGARCH"), 
                      distribution.model = "norm")
  uspec2 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(model = "sGARCH"), 
                      distribution.model = "norm")
  uspec3 = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(model = "sGARCH"), 
                      distribution.model = "norm")
  uspec4 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(model = "sGARCH",
                                                                                   garchOrder = c(1, 1)), 
                      distribution.model = "norm")
  uspec5 = ugarchspec(mean.model = list(armaOrder = c(1,1)), variance.model = list(model = "sGARCH",
                                                                                   garchOrder = c(1, 1)), 
                      distribution.model = "norm")
  spec3 = dccspec(uspec = multispec( uspec ), dccOrder = c(1,1), distribution = "mvt")
  fit3 = dccfit(spec3, data = Dat, solver = "solnp", fit.control = list(eval.se=FALSE), cluster = cluster)
  
  # now go back and fix the shape
  shp = unname(coef(fit3, "dcc")["mshape"])
  uspec1 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(model = "sGARCH"), 
                      distribution.model = "std", fixed.pars = list(shape=shp))
  uspec2 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(model = "sGARCH"), 
                      distribution.model = "std", fixed.pars = list(shape=shp))
  uspec3 = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(model = "sGARCH"), 
                      distribution.model = "std", fixed.pars = list(shape=shp))
  uspec4 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(model = "sGARCH",
                                                                                   garchOrder = c(1, 1)), 
                      distribution.model = "std", fixed.pars = list(shape=shp))
  uspec5 = ugarchspec(mean.model = list(armaOrder = c(1,1)), variance.model = list(model = "sGARCH",
                                                                                   garchOrder = c(1, 1)), 
                      distribution.model = "std", fixed.pars = list(shape=shp))
  spec3 = dccspec(uspec = multispec( uspec ), dccOrder = c(1,1), distribution = "mvt", fixed.pars=list(mshape=shp))
  fit3 = dccfit(spec3, data = Dat, solver = "solnp", fit.control = list(eval.se=FALSE), cluster = cluster)
  
  sim3 = dccsim(fit3, n.sim = 1, m.sim = 1000, startMethod = "sample", cluster = cluster, rseed = 1:1000)
  
  sim3M = t(sapply(sim3@msim$simX, FUN = function(x) x))
  
  mvtH = apply(sim3M, 2, FUN = function(x) hist(x, breaks="fd", plot=FALSE))
  minX = maxX = rep(0, 5)
  maxY = rep(0, 5)
  for(i in 1:5){
    maxX[i] = max(c(mvnH[[i]]$breaks, mvtH[[i]]$breaks))
    minX[i] = min(c(mvnH[[i]]$breaks, mvtH[[i]]$breaks))
    maxY[i] = max(c(mvnH[[i]]$counts, mvtH[[i]]$counts))
  }
  
  postscript("test2h2.eps", width = 16, height = 14)
  par(mfrow = c(2,3))
  for(i in 1:5){
    plot(mvtH[[i]], xlim = c(minX[i], maxX[i]), ylim = c(0, maxY[i]), border = "steelblue", xlab = "", main = cnames[i])
    plot(mvnH[[i]], border = "tomato", add = TRUE)
    abline(v = forcM[i], col = "black", lwd=1)
    legend("topleft", c("MVT", "MVN"), col = c("steelblue", "tomato"), bty="n", lty=c(1,1))
    legend("topright", c("Forecast"), col = c("black"), bty="n", lty=1)
  }
  dev.off()	
}
