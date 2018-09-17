## source files' folder
## https://bitbucket.org/alexiosg/rmgarch/src/ce811acd0c7b/inst/rmgarch.tests/?at=beta

## source file
## https://bitbucket.org/alexiosg/rmgarch/src/ce811acd0c7bdda4a61dda2aba4d63c506987448/inst/rmgarch.tests/rmgarch.test5.R?at=beta&fileviewer=file-view-default

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

# FDCC Model

rmgarch.test5a = function(cluster = NULL)
{
  # DCC under different specifications
  tic = Sys.time()
  data(dji30retw)
  Dat = dji30retw[, 1:3, drop = FALSE]
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), variance.model = list(garchOrder = c(3,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec1  = dccspec(uspec = multispec( replicate(3, uspec) ), dccOrder = c(1,1), 
                   model = "DCC", distribution = "mvnorm")
  fspec1 = dccspec(uspec = multispec( replicate(3, uspec) ), dccOrder = c(1,1), 
                   model = "FDCC", groups = c(1,2,3), distribution = "mvnorm")
  
  fit1 = dccfit(spec1, data = Dat, fit.control = list(eval.se=FALSE), solver.control=list(trace=1))
  ffit1 = dccfit(fspec1, data = Dat, fit.control = list(eval.se=FALSE), solver.control=list(trace=1))
  
  specx1 = ugarchspec(mean.model = list(armaOrder = c(2,1)), variance.model = list(garchOrder = c(3,1), model = "sGARCH"), 
                      distribution.model = "norm")
  specx2 = ugarchspec(mean.model = list(armaOrder = c(0,1)), variance.model = list(garchOrder = c(1,1), model = "eGARCH"), 
                      distribution.model = "norm")
  specx3 = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1), model = "apARCH"), 
                      distribution.model = "norm")
  spec2 = dccspec(uspec = multispec( list(specx1, specx2, specx3) ), dccOrder = c(1,1), distribution = "mvnorm")
  fspec2 = dccspec(uspec = multispec( list(specx1, specx2, specx3) ), dccOrder = c(1,1), 
                   model = "FDCC", groups = c(1,2,3), distribution = "mvnorm")
  
  fit2 = dccfit(spec2, data = Dat, fit.control = list(eval.se=FALSE))
  ffit2 = dccfit(fspec2, data = Dat, fit.control = list(eval.se=FALSE))
  
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0),  include.mean = FALSE), 
                     variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec3 = dccspec(uspec = multispec( replicate(3, uspec) ), VAR = TRUE, 
                  dccOrder = c(1,1), distribution = "mvnorm")
  fspec3 = dccspec(uspec = multispec( replicate(3, uspec) ), VAR = TRUE, 
                   dccOrder = c(1,1), model = "FDCC", groups = c(1,2,3), 
                   distribution = "mvnorm")
  
  fit3 = dccfit(spec3, data = Dat, fit.control = list(eval.se=FALSE))
  ffit3 = dccfit(fspec3, data = Dat, fit.control = list(eval.se=FALSE))
  
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0),  include.mean = FALSE), 
                     variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec4 = dccspec(uspec = multispec( replicate(3, uspec) ), VAR = TRUE, 
                  lag = 3, dccOrder = c(1,1), distribution = "mvnorm")
  fspec4 = dccspec(uspec = multispec( replicate(3, uspec) ), VAR = TRUE, 
                   lag = 3, dccOrder = c(1,1), model = "FDCC", groups = c(1,2,3), 
                   distribution = "mvnorm")
  fit4 = dccfit(spec4, data = Dat, fit.control = list(eval.se=FALSE))
  ffit4 = dccfit(fspec4, data = Dat, fit.control = list(eval.se=FALSE))	
  
  np = c(
    length(fit1@mfit$matcoef[,1]),
    length(ffit1@mfit$matcoef[,1]),
    length(fit2@mfit$matcoef[,1]),
    length(ffit2@mfit$matcoef[,1]),
    length(fit3@mfit$matcoef[,1]),
    length(ffit3@mfit$matcoef[,1]),
    length(fit4@mfit$matcoef[,1]),
    length(ffit4@mfit$matcoef[,1])) +  (3^2 - 3)/2
  # (3^2-2)/2 is the calculated unconditional matrix
  aicmod = c(
    rugarch:::.information.test(fit1@mfit$llh,  nObs = fit1@model$modeldata$T, 
                                nPars = np[1])[[1]],
    rugarch:::.information.test(ffit1@mfit$llh, nObs = ffit1@model$modeldata$T, 
                                nPars = np[2])[[1]],
    rugarch:::.information.test(fit2@mfit$llh,  nObs = fit2@model$modeldata$T, 
                                nPars = np[3])[[1]],
    rugarch:::.information.test(ffit2@mfit$llh, nObs = ffit2@model$modeldata$T, 
                                nPars = np[4])[[1]],
    rugarch:::.information.test(fit3@mfit$llh,  nObs = fit3@model$modeldata$T, 
                                nPars = np[5])[[1]],
    rugarch:::.information.test(ffit3@mfit$llh, nObs = ffit3@model$modeldata$T, 
                                nPars = np[6])[[1]],
    rugarch:::.information.test(fit4@mfit$llh,  nObs = fit4@model$modeldata$T, 
                                nPars = np[7])[[1]],
    rugarch:::.information.test(ffit4@mfit$llh, nObs = ffit4@model$modeldata$T, 
                                nPars = np[8])[[1]])
  tmp = data.frame(n.pars = np, AIC = aicmod)
  rownames(tmp) = paste("Model_", 1:8, sep = "")
  
  options(width = 120)
  zz <- file("test5a.txt", open="wt")
  sink(zz)	
  print(tmp)
  sink(type="message")
  sink()
  close(zz)
  
  rc1 = rcor(fit1)
  frc1 = rcor(ffit1)
  rc2 = rcor(fit2)
  frc2 = rcor(ffit2)
  rc3 = rcor(fit3)
  frc3 = rcor(ffit3)
  rc4 = rcor(fit4)
  frc4 = rcor(ffit4)
  
  postscript("test5a.eps", width = 10, height = 8)
  par(mfrow=c(2,2))
  plot(rc1[1,2, ], type = "l")
  lines(frc1[1,2, ], col = 2)
  legend("topleft", legend = c("DCC", "FDCC"), col= 1:2, fill = 1:2)
  plot(rc2[2,3, ], type = "l")
  lines(frc2[2,3, ], col = 2)
  legend("topleft", legend = c("DCC", "FDCC"), col= 1:2, fill = 1:2)
  plot(rc3[1,3, ], type = "l")
  lines(frc2[1,3, ], col = 2)
  legend("topleft", legend = c("DCC", "FDCC"), col= 1:2, fill = 1:2)
  plot(rc4[1,2, ], type = "l")
  lines(frc4[1,2, ], col = 2)
  legend("topleft", legend = c("DCC", "FDCC"), col= 1:2, fill = 1:2)
  dev.off()
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}



# Filter Tests
rmgarch.test5b = function(cluster = NULL)
{
  tic = Sys.time()
  
  data(dji30retw)
  Dat = dji30retw[, 1:3, drop = FALSE]
  uspec = ugarchspec(mean.model = list(armaOrder = c(1,2)), 
                     variance.model = list(garchOrder = c(1,1), model = "eGARCH"), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(3, uspec) ),  
                  dccOrder = c(1,1),  model = "FDCC", groups = c(1,2,2), 
                  distribution = "mvnorm")
  fit1 = dccfit(spec1, data = Dat, fit.control = list(eval.se=TRUE))
  
  vspec = vector(mode = "list", length = 3)
  midx = fit1@model$midx
  mpars = fit1@model$mpars
  for(i in 1:3){
    vspec[[i]] = uspec
    setfixed(vspec[[i]])<-as.list(mpars[midx[,i]==1, i])
  }
  dccfix = as.list(coef(fit1, type="dcc"))
  spec2 = dccspec(uspec = multispec( vspec ),  
                  dccOrder = c(1,1),  model = "FDCC", groups =c(1,2,2), 
                  distribution = "mvnorm", fixed.pars = dccfix)
  filt1 = dccfilter(spec2, data = Dat)
  
  rc1 = rcor(fit1)
  rc2 = rcor(filt1)
  
  options(width = 120)
  zz <- file("test5b1.txt", open="wt")
  sink(zz)
  print( all.equal(first(rc1)[,,1], first(rc2)[,,1] ) )
  print( all.equal(last(rc1)[,,1], last(rc2)[,,1] ) )
  print( all.equal(first(rc1, 10), first(rc2, 10) ) )
  print( all.equal(head(fitted(fit1)), head(fitted(filt1))) ) 
  print( all.equal(head(residuals(fit1)), head(residuals(filt1)) )) 
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
                  dccOrder = c(1,1),  model = "FDCC", groups = c(1,2,3), 
                  distribution = "mvnorm")
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
  spec2 = dccspec(uspec = multispec( vspec ), dccOrder = c(1,1), model = "FDCC", 
                  groups = c(1,2,3), distribution = "mvnorm", fixed.pars = dccfix)
  filt1 = dccfilter(spec2, data = Dat[1:(T+5), ], filter.control = list(n.old = T))
  
  
  rc1 = rcor(fit1)
  rc2 = rcor(filt1)
  options(width = 120)
  zz <- file("test5b2.txt", open="wt")
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
                  dccOrder = c(1,1),  model = "FDCC", groups = c(1,1,1), 
                  VAR = TRUE, lag = 2, distribution = "mvnorm")
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
                  dccOrder = c(1,1),  model = "FDCC", groups = c(1,1,1), VAR = TRUE, 
                  lag = 2, distribution = "mvnorm",fixed.pars = dccfix)
  filt1 = dccfilter(spec2, data = Dat[1:(T+5), ], filter.control = list(n.old = T), varcoef = VAR.fit$Bcoef)
  
  
  rc1 = rcor(fit1)
  rc2 = rcor(filt1)
  options(width = 120)
  zz <- file("test5b3.txt", open="wt")
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


# Forecast Tests 
rmgarch.test5c = function(cluster = NULL)
{
  tic = Sys.time()
  
  data(dji30retw)
  Dat = dji30retw[, 1:3, drop = FALSE]
  
  #---------------------------------------------------------------------------------------------------------------
  # ARMA-GARCH-DCC
  cnames = colnames(Dat)
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), variance.model = list(garchOrder = c(1,1), model = "gjrGARCH"), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(3, uspec) ), dccOrder = c(1,1), model="FDCC",
                  groups = c(1,2,3), distribution = "mvnorm")
  fit1 = dccfit(spec1, data = Dat, out.sample = 100, fit.control = list(eval.se=FALSE))
  T = dim(Dat)[1]-100
  
  forc = dccforecast(fit1, n.ahead = 1, n.roll = 10)
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
  spec2 = dccspec(uspec = multispec( vspec ), dccOrder = c(1,1),  model="FDCC",
                  groups = c(1,2,3), distribution = "mvnorm", fixed.pars = dccfix)
  
  # 1-step ahead forecast on out-sample data is equivalent to filtering
  # Note the use of n.old so that filtering assumptions are observed based on
  # the size of the original dataset
  filt1 = dccfilter(spec2, data = Dat[1:(T+100),], filter.control = list(n.old = T))
  
  # check mean and sigma
  options(width = 120)
  zz <- file("test5c1.txt", open="wt")
  sink(zz)
  # REM Forecast Headings are for T+0 (i.e. the time the forecast was made) and NOT
  # the forecast date (T+i)
  print(all.equal(fitted(forc)[,,1], filt1@model$mu[T+1,], fitted(filt1)[T+1,], check.attributes=FALSE))
  print(all.equal(sigma(forc)[,,1], filt1@model$sigma[T+1,], sigma(filt1)[T+1,], check.attributes=FALSE))
  print(all.equal(fitted(forc)[,,11], filt1@model$mu[T+11,], fitted(filt1)[T+11,], check.attributes=FALSE))
  print(all.equal(rcor(forc)[[1]][,,1], rcor(filt1)[,,T+1], check.attributes=FALSE))
  print(all.equal(rcor(forc)[[11]][,,1], rcor(filt1)[,,T+11], check.attributes=FALSE))
  print(all.equal(sigma(forc)[,,11], filt1@model$sigma[T+11,], sigma(filt1)[T+11,], check.attributes=FALSE))
  sink(type="message")
  sink()
  close(zz)
  
  
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
  spec1 = dccspec(uspec = multispec( replicate(3, uspec) ), VAR = TRUE, lag = 2, 
                  dccOrder = c(1,1), model="FDCC", groups = c(1,1,2), distribution = "mvnorm")
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
                  dccOrder = c(1,1),  model="FDCC", groups = c(1,1,2), 
                  distribution = "mvnorm", fixed.pars = dccfix)
  
  filt1 = dccfilter(spec2, data = Dat[1:(T+100),], filter.control = list(n.old = T), varcoef = VAR.filt$Bcoef)
  
  # check mean and sigma
  options(width = 120)
  zz <- file("test5c2.txt", open="wt")
  sink(zz)
  print(all.equal(fitted(forc)[,,1], as.numeric(filt1@model$mu[T+1,]), check.attributes=FALSE))
  print(all.equal(fitted(forc)[,,1], as.numeric(fitted(filt1)[T+1,]), check.attributes=FALSE))
  
  print(all.equal(sigma(forc)[,,1], filt1@model$sigma[T+1,], check.attributes=FALSE))
  print(all.equal(sigma(forc)[,,1], as.numeric(sigma(filt1)[T+1,]), check.attributes=FALSE))
  print(all.equal(fitted(forc)[,,11], as.numeric(filt1@model$mu[T+11,]), check.attributes=FALSE))
  print(all.equal(fitted(forc)[,,11], as.numeric(fitted(filt1)[T+11,]), check.attributes=FALSE))
  print(all.equal(sigma(forc)[,,11], filt1@model$sigma[T+11,], check.attributes=FALSE))
  print(all.equal(sigma(forc)[,,11], as.numeric(sigma(filt1)[T+11,]), check.attributes=FALSE))
  print(all.equal(rcor(forc)[[1]][,,1], rcor(filt1)[,,T+1], check.attributes=FALSE))
  # the difference is explained in the note above
  print(all.equal(rcor(forc)[[11]][,,1], rcor(filt1)[,,T+11], check.attributes=FALSE))
  sink(type="message")
  sink()
  close(zz)
  
  data(dji30ret)
  Dat = dji30ret[,1:10]
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                     variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec1 = dccspec(uspec = multispec( replicate(10, uspec) ), VAR = TRUE, lag = 1, 
                  dccOrder = c(1,1),  model="FDCC", groups = c(1,1,1,2,2,2,3,3,4,4), 
                  distribution = "mvnorm")
  fit1 = dccfit(spec1, data = Dat, out.sample = 100, solver = "solnp", 
                fit.control = list(eval.se=FALSE))	
  
  forc1 = dccforecast(fit1, n.ahead = 1500, n.roll = 0)	
  forc2 = dccforecast(fit1, n.ahead = 1, n.roll = 99)
  
  
  # forecast R --> unconditional R (Rbar) as N --> large
  tmp = rmgarch:::getfdccpars(fit1@model$ipars, fit1@model)
  A = tmp$A %*% t(tmp$A)
  B = tmp$B %*% t(tmp$B)
  C = matrix(1, NROW(B), NCOL(B)) - A - B
  
  Qbar = fit1@mfit$Qbar
  Rbar = cov2cor( C * Qbar )
  
  options(width = 120)
  zz <- file("test5c4.txt", open="wt")
  sink(zz)
  all.equal(Rbar, forc1@mforecast$R[[1]][,,1500])
  print(Rbar - forc1@mforecast$R[[1]][,,1500])
  sink(type="message")
  sink()
  close(zz)
  
  postscript("test5c4.eps", width = 10, height = 8)
  par(mfrow = c(3,3))
  plot(forc1@mforecast$R[[1]][1,2,], type = "l")
  abline(h = Rbar[1,2], col = 2)
  plot(forc1@mforecast$R[[1]][6,8,], type = "l")
  abline(h = Rbar[6,8], col = 2)
  plot(forc1@mforecast$R[[1]][4,5,], type = "l")
  abline(h = Rbar[4,5], col = 2)
  plot(forc1@mforecast$R[[1]][9,10,], type = "l")
  abline(h = Rbar[9,10], col = 2)		
  plot(forc1@mforecast$R[[1]][2,3,], type = "l")
  abline(h = Rbar[2,3], col = 2)
  plot(forc1@mforecast$R[[1]][1,8,], type = "l")
  abline(h = Rbar[1,8], col = 2)
  plot(forc1@mforecast$R[[1]][4,5,], type = "l")
  abline(h = Rbar[4,5], col = 2)
  plot(forc1@mforecast$R[[1]][6,7,], type = "l")
  abline(h = Rbar[6,7], col = 2)
  dev.off()
  #############################################
  
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}


# simulation tests
rmgarch.test5d = function(cluster = NULL)
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
  spec1 = dccspec(uspec = multispec( uspec ), dccOrder = c(1,1), model = "FDCC", 
                  groups = c(1,2,3,4,5), distribution = "mvnorm")
  
  fit1 = dccfit(spec1, data = Dat, solver = "solnp", fit.control = list(eval.se=FALSE), cluster = cluster)
  
  spec2 = dccspec(uspec = multispec( uspec ), dccOrder = c(1,1), distribution = "mvnorm")
  fit2 = dccfit(spec2, data = Dat, solver = "solnp", fit.control = list(eval.se=FALSE), cluster = cluster)
  
  forc = dccforecast(fit1, n.ahead = 1)
  
  sim1 = dccsim(fit1, n.sim = 1, m.sim = 1000, startMethod = "sample", cluster = cluster, rseed = 1:1000)
  
  
  # gauge the uncertainty around 1-ahead
  forcM = fitted(forc)[,,1]
  sim1M = t(sapply(sim1@msim$simX, FUN = function(x) x))
  # equivalent: t(sapply(1:1000, FUN = function(i) fitted(sim1, sim=i)))	
  cnames = fit1@model$modeldata$asset.names
  mvnH = apply(sim1M, 2, FUN = function(x) hist(x, breaks="fd", plot=FALSE))
  minX = maxX = rep(0, 5)
  maxY = rep(0, 5)
  for(i in 1:5){
    maxX[i] = max(c(mvnH[[i]]$breaks))
    minX[i] = min(c(mvnH[[i]]$breaks))
    maxY[i] = max(c(mvnH[[i]]$counts))
  }
  
  postscript("test5d1.eps", width = 16, height = 14)
  par(mfrow = c(2,3))
  for(i in 1:5){
    plot(mvnH[[i]], xlim = c(minX[i], maxX[i]), ylim = c(0, maxY[i]), border = "steelblue", xlab = "", main = cnames[i])
    abline(v = forcM[i], col = "black", lwd=1)
    legend("topleft", c("MVN"), col = c("steelblue"), bty="n", lty=c(1,1))
    legend("topright", c("Forecast"), col = c("black"), bty="n", lty=1)
  }
  dev.off()
  
  sim1 = dccsim(fit1, n.sim = 2500, m.sim = 1, startMethod = "sample", cluster = cluster, rseed = 10)
  sim2 = dccsim(fit2, n.sim = 2500, m.sim = 1, startMethod = "sample", cluster = cluster, rseed = 10)
  
  
  R1 = rcor(sim1)
  R2 = rcor(sim2)
  postscript("test5d2.eps", width = 16, height = 14)
  par(mfrow = c(2,3))
  Rx1 = apply(R1, 3, function(x) x[1,2])
  Rx2 = apply(R2, 3, function(x) x[1,2])
  maxy = max(c(Rx1, Rx2))
  miny = min(c(Rx1, Rx2))
  plot( Rx1, main = paste(cnames[1],"-",cnames[2],sep=""), type="l", col = "steelblue", ylim=c(miny, maxy), ylab="Corr",
        xlab = "T[Sim]")
  lines(Rx2, main = paste(cnames[1],"-",cnames[2],sep=""), col = "tomato1")
  legend("topleft", c("FDCC", "DCC"), col = c("steelblue", "tomato1"), lty=c(1,1), bty="n")
  
  Rx1 = apply(R1, 3, function(x) x[1,3])
  Rx2 = apply(R2, 3, function(x) x[1,3])
  maxy = max(c(Rx1, Rx2))
  miny = min(c(Rx1, Rx2))
  plot( Rx1, main = paste(cnames[1],"-",cnames[3],sep=""), type="l", col = "steelblue", ylim=c(miny, maxy), ylab="Corr",
        xlab = "T[Sim]")
  lines(Rx2, main = paste(cnames[1],"-",cnames[3],sep=""), col = "tomato1")
  legend("topleft", c("FDCC", "DCC"), col = c("steelblue", "tomato1"), lty=c(1,1), bty="n")
  
  Rx1 = apply(R1, 3, function(x) x[2,4])
  Rx2 = apply(R2, 3, function(x) x[2,4])
  maxy = max(c(Rx1, Rx2))
  miny = min(c(Rx1, Rx2))
  plot( Rx1, main = paste(cnames[2],"-",cnames[4],sep=""), type="l", col = "steelblue", ylim=c(miny, maxy), ylab="Corr",
        xlab = "T[Sim]")
  lines(Rx2, main = paste(cnames[2],"-",cnames[4],sep=""), col = "tomato1")
  legend("topleft", c("FDCC", "DCC"), col = c("steelblue", "tomato1"), lty=c(1,1), bty="n")
  
  Rx1 = apply(R1, 3, function(x) x[3,4])
  Rx2 = apply(R2, 3, function(x) x[3,4])
  maxy = max(c(Rx1, Rx2))
  miny = min(c(Rx1, Rx2))
  plot( Rx1, main = paste(cnames[3],"-",cnames[4],sep=""), type="l", col = "steelblue", ylim=c(miny, maxy), ylab="Corr",
        xlab = "T[Sim]")
  lines(Rx2, main = paste(cnames[3],"-",cnames[4],sep=""), col = "tomato1")
  legend("topleft", c("FDCC", "DCC"), col = c("steelblue", "tomato1"), lty=c(1,1), bty="n")
  
  Rx1 = apply(R1, 3, function(x) x[4,5])
  Rx2 = apply(R2, 3, function(x) x[4,5])
  maxy = max(c(Rx1, Rx2))
  miny = min(c(Rx1, Rx2))
  plot( Rx1, main = paste(cnames[4],"-",cnames[5],sep=""), type="l", col = "steelblue", ylim=c(miny, maxy), ylab="Corr",
        xlab = "T[Sim]")
  lines(Rx2, main = paste(cnames[4],"-",cnames[5],sep=""), col = "tomato1")
  legend("topleft", c("FDCC", "DCC"), col = c("steelblue", "tomato1"), lty=c(1,1), bty="n")
  
  Rx1 = apply(R1, 3, function(x) x[1,5])
  Rx2 = apply(R2, 3, function(x) x[1,5])
  maxy = max(c(Rx1, Rx2))
  miny = min(c(Rx1, Rx2))
  plot( Rx1, main = paste(cnames[1],"-",cnames[5],sep=""), type="l", col = "steelblue", ylim=c(miny, maxy), ylab="Corr",
        xlab = "T[Sim]")
  lines(Rx2, main = paste(cnames[1],"-",cnames[5],sep=""), col = "tomato1")
  legend("topleft", c("FDCC", "DCC"), col = c("steelblue", "tomato1"), lty=c(1,1), bty="n")
  dev.off()
}
