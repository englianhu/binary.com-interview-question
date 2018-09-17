## source files' folder
## https://bitbucket.org/alexiosg/rmgarch/src/ce811acd0c7b/inst/rmgarch.tests/?at=beta

## source file
## https://bitbucket.org/alexiosg/rmgarch/src/ce811acd0c7bdda4a61dda2aba4d63c506987448/inst/rmgarch.tests/rmgarch.test3.R?at=beta&fileviewer=file-view-default

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
# Copula GARCH model
#################################################################################
# Fit Tests & Filter

## DCC Copulas
rmgarch.test3a = function(cluster = NULL)
{
  tic = Sys.time()
  data(dji30retw)
  Dat = dji30retw[, 1:3, drop = FALSE]
  
  # DCC timecopula MVN (check against DCC-NORM)--> They should be the same
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), 
                     variance.model = list(garchOrder = c(1,1), model = "sGARCH", variance.targeting=FALSE), 
                     distribution.model = "norm")
  spec1 = cgarchspec(uspec = multispec( replicate(3, uspec) ), asymmetric = TRUE, 
                     distribution.model = list(copula = "mvnorm", method = "Kendall", 
                                               time.varying = TRUE, transformation = "parametric"))
  
  fit1 = cgarchfit(spec1, data = Dat, cluster = cluster, solver.control=list(trace=1))
  
  # Create and Check the Filter method
  specx1 = spec1
  for(i in 1:3) specx1@umodel$fixed.pars[[i]] = as.list(fit1@model$mpars[fit1@model$midx[,i]==1,i])
  setfixed(specx1)<-as.list(fit1@model$mpars[fit1@model$midx[,4]==1,4])
  filt1 = cgarchfilter(specx1, data = Dat, cluster = cluster)
  
  options(width = 120)
  zz <- file("test3a1.txt", open="wt")
  sink(zz)
  print(all.equal(last(rcov(fit1))[,,1], last(rcov(filt1))[,,1]))
  print(all.equal(first(rcov(fit1))[,,1], first(rcov(filt1))[,,1]))
  print(all.equal(head(fitted(fit1)), head(fitted(filt1))))
  print(all.equal(head(sigma(fit1)), head(sigma(filt1))))
  sink(type="message")
  sink()
  close(zz)
  
  
  spec2 = dccspec(uspec = multispec( replicate(3, uspec) ), dccOrder = c(1,1), VAR = FALSE, 
                  model = "aDCC", distribution = "mvnorm")
  fit2 = dccfit(spec2, data = Dat, cluster = cluster)
  
  options(width = 120)
  zz <- file("test3a2.txt", open="wt")
  sink(zz)
  print(data.frame(aDCCcopula = round(coef(fit1), 3), aDCC = round(coef(fit2), 3)))
  print(data.frame(aDCCcopula.se = round(fit1@mfit$matcoef[,2], 4), aDCC.se = round(fit2@mfit$matcoef[,2], 4)))
  print(data.frame(aDCCcopula.LL = likelihood(fit1), aDCC.LL = likelihood(fit2)))
  sink(type="message")
  sink()
  close(zz)
  
  # DCC timecopula MVT (check against DCC-Student)--> They should be the same
  uspec3 = ugarchspec(mean.model = list(armaOrder = c(2,1)), 
                      variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                      distribution.model = "std", fixed.pars = list(shape = 6))
  
  spec3 = cgarchspec(uspec = multispec( replicate(3, uspec3) ), VAR = FALSE, 
                     robust = FALSE, lag = 1, lag.max = NULL, 
                     lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL,
                     robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
                     dccOrder = c(1,1), asymmetric = TRUE, 
                     distribution.model = list(copula = c("mvnorm", "mvt")[2], 
                                               method = c("Kendall", "ML")[1], time.varying = TRUE, 
                                               transformation = c("parametric", "empirical", "spd")[1]),
                     start.pars = list(), fixed.pars = list(mshape = 6))
  fit3 = cgarchfit(spec3, data = Dat, cluster = cluster, solver.control=list(trace=1))
  
  # Create and Check the Filter method
  specx3 = spec3
  for(i in 1:3) specx3@umodel$fixed.pars[[i]] = as.list(fit3@model$mpars[fit3@model$midx[,i]==1,i])
  setfixed(specx3)<-as.list(fit3@model$mpars[fit3@model$midx[,4]==1,4])
  filt3 = cgarchfilter(specx3, data = Dat, cluster = cluster)
  
  options(width = 120)
  zz <- file("test3a3.txt", open="wt")
  sink(zz)
  print(all.equal(last(rcov(fit3))[,,1], last(rcov(filt3))[,,1]))
  print(all.equal(first(rcov(fit3))[,,1], first(rcov(filt3))[,,1]))
  print(all.equal(head(fitted(fit3)), head(fitted(filt3))))
  print(all.equal(head(sigma(fit3)), head(sigma(filt3))))
  sink(type="message")
  sink()
  close(zz)
  
  spec4 = dccspec(uspec = multispec( replicate(3, uspec3) ), dccOrder = c(1,1), 
                  model = "aDCC", distribution = "mvt", fixed.pars = list(mshape = 6))
  fit4 = dccfit(spec4, data = Dat, cluster = cluster)
  
  options(width = 120)
  zz <- file("test3a4.txt", open="wt")
  sink(zz)
  print(data.frame(aDCCcopula = round(coef(fit3), 3), aDCC = round(coef(fit4), 3)))
  print(data.frame(aDCCcopula.se = round(fit3@mfit$matcoef[,2], 4), aDCC.se = round(fit4@mfit$matcoef[,2], 4)))
  print(data.frame(aDCCcopula.p = round(fit3@mfit$matcoef[,4], 3), aDCC.p = round(fit4@mfit$matcoef[,4], 3)))
  print(data.frame(aDCCcopula.LL = likelihood(fit3), aDCC.LL = likelihood(fit4)))
  sink(type="message")
  sink()
  close(zz)
  
  postscript("test3a-1.eps")
  plot(rcor(fit4)[1,2,], type="l")
  lines(rcor(fit3)[1,2,], col = 2, lty=4)
  dev.off()
  
  # Some Alternative Parametrizations
  uspec4 = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                      variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                      distribution.model = "jsu")
  
  spec4 = cgarchspec(uspec = multispec( replicate(3, uspec4) ), VAR = TRUE, 
                     robust = FALSE, lag = 1, lag.max = NULL, 
                     dccOrder = c(1,1), asymmetric = FALSE, 
                     distribution.model = list(copula = c("mvnorm", "mvt")[2], 
                                               time.varying = TRUE, 
                                               transformation = c("parametric", "empirical", "spd")[2]))
  
  fit4 = cgarchfit(spec4, data = Dat, cluster = cluster)
  
  
  # Create and Check the Filter method
  specx4 = spec4
  for(i in 1:3) specx4@umodel$fixed.pars[[i]] = as.list(fit4@model$mpars[fit4@model$midx[,i]==1,i])
  setfixed(specx4)<-as.list(fit4@model$mpars[fit4@model$midx[,4]==1,4])
  filt4 = cgarchfilter(specx4, data = Dat, cluster = cluster,
                       varcoef = fit4@model$varcoef)
  
  options(width = 120)
  zz <- file("test3a5.txt", open="wt")
  sink(zz)
  print(all.equal(last(rcov(fit4))[,,1], last(rcov(filt4))[,,1]))
  print(all.equal(first(rcov(fit4))[,,1], first(rcov(filt4))[,,1]))
  print(all.equal(head(fitted(fit4)), head(fitted(filt4))))
  print(all.equal(head(sigma(fit4)), head(sigma(filt4))))
  sink(type="message")
  sink()
  close(zz)
  
  # Check out of sample (VAR)
  uspec5 = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                      variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                      distribution.model = "jsu")
  
  spec5 = cgarchspec(uspec = multispec( replicate(3, uspec5) ), VAR = TRUE, 
                     robust = FALSE, lag = 1, lag.max = NULL, 
                     dccOrder = c(1,1), asymmetric = FALSE, 
                     distribution.model = list(copula = c("mvnorm", "mvt")[2], 
                                               time.varying = TRUE, 
                                               transformation = c("parametric", "empirical", "spd")[3]))
  
  fit5 = cgarchfit(spec5, data = Dat, out.sample = 100, cluster = cluster)
  specx5 = spec5
  for(i in 1:3) specx5@umodel$fixed.pars[[i]] = as.list(fit5@model$mpars[fit5@model$midx[,i]==1,i])
  setfixed(specx5)<-as.list(fit5@model$mpars[fit5@model$midx[,4]==1,4])
  filt5 = cgarchfilter(specx5, data = Dat, out.sample = 100, 
                       cluster = cluster, varcoef = fit5@model$varcoef)
  
  options(width = 120)
  zz <- file("test3a6.txt", open="wt")
  sink(zz)
  print(all.equal(last(rcov(fit5))[,,1], last(rcov(filt5))[,,1]))
  print(all.equal(first(rcov(fit5))[,,1], first(rcov(filt5))[,,1]))
  print(all.equal(head(fitted(fit5)), head(fitted(filt5))))
  print(all.equal(tail(fitted(fit5)), tail(fitted(filt5))))
  print(all.equal(head(sigma(fit5)), head(sigma(filt5))))
  sink(type="message")
  sink()
  close(zz)
  
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}

## Static Copulas
rmgarch.test3b = function(cluster = NULL)
{
  tic = Sys.time()
  data(dji30retw)
  Dat = dji30retw[, 1:3, drop = FALSE]
  
  # GARCH-Normal MVN (equivalent to CCC-Normal)
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec1 = cgarchspec(uspec = multispec( replicate(3, uspec) ), VAR = FALSE, robust = FALSE, lag = 1, lag.max = NULL, 
                     lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL,
                     robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
                     dccOrder = c(1,1), asymmetric = FALSE, distribution.model = list(copula = c("mvnorm", "mvt")[1], 
                                                                                      method = c("Kendall", "ML")[2], time.varying = FALSE, 
                                                                                      transformation = c("parametric", "empirical", "spd")[1]),
                     start.pars = list(), fixed.pars = list())
  
  fit1 = cgarchfit(spec1, data = Dat, cluster = cluster, fit.control = list(eval.se=TRUE))	
  
  specx1 = spec1
  for(i in 1:3) specx1@umodel$fixed.pars[[i]] = as.list(fit1@model$mpars[fit1@model$midx[,i]==1,i])
  setfixed(specx1)<-as.list(fit1@model$mpars[fit1@model$midx[,4]==1,4])
  filt1 = cgarchfilter(specx1, data = Dat, cluster = cluster)
  
  
  options(width = 120)
  zz <- file("test3b1.txt", open="wt")
  sink(zz)
  print(all.equal(last(rcov(fit1))[,,1], last(rcov(filt1))[,,1]))
  print(all.equal(first(rcov(fit1))[,,1], first(rcov(filt1))[,,1]))
  print(all.equal(head(fitted(fit1)), head(fitted(filt1))))
  print(all.equal(head(sigma(fit1)), head(sigma(filt1))))
  sink(type="message")
  sink()
  close(zz)
  
  # GARCH-Student MVT (equivalent to CCC-Student)
  uspec2 = ugarchspec(mean.model = list(armaOrder = c(2,1)), 
                      variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                      distribution.model = "std", fixed.pars = list(shape = 6))
  
  spec2 = cgarchspec(uspec = multispec( replicate(3, uspec2) ), 
                     VAR = FALSE, robust = FALSE, lag = 1, lag.max = NULL, 
                     lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL,
                     robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
                     distribution.model = list(copula = c("mvnorm", "mvt")[2], 
                                               method = c("Kendall", "ML")[1], time.varying = FALSE, 
                                               transformation = c("parametric", "empirical", "spd")[1]),
                     fixed.pars = list(mshape = 6))
  fit2a = cgarchfit(spec2, data = Dat, cluster = cluster,
                    fit.control = list(eval.se=TRUE))
  fit2b = cgarchfit(spec2, data = Dat, cluster = cluster,
                    fit.control = list(eval.se=FALSE))
  
  specx2 = spec2
  for(i in 1:3) specx2@umodel$fixed.pars[[i]] = as.list(fit2a@model$mpars[fit2a@model$midx[,i]==1,i])
  setfixed(specx2)<-as.list(fit2a@model$mpars[fit2a@model$midx[,4]==1,4])
  filt2 = cgarchfilter(specx2, data = Dat, cluster = cluster)
  
  
  options(width = 120)
  zz <- file("test3b2.txt", open="wt")
  sink(zz)
  print(all.equal(last(rcov(fit2a))[,,1], last(rcov(fit2b))[,,1]))
  print(all.equal(last(rcov(fit2a))[,,1], last(rcov(filt2))[,,1]))
  print(all.equal(first(rcov(fit2a))[,,1], first(rcov(fit2b))[,,1]))
  print(all.equal(first(rcov(fit2a))[,,1], first(rcov(filt2))[,,1]))
  print(all.equal(head(fitted(fit2a)), head(fitted(fit2b))))
  print(all.equal(head(fitted(fit2a)), head(fitted(filt2))))
  print(all.equal(head(sigma(fit2a)), head(sigma(fit2b))))
  print(all.equal(head(sigma(fit2a)), head(sigma(filt2))))
  sink(type="message")
  sink()
  close(zz)
  
  
  # GARCH-Student MVT + VAR+ML
  # Notice that even though we 'forget' to set arma to (0,0) and exclude the
  # mean, the program will do that check and exclude them when using VAR for
  # mean filtration (check done in the cgarchspec stage). 
  uspec3 = ugarchspec(mean.model = list(armaOrder = c(2,1)), 
                      variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                      distribution.model = "std")
  spec3 = cgarchspec(uspec = multispec( replicate(3, uspec3) ), 
                     VAR = TRUE, robust = TRUE, lag = 2, lag.max = NULL, 
                     lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL,
                     robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
                     distribution.model = list(copula = c("mvnorm", "mvt")[2], 
                                               method = c("Kendall", "ML")[2], time.varying = FALSE, 
                                               transformation = c("parametric", "empirical", "spd")[2]),
                     fixed.pars = list(mshape = 5.5))
  fit3a = cgarchfit(spec3, data = Dat, cluster = cluster,
                    fit.control = list(eval.se=TRUE))
  # pass a VAR object instead (postpad = "constant" = correct)
  # if using out.sample in the fit, make sure to subtract that from the
  # Data prior to passing to varxfilter
  varobj = varxfilter(X = Dat, p = 2, Bcoef = fit3a@model$varcoef, postpad = "constant")
  fit3b = cgarchfit(spec3, data = Dat, cluster = cluster,
                    fit.control = list(eval.se=FALSE), VAR.fit = varobj)
  
  specx3 = spec3
  for(i in 1:3) specx3@umodel$fixed.pars[[i]] = as.list(fit3a@model$mpars[fit3a@model$midx[,i]==1,i])
  setfixed(specx3)<-as.list(fit3a@model$mpars[fit3a@model$midx[,4]==1,4])
  filt3 = cgarchfilter(specx3, data = Dat, cluster = cluster,
                       varcoef = varobj$Bcoef)	
  
  options(width = 120)
  zz <- file("test3b3.txt", open="wt")
  sink(zz)
  print(all.equal(last(rcov(fit3a))[,,1], last(rcov(fit3b))[,,1]))
  print(all.equal(last(rcov(fit3a))[,,1], last(rcov(filt3))[,,1]))
  print(all.equal(first(rcov(fit3a))[,,1], first(rcov(fit3b))[,,1]))
  print(all.equal(first(rcov(fit3a))[,,1], first(rcov(filt3))[,,1]))
  print(all.equal(head(fitted(fit3a)), head(fitted(fit3b))))
  print(all.equal(head(fitted(fit3a)), head(fitted(filt3))))
  print(all.equal(head(sigma(fit3a)), head(sigma(fit3b))))
  print(all.equal(head(sigma(fit3a)), head(sigma(filt3))))
  sink(type="message")
  sink()
  close(zz)
  
  
  
  spec4 = cgarchspec(uspec = multispec( replicate(3, uspec3) ), 
                     VAR = TRUE, robust = FALSE, lag = 2, 
                     distribution.model = list(copula = c("mvnorm", "mvt")[2], 
                                               method = c("Kendall", "ML")[2], time.varying = FALSE, 
                                               transformation = c("parametric", "empirical", "spd")[2]),
                     fixed.pars = list(mshape = 5.5))
  fit4a = cgarchfit(spec4, data = Dat, out.sample = 101, cluster = cluster,
                    fit.control = list(eval.se=TRUE))
  # pass a VAR object instead (postpad = "constant" = correct)
  # if using out.sample in the fit, make sure to subtract that from the
  # Dat prior to passing to varxfilter
  # NB: if robust=TRUE, then you will never get exactly the same results (see
  # reference paper on algorithm to understand why).
  varobj1 = varxfilter(X = Dat[1:(NROW(Dat)-101),], p = 2, Bcoef = fit4a@model$varcoef, postpad = "constant")
  varobj2 = varxfit(X = Dat[1:(NROW(Dat)-101),], p = 2, postpad = "constant")
  
  fit4b = cgarchfit(spec4, data = Dat, out.sample = 101, cluster = cluster,
                    fit.control = list(eval.se=FALSE), VAR.fit = varobj2)
  
  specx4 = spec4
  for(i in 1:3) specx4@umodel$fixed.pars[[i]] = as.list(fit4a@model$mpars[fit4a@model$midx[,i]==1,i])
  setfixed(specx4)<-as.list(fit4a@model$mpars[fit4a@model$midx[,4]==1,4])
  filt4 = cgarchfilter(specx4, data = Dat, out.sample = 101, cluster = cluster,
                       varcoef = varobj1$Bcoef)
  
  options(width = 120)
  zz <- file("test3b4.txt", open="wt")
  sink(zz)
  print(all.equal(last(rcov(fit4a))[,,1], last(rcov(fit4b))[,,1]))
  print(all.equal(last(rcov(fit4a))[,,1], last(rcov(filt4))[,,1]))
  print(all.equal(first(rcov(fit4a))[,,1], first(rcov(fit4b))[,,1]))
  print(all.equal(first(rcov(fit4a))[,,1], first(rcov(filt4))[,,1]))
  print(all.equal(head(fitted(fit4a)), head(fitted(fit4b))))
  print(all.equal(head(fitted(fit4a)), head(fitted(filt4))))
  print(all.equal(head(sigma(fit4a)), head(sigma(fit4b))))
  print(all.equal(head(sigma(fit4a)), head(sigma(filt4))))
  sink(type="message")
  sink()
  close(zz)
  
  
  # Test one with time-varying
  spec5 = cgarchspec(uspec = multispec( replicate(3, uspec3) ), 
                     VAR = TRUE, robust = FALSE, lag = 2, 
                     distribution.model = list(copula = c("mvnorm", "mvt")[2], 
                                               method = c("Kendall", "ML")[2], time.varying = TRUE, 
                                               transformation = c("parametric", "empirical", "spd")[2]),
                     fixed.pars = list(mshape = 5.5))
  fit5 = cgarchfit(spec5, data = Dat, out.sample = 101, cluster = cluster,
                   fit.control = list(eval.se=TRUE))
  # pass a VAR object instead (postpad default = "constant" = correct)
  # if using out.sample in the fit, make sure to subtract that from the
  # Data prior to passing to varxfilter
  # NB: if robust=TRUE, then you will never get exactly the same results (see
  # reference paper on algorithm to understand why).
  
  fit5 = cgarchfit(spec5, data = Dat, out.sample = 101, cluster = cluster,
                   fit.control = list(eval.se=FALSE), VAR.fit = varobj2)
  
  specx5 = spec5
  for(i in 1:3) specx5@umodel$fixed.pars[[i]] = as.list(fit5@model$mpars[fit5@model$midx[,i]==1,i])
  setfixed(specx5)<-as.list(fit5@model$mpars[fit5@model$midx[,4]==1,4])
  filt5 = cgarchfilter(specx5, data = Dat, out.sample = 101, cluster = cluster,
                       varcoef = varobj1$Bcoef)	
  
  options(width = 120)
  zz <- file("test3b5.txt", open="wt")
  sink(zz)
  print(all.equal(last(rcov(fit5))[,,1], last(rcov(filt5))[,,1]))
  print(all.equal(first(rcov(fit5))[,,1], first(rcov(filt5))[,,1]))
  print(all.equal(head(fitted(fit5)), head(fitted(filt5))))
  print(all.equal(head(sigma(fit5)), head(sigma(filt5))))
  sink(type="message")
  sink()
  close(zz)
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}

# Filter & out of sample
rmgarch.test3c = function(cluster = NULL)
{
  tic = Sys.time()
  data(dji30retw)
  Dat = dji30retw[, 1:3, drop = FALSE]
  
  # GARCH-Normal MVN (equivalent to CCC-Normal)
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec1 = cgarchspec(uspec = multispec( replicate(3, uspec) ), VAR = FALSE, robust = FALSE, lag = 1, lag.max = NULL, 
                     lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL,
                     robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
                     dccOrder = c(1,1), asymmetric = FALSE, distribution.model = list(copula = c("mvnorm", "mvt")[1], 
                                                                                      method = c("Kendall", "ML")[2], time.varying = FALSE, 
                                                                                      transformation = c("parametric", "empirical", "spd")[1]),
                     start.pars = list(), fixed.pars = list())
  
  fit1 = cgarchfit(spec1, data = Dat, out.sample = 100, cluster = cluster,
                   fit.control = list(eval.se=FALSE))
  T = dim(Dat)[1]-100
  
  specx1 = spec1
  for(i in 1:3) specx1@umodel$fixed.pars[[i]] = as.list(fit1@model$mpars[fit1@model$midx[,i]==1,i])
  setfixed(specx1)<-as.list(fit1@model$mpars[fit1@model$midx[,4]==1,4])
  
  # including n.old filters based on the full assumptions of the fitted model
  filt1 = cgarchfilter(specx1, data = Dat[1:(T+100), ], filter.control  = list(n.old = T))
  # without using n.old
  filt2 = cgarchfilter(specx1, data = Dat[1:(T+100), ])
  
  options(width = 120)
  zz <- file("test3c1.txt", open="wt")
  sink(zz)
  # What happens is:
  # The initial variance used to start the garch iteration is based on the
  # full data set provided (T+100) unless explicitly using n.old. Therefore,
  # the first few values of the covariance are different for filt2 which used
  # an additional 100 points to estimate the initial variance....
  print(all.equal(rcov(fit1)[,,1], rcov(filt1)[,,1]))
  print(all.equal(rcov(fit1)[,,1], rcov(filt2)[,,1]))
  # ... as T grow, the impact of the initial values decays and the results is
  # the same for methods using either n.old and without
  print(all.equal(rcov(fit1)[,,T-2], rcov(filt1)[,,T-2]))
  print(all.equal(rcov(fit1)[,,T-2], rcov(filt2)[,,T-2]))
  sink(type="message")
  sink()
  close(zz)
  
  
  # spd
  spec2 = cgarchspec(uspec = multispec( replicate(3, uspec) ), VAR = FALSE, robust = FALSE, lag = 1, lag.max = NULL, 
                     lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL,
                     robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
                     dccOrder = c(1,1), asymmetric = FALSE, distribution.model = list(copula = c("mvnorm", "mvt")[1], 
                                                                                      method = c("Kendall", "ML")[2], time.varying = FALSE, 
                                                                                      transformation = c("parametric", "empirical", "spd")[3]),
                     start.pars = list(), fixed.pars = list())
  
  fit2 = cgarchfit(spec2, data = Dat, out.sample = 100, cluster = cluster,
                   fit.control = list(eval.se=FALSE))
  T = dim(Dat)[1]-100
  
  specx2 = spec2
  for(i in 1:3) specx2@umodel$fixed.pars[[i]] = as.list(fit2@model$mpars[fit2@model$midx[,i]==1,i])
  setfixed(specx2)<-as.list(fit2@model$mpars[fit2@model$midx[,4]==1,4])
  
  # including n.old filters based on the full assumptions of the fitted model
  filt2a = cgarchfilter(specx2, data = Dat[1:(T+100), ], filter.control  = list(n.old = T))
  # without using n.old
  filt2b = cgarchfilter(specx2, data = Dat[1:(T+100), ])
  
  filt2c = cgarchfilter(specx2, data = Dat[1:(T), ])
  
  options(width = 120)
  zz <- file("test3c2.txt", open="wt")
  sink(zz)
  # What happens is:
  # The initial variance used to start the garch iteration is based on the
  # full data set provided (T+100) unless explicitly using n.old. Therefore,
  # the first few values of the covariance are different for filt2 which used
  # an additional 100 points to estimate the initial variance....
  print(all.equal(rcov(fit2)[,,1], rcov(filt2a)[,,1]))
  print(all.equal(rcov(fit2)[,,1], rcov(filt2b)[,,1]))
  # ... as T grow, the impact of the initial values decays and the results is
  # the same for methods using either n.old and without
  print(all.equal(rcov(fit2)[,,T], rcov(filt2a)[,,T]))
  print(all.equal(rcov(fit2)[,,T], rcov(filt2b)[,,T]))
  sink(type="message")
  sink()
  close(zz)
  
  # Parametric & timecopula & VAR
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                     variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec3 = cgarchspec(uspec = multispec( replicate(3, uspec) ), VAR = TRUE, robust = FALSE, lag = 1, lag.max = NULL, 
                     lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL,
                     robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
                     dccOrder = c(1,1), asymmetric = FALSE, distribution.model = list(copula = c("mvnorm", "mvt")[1], 
                                                                                      method = c("Kendall", "ML")[2], time.varying = TRUE, 
                                                                                      transformation = c("parametric", "empirical", "spd")[1]),
                     start.pars = list(), fixed.pars = list())
  
  fit3 = cgarchfit(spec3, data = Dat, out.sample = 100, cluster = cluster,
                   fit.control = list(eval.se=FALSE))
  T = dim(Dat)[1]-100
  
  specx3 = spec3
  for(i in 1:3) specx3@umodel$fixed.pars[[i]] = as.list(fit3@model$mpars[fit3@model$midx[,i]==1,i])
  setfixed(specx3)<-as.list(fit3@model$mpars[fit3@model$midx[,4]==1,4])
  
  
  # including n.old filters based on the full assumptions of the fitted model
  filt3a = cgarchfilter(specx3, data = Dat[1:(T+100), ], filter.control = list(n.old = T), varcoef = fit3@model$varcoef)
  # without using n.old
  filt3b = cgarchfilter(specx3, data = Dat[1:(T+100), ], varcoef = fit3@model$varcoef)
  
  filt3c = cgarchfilter(specx3, data = Dat[1:(T), ], varcoef = fit3@model$varcoef)
  
  options(width = 120)
  zz <- file("test3c3.txt", open="wt")
  sink(zz)
  print(all.equal(rcov(fit3)[,,1], rcov(filt3a)[,,1]))
  print(all.equal(rcov(fit3)[,,1], rcov(filt3b)[,,1]))
  print(all.equal(rcov(fit3)[,,1], rcov(filt3c)[,,1]))
  print(all.equal(head(fitted(fit3)), head(fitted(filt3a))))
  print(all.equal(head(fitted(fit3)), head(fitted(filt3b))))
  print(all.equal(head(fitted(fit3)), head(fitted(filt3c))))	
  # ... as T grow, the impact of the initial values decays and the results is
  # the same for methods using either n.old and without (almost for 3b..)
  print(all.equal(rcov(fit3)[,,T-2], rcov(filt3a)[,,T-2]))
  print(all.equal(rcov(fit3)[,,T-2], rcov(filt3b)[,,T-2]))
  sink(type="message")
  sink()
  close(zz)
  
  
  # spd & timecopula & VAR
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec4 = cgarchspec(uspec = multispec( replicate(3, uspec) ), VAR = TRUE, robust = FALSE, lag = 2, lag.max = NULL, 
                     lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL,
                     robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
                     dccOrder = c(1,1), asymmetric = FALSE, distribution.model = list(copula = c("mvnorm", "mvt")[1], 
                                                                                      method = c("Kendall", "ML")[2], time.varying = TRUE, 
                                                                                      transformation = c("parametric", "empirical", "spd")[3]),
                     start.pars = list(), fixed.pars = list())
  
  fit4 = cgarchfit(spec4, data = Dat, out.sample = 100, cluster = cluster,
                   fit.control = list(eval.se=FALSE))
  T = dim(Dat)[1]-100
  
  specx4 = spec4
  for(i in 1:3) specx4@umodel$fixed.pars[[i]] = as.list(fit4@model$mpars[fit4@model$midx[,i]==1,i])
  setfixed(specx4)<-as.list(fit4@model$mpars[fit4@model$midx[,4]==1,4])
  
  # including n.old filters based on the full assumptions of the fitted model
  filt4a = cgarchfilter(specx4, data = Dat[1:(T+100), ], filter.control  = list(n.old = T), varcoef = fit4@model$varcoef)
  # without using n.old
  filt4b = cgarchfilter(specx4, data = Dat[1:(T+100), ], varcoef = fit4@model$varcoef)
  
  filt4c = cgarchfilter(specx4, data = Dat[1:(T), ], varcoef = fit4@model$varcoef)
  
  options(width = 120)
  zz <- file("test3c3.txt", open="wt")
  sink(zz)
  # What happens is:
  # The initial variance used to start the garch iteration is based on the
  # full data set provided (T+100) unless explicitly using n.old. Therefore,
  # the first few values of the covariance are different for filt2 which used
  # an additional 100 points to estimate the initial variance....
  print(all.equal(rcov(fit4)[,,1], rcov(filt4a)[,,1]))
  print(all.equal(rcov(fit4)[,,1], rcov(filt4b)[,,1]))
  print(all.equal(head(fitted(fit4)), head(fitted(filt4a))))
  # ... as T grow, the impact of the initial values decays and the results is
  # the same for methods using either n.old and without (almost for 4b..)
  print(all.equal(rcov(fit4)[,,T-2], rcov(filt4a)[,,T-2]))
  print(all.equal(rcov(fit4)[,,T-2], rcov(filt4b)[,,T-2]))
  sink(type="message")
  sink()
  close(zz)
  
  
  # spd & timecopula & ARMA
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec5 = cgarchspec(uspec = multispec( replicate(3, uspec) ), VAR = FALSE, robust = FALSE, lag = 1, lag.max = NULL, 
                     lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL,
                     robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
                     dccOrder = c(1,1), asymmetric = FALSE, distribution.model = list(copula = c("mvnorm", "mvt")[1], 
                                                                                      method = c("Kendall", "ML")[2], time.varying = TRUE, 
                                                                                      transformation = c("parametric", "empirical", "spd")[3]),
                     start.pars = list(), fixed.pars = list())
  
  fit5 = cgarchfit(spec5, data = Dat, out.sample = 100, cluster = cluster,
                   fit.control = list(eval.se=FALSE))
  T = dim(Dat)[1]-100
  
  specx5 = spec5
  for(i in 1:3) specx5@umodel$fixed.pars[[i]] = as.list(fit5@model$mpars[fit5@model$midx[,i]==1,i])
  setfixed(specx5)<-as.list(fit5@model$mpars[fit5@model$midx[,4]==1,4])
  
  # including n.old filters based on the full assumptions of the fitted model
  filt5a = cgarchfilter(specx5, data = Dat[1:(T+100), ], filter.control  = list(n.old = T))
  # without using n.old
  filt5b = cgarchfilter(specx5, data = Dat[1:(T+100), ])
  
  filt5c = cgarchfilter(specx5, data = Dat[1:(T), ])
  
  options(width = 120)
  zz <- file("test3c3.txt", open="wt")
  sink(zz)
  # What happens is:
  # The initial variance used to start the garch iteration is based on the
  # full data set provided (T+100) unless explicitly using n.old. Therefore,
  # the first few values of the covariance are different for filt2 which used
  # an additional 100 points to estimate the initial variance....
  print(all.equal(rcov(fit5)[,,1], rcov(filt5a)[,,1]))
  print(all.equal(rcov(fit5)[,,1], rcov(filt5b)[,,1]))
  print(all.equal(head(fitted(fit5)), head(fitted(filt5a))))
  # ... as T grow, the impact of the initial values decays and the results is
  # the same for methods using either n.old and without (almost for 5b...decay somewhat
  # faster for ARMA than VAR)
  print(all.equal(rcov(fit5)[,,T-2], rcov(filt5a)[,,T-2]))
  print(all.equal(rcov(fit5)[,,T-2],rcov(filt5b)[,,T-2]))
  sink(type="message")
  sink()
  close(zz)
  
  
  # empirial & timecopula & ARMA
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec6 = cgarchspec(uspec = multispec( replicate(3, uspec) ), VAR = FALSE, robust = FALSE, lag = 1, lag.max = NULL, 
                     lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL,
                     robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
                     dccOrder = c(1,1), asymmetric = FALSE, distribution.model = list(copula = c("mvnorm", "mvt")[1], 
                                                                                      method = c("Kendall", "ML")[2], time.varying = TRUE, 
                                                                                      transformation = c("parametric", "empirical", "spd")[2]),
                     start.pars = list(), fixed.pars = list())
  
  fit6 = cgarchfit(spec6, data = Dat, out.sample = 100, cluster = cluster,
                   fit.control = list(eval.se=FALSE))
  T = dim(Dat)[1]-100
  
  specx6 = spec6
  for(i in 1:3) specx6@umodel$fixed.pars[[i]] = as.list(fit6@model$mpars[fit6@model$midx[,i]==1,i])
  setfixed(specx6)<-as.list(fit6@model$mpars[fit6@model$midx[,4]==1,4])
  
  # including n.old filters based on the full assumptions of the fitted model
  filt6a = cgarchfilter(specx6, data = Dat[1:(T+100), ], filter.control  = list(n.old = T))
  # without using n.old
  filt6b = cgarchfilter(specx6, data = Dat[1:(T+100), ])
  
  filt6c = cgarchfilter(specx6, data = Dat[1:(T), ])
  
  options(width = 120)
  zz <- file("test3c3.txt", open="wt")
  sink(zz)
  # What happens is:
  # The initial variance used to start the garch iteration is based on the
  # full data set provided (T+100) unless explicitly using n.old. Therefore,
  # the first few values of the covariance are different for filt2 which used
  # an additional 100 points to estimate the initial variance....
  print(all.equal(rcov(fit6)[,,1], rcov(filt6a)[,,1]))
  print(all.equal(rcov(fit6)[,,1], rcov(filt6b)[,,1]))
  print(all.equal(head(fitted(fit6)), head(fitted(filt6a))))
  # ... as T grow, the impact of the initial values decays and the results is
  # the same for methods using either n.old and without (almost for 6b)
  print(all.equal(rcov(fit6)[,,T-2], rcov(filt6a)[,,T-2]))
  print(all.equal(rcov(fit6)[,,T-2], rcov(filt6b)[,,T-2]))
  sink(type="message")
  sink()
  close(zz)
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}
## Simulation static copula
rmgarch.test3d = function(cluster = NULL)
{
  
  tic = Sys.time()
  data(dji30retw)
  Dat = dji30retw[, 1:3, drop = FALSE]
  
  # GARCH-Normal MVN (equivalent to CCC-Normal)
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec1 = cgarchspec(uspec = multispec( replicate(3, uspec) ), VAR = FALSE, robust = FALSE, lag = 1, lag.max = NULL, 
                     lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL,
                     robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
                     dccOrder = c(1,1), asymmetric = FALSE, distribution.model = list(copula = c("mvnorm", "mvt")[1], 
                                                                                      method = c("Kendall", "ML")[2], time.varying = FALSE, 
                                                                                      transformation = c("parametric", "empirical", "spd")[1]),
                     start.pars = list(), fixed.pars = list())
  
  fit1 = cgarchfit(spec1, data = Dat, cluster = cluster,
                   fit.control = list(eval.se=FALSE))
  
  sim1 = cgarchsim(fit1, n.sim = 1000, m.sim = 10, startMethod = "sample")
  sim2 = cgarchsim(fit1, n.sim = 2, m.sim = 1000, startMethod = "sample")
  # 2-ahead mean covariance (1-ahead has no uncertainty).
  meanH = matrix(0,  3, 3)
  for(i in 1:1000) meanH = meanH + sim2@msim$simH[[i]][,,2]
  meanH = meanH/1000
  ebars = matrix(NA, ncol = 3, nrow = 1000)
  ebars[,1] = sapply(sim2@msim$simH, FUN = function(x) x[1,2,2])
  ebars[,2] = sapply(sim2@msim$simH, FUN = function(x) x[1,3,2])
  ebars[,3] = sapply(sim2@msim$simH, FUN = function(x) x[2,3,2])
  
  postscript("test3d1.eps", width = 10, height = 8)
  boxplot(ebars, notch = TRUE, names = c("H12", "H13", "H23"), main = "Simulated 2-ahead Covariance",
          col = 2:4)
  dev.off()
  
  # constant correlation:
  #print(all.equal(cov2cor(sim1@msim$simH[[1]][,,1]), cov2cor(sim1@msim$simH[[2]][,,40]), cov2cor(sim1@msim$simH[[10]][,,1000])))
  
  # 1-ahead rolling forecast exercise
  fit3 = cgarchfit(spec1, data = Dat, out.sample = 100, cluster = cluster,
                   fit.control = list(eval.se=FALSE))
  T = dim(Dat)[1]-100
  simMu = simS = filtMu = filtS = matrix(NA, ncol = 3, nrow = 100)
  simC = filtC = array(NA, dim = c(3,3,100))
  colSd = function(x) apply(x, 2, "sd")
  specx = spec1
  for(i in 1:3) specx@umodel$fixed.pars[[i]] = as.list(fit3@model$mpars[fit3@model$midx[,i]==1,i])
  setfixed(specx)<-as.list(fit3@model$mpars[fit3@model$midx[,4]==1,4])
  
  for(i in 1:100){
    if(i==1){
      presigma = matrix(tail(sigma(fit3), 2), ncol = 3)
      # arma = c(2,1) therefore need 2 lags
      prereturns = matrix(unlist(Dat[(T-1):T, ]), ncol = 3, nrow = 2)
      preresiduals = matrix(tail(residuals(fit3),2), ncol = 3, nrow = 2)
      
      tmp = cgarchfilter(specx, Dat[1:(T+1), ], filter.control = list(n.old = T))
      filtMu[i,] = tail(fitted(tmp), 1)
      filtS[i,] = tail(sigma(tmp), 1)
      filtC[,,i] = last(rcov(tmp))[,,1]
    } else{
      presigma = matrix(tail(sigma(tmp), 2), ncol = 3)
      # arma = c(2,1) therefore need 2 lags
      prereturns = matrix(unlist(Dat[(T+i-2):(T+i-1), ]), ncol = 3, nrow = 2)
      preresiduals = matrix(tail(residuals(tmp),2), ncol = 3, nrow = 2)
      
      tmp = cgarchfilter(specx, Dat[1:(T+i), ], filter.control = list(n.old = T))			
      filtMu[i,] = tail(fitted(tmp), 1)
      filtS[i,] = tail(sigma(tmp), 1)
      filtC[,,i] = last(rcov(tmp))[,,1]
    }
    sim3 = cgarchsim(fit3, n.sim = 1, m.sim = 10000, startMethod = "sample", prereturns = prereturns,
                     presigma = presigma, preresiduals = preresiduals)
    simx = t(sapply(sim3@msim$simX, FUN = function(x) x[1,]))
    simMu[i,] = colMeans(simx)
    simS[i,] = colSd(simx)
    simC[,,i] = cov(simx)
    print(i)
    
    # CHECK:
    # X[t+1] = mu[t+1] + e[t+1]
    # sim3@msim$simX[[i]][1,] - (sim3@msim$simZ[,,i]*sqrt(diag(sim3@msim$simH[[i]][,,1])))
    # is equal to filtMu[i,]
    if(i < 3 ){
      print(all.equal(sim3@msim$simX[[2]][1,] - (sim3@msim$simZ[,,2]*sqrt(diag(sim3@msim$simH[[2]][,,1]))), 
                      filtMu[i,]))
      print(all.equal(sim3@msim$simX[[10000]][1,] - (sim3@msim$simZ[,,10000]*sqrt(diag(sim3@msim$simH[[10000]][,,1]))), 
                      filtMu[i,]))
    }
  }
  
  postscript("test3d2.eps", width = 10, height = 8)
  par(mfrow = c(2,2))
  plot(simMu[,1], type = "l", main = "AA Conditional Mean\nRolling Forecast",
       ylab = "mu", xlab = "Time")
  lines(filtMu[,1], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simMu[,2], type = "l", main = "AXP Conditional Mean\nRolling Forecast",
       ylab = "mu", xlab = "Time")
  lines(filtMu[,2], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simMu[,3], type = "l", main = "BA Conditional Mean\nRolling Forecast",
       ylab = "mu", xlab = "Time")
  lines(filtMu[,3], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  
  dev.off()
  
  postscript("test3d3.eps", width = 10, height = 8)
  par(mfrow = c(2,2))
  plot(simS[,1], type = "l", main = "AA Conditional Sigma\nRolling Forecast",
       ylab = "sigma", xlab = "Time")
  lines(filtS[,1], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simS[,2], type = "l", main = "AXP Conditional Sigma\nRolling Forecast",
       ylab = "sigma", xlab = "Time")
  lines(filtS[,2], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simS[,3], type = "l", main = "BA Conditional Sigma\nRolling Forecast",
       ylab = "sigma", xlab = "Time")
  lines(filtS[,3], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  dev.off()
  
  postscript("test3d4.eps", width = 10, height = 8)
  par(mfrow = c(2,2))
  plot(simC[1,2,], type = "l", main = "AA-AXP Conditional Covariance\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtC[1,2,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simC[1,3,], type = "l", main = "AA-BA Conditional Covariance\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtC[1,3,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  
  plot(simC[2,3,], type = "l", main = "AXP-BA Conditional Covariance\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtC[2,3,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  
  dev.off()
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
  
}

## Simulation dynamic copula (parametric)
rmgarch.test3e = function(cluster = NULL)
{
  tic = Sys.time()
  data(dji30retw)
  Dat = dji30retw[, 1:3, drop = FALSE]
  
  
  # GARCH-Normal MVN
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "norm")
  spec1 = cgarchspec(uspec = multispec( replicate(3, uspec) ), VAR = TRUE, robust = FALSE, lag = 2, lag.max = NULL, 
                     lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL,
                     robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
                     dccOrder = c(1,1), asymmetric = FALSE, distribution.model = list(copula = c("mvnorm", "mvt")[1], 
                                                                                      method = c("Kendall", "ML")[2], time.varying = TRUE, 
                                                                                      transformation = c("parametric", "empirical", "spd")[1]))
  
  fit1 = cgarchfit(spec1, data = Dat, cluster = cluster, fit.control = list(eval.se=FALSE))
  
  
  sim1 = cgarchsim(fit1, n.sim = 1000, m.sim = 10, startMethod = "sample", cluster=cluster)
  sim2 = cgarchsim(fit1, n.sim = 2, m.sim = 1000, startMethod = "sample", cluster=cluster)
  # 2-ahead mean covariance (1-ahead has no uncertainty).
  meanH = matrix(0,  3, 3)
  for(i in 1:1000) meanH = meanH + sim2@msim$simH[[i]][,,2]
  meanH = meanH/1000
  ebars = matrix(NA, ncol = 3, nrow = 1000)
  ebars[,1] = sapply(sim2@msim$simH, FUN = function(x) x[1,2,2])
  ebars[,2] = sapply(sim2@msim$simH, FUN = function(x) x[1,3,2])
  ebars[,3] = sapply(sim2@msim$simH, FUN = function(x) x[2,3,2])
  
  postscript("test3e1.eps", width = 10, height = 8)
  boxplot(ebars, notch = TRUE, names = c("H12", "H13", "H23"), main = "Simulated 2-ahead Covariance",
          col = 2:4)
  dev.off()
  
  
  # 1-ahead rolling forecast exercise (but without updating initial VAR model)
  fit3 = cgarchfit(spec1, data = Dat, out.sample = 100, cluster = cluster,
                   fit.control = list(eval.se=FALSE))
  T = dim(Dat)[1]-100
  simMu = simS = filtMu = filtS = matrix(NA, ncol = 3, nrow = 100)
  simCor = simC = filtC = filtCor = array(NA, dim = c(3,3,100))
  colSd = function(x) apply(x, 2, "sd")
  specx = spec1
  for(i in 1:3) specx@umodel$fixed.pars[[i]] = as.list(fit3@model$mpars[fit3@model$midx[,i]==1,i])
  setfixed(specx)<-as.list(fit3@model$mpars[fit3@model$midx[,4]==1,4])
  
  for(i in 1:100){
    if(i==1){
      presigma = matrix(tail(sigma(fit3), 2), ncol = 3)
      # arma = c(2,1) therefore need 2 lags
      prereturns = matrix(unlist(Dat[(T-1):T, ]), ncol = 3, nrow = 2)
      preresiduals = matrix(tail(residuals(fit3),2), ncol = 3, nrow = 2)
      preR = last(rcor(fit3))[,,1]
      diag(preR) = 1
      preQ = fit3@mfit$Qt[[length(fit3@mfit$Qt)]]
      preZ = tail(fit3@mfit$Z, 1)
      tmp = cgarchfilter(specx, Dat[1:(T+1), ], filter.control = list(n.old = T), varcoef = fit3@model$varcoef)
      filtMu[i,] = tail(fitted(tmp), 1)
      filtS[i,] = tail(sigma(tmp), 1)
      filtC[,,i] = last(rcov(tmp))[,,1]
      filtCor[,,i] = last(rcor(tmp))[,,1]
    } else{
      presigma = matrix(tail(sigma(tmp), 2), ncol = 3)
      # arma = c(2,1) therefore need 2 lags
      prereturns = matrix(unlist(Dat[(T+i-2):(T+i-1), ]), ncol = 3, nrow = 2)
      preresiduals = matrix(tail(residuals(tmp),2), ncol = 3, nrow = 2)
      preR = last(rcor(tmp))[,,1]
      diag(preR) = 1
      preQ = tmp@mfilter$Qt[[length(tmp@mfilter$Qt)]]
      preZ = tail(tmp@mfilter$Z, 1)
      
      tmp = cgarchfilter(specx, Dat[1:(T+i), ], filter.control = list(n.old = T), varcoef = fit3@model$varcoef)
      filtMu[i,] = tail(fitted(tmp), 1)
      filtS[i,] = tail(sigma(tmp), 1)
      filtC[,,i] = last(rcov(tmp))[,,1]
      filtCor[,,i] = last(rcor(tmp))[,,1]
    }
    sim3 = cgarchsim(fit3, n.sim = 1, m.sim = 2000, startMethod = "sample", preR = preR, preQ = preQ, preZ = preZ,
                     prereturns = prereturns, presigma = presigma, preresiduals = preresiduals, cluster = cluster)
    simx = t(sapply(sim3@msim$simX, FUN = function(x) x[1,]))
    # Note: There is no uncertainty for the 1-ahead simulation of cov or cor
    simC[,,i] = sim3@msim$simH[[1]][,,1]
    simCor[,,i] = sim3@msim$simR[[1]][,,1]
    simMu[i,] = colMeans(simx)
    simS[i,] = sqrt(diag(simC[,,i]))
    
    print(i)
    # CHECK:
    # X[t+1] = mu[t+1] + e[t+1]
    # sim3@msim$simX[[i]][1,] - sim3@msim$simRes[[i]][1,]
    # is equal to filtMu[i,]
    if(i == 1){
      print(all.equal(sim3@msim$simX[[2]][1,] - (sim3@msim$simZ[,,2]*sqrt(diag(sim3@msim$simH[[2]][,,1]))), 
                      filtMu[i,]))
      print(all.equal(sim3@msim$simX[[2000]][1,] - (sim3@msim$simZ[,,2000]*sqrt(diag(sim3@msim$simH[[2000]][,,1]))), 
                      filtMu[i,]))
    }
    if(i == 2){
      print(all.equal(sim3@msim$simX[[2]][1,] - (sim3@msim$simZ[,,2]*sqrt(diag(sim3@msim$simH[[2]][,,1]))), 
                      filtMu[i,]))
      print(all.equal(sim3@msim$simX[[2000]][1,] - (sim3@msim$simZ[,,2000]*sqrt(diag(sim3@msim$simH[[2000]][,,1]))), 
                      filtMu[i,]))
    }
  }
  
  
  postscript("test3e2.eps", width = 10, height = 8)
  par(mfrow = c(2,2))
  plot(simMu[,1], type = "l", main = "AA Conditional Mean\nRolling Forecast",
       ylab = "mu", xlab = "Time")
  lines(filtMu[,1], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simMu[,2], type = "l", main = "AXP Conditional Mean\nRolling Forecast",
       ylab = "mu", xlab = "Time")
  lines(filtMu[,2], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simMu[,3], type = "l", main = "BA Conditional Mean\nRolling Forecast",
       ylab = "mu", xlab = "Time")
  lines(filtMu[,3], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  dev.off()
  
  postscript("test3e3.eps", width = 10, height = 8)
  par(mfrow = c(2,2))
  plot(simS[,1], type = "l", main = "AA Conditional Sigma\nRolling Forecast",
       ylab = "sigma", xlab = "Time")
  lines(filtS[,1], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simS[,2], type = "l", main = "AXP Conditional Sigma\nRolling Forecast",
       ylab = "sigma", xlab = "Time")
  lines(filtS[,2], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simS[,3], type = "l", main = "BA Conditional Sigma\nRolling Forecast",
       ylab = "sigma", xlab = "Time")
  lines(filtS[,3], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  dev.off()
  
  postscript("test3e4.eps", width = 10, height = 8)
  par(mfrow = c(2,2))
  plot(simC[1,2,], type = "l", main = "AA-AXP Conditional Covariance\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtC[1,2,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simC[1,3,], type = "l", main = "AA-BA Conditional Covariance\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtC[1,3,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  
  plot(simC[2,3,], type = "l", main = "AXP-BA Conditional Covariance\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtC[2,3,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  dev.off()
  
  postscript("test3e5.eps", width = 10, height = 8)
  par(mfrow = c(2,2))
  plot(simCor[1,2,], type = "l", main = "AA-AXP Conditional Correlation\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtCor[1,2,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simCor[1,3,], type = "l", main = "AA-BA Conditional Correlation\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtCor[1,3,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  
  plot(simCor[2,3,], type = "l", main = "AXP-BA Conditional Correlation\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtCor[2,3,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  dev.off()
  
  
  # n-ahead forecast exercise
  # when m.sim == 1, no need to supply any pre-data since the startMethod 
  # indicates that we extract the last values from the fit object
  
  sim4 = cgarchsim(fit3, n.sim = 1000, n.start = 500, m.sim = 100, startMethod = "sample", cluster = cluster)
  
  mR = matrix(0, nrow = 100, ncol = 3)
  for(i in 1:100){
    rc = rcor(sim4, sim = i)
    meanR = matrix(0, 3, 3)
    for(j in 1:1000) meanR = meanR + rc[,,j]
    # simulated average correlation
    meanR = meanR/1000
    mR[i,] = c(meanR[1,2], meanR[1,3], meanR[2,3])
    #print(i)
  }
  UQ = fit3@mfit$Qbar*(1-sum(coef(fit3, "dcc")))
  UR = UQ/(sqrt(diag(UQ)) %*% t(sqrt(diag(UQ)) ))
  
  postscript("test3e6.eps", width = 10, height = 8)
  par(mfrow = c(2,2))
  hist(mR[,1], main = "AA-AXP Simulated \nUnconditional Correlation", xlab = "cor", cex.main=0.8)
  abline(v = UR[1,2], col = "orange", lwd = 2)
  legend("topright", "Unconditional \n(Analytical)", col = "orange", lwd=2, bty = "n", 
         cex = 0.7)
  
  hist(mR[,2], main = "AA-BA Simulated \nUnconditional Correlation", xlab = "cor", cex.main=0.8)
  abline(v = UR[1,3], col = "orange", lwd = 2)
  legend("topright", "Unconditional \n(Analytical)", col = "orange", lwd=2, bty = "n", 
         cex = 0.7)
  
  hist(mR[,3], main = "AXP-BA Simulated \nUnconditional Correlation", xlab = "cor", cex.main=0.8)
  abline(v = UR[2,3], col = "orange", lwd = 2)
  legend("topright", "Unconditional \n(Analytical)", col = "orange", lwd=2, bty = "n", 
         cex = 0.7)
  dev.off()
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}

## Simulation dynamic copula (spd)
rmgarch.test3f = function(cluster = NULL)
{
  tic = Sys.time()
  data(dji30retw)
  Dat = dji30retw[, 1:3, drop = FALSE]
  
  
  # GARCH-Normal MVN
  uspec = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "jsu")
  spec1 = cgarchspec(uspec = multispec( replicate(3, uspec) ), VAR = TRUE, robust = FALSE, lag = 2, lag.max = NULL, 
                     lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL,
                     robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
                     dccOrder = c(1,1), asymmetric = FALSE, distribution.model = list(copula = c("mvnorm", "mvt")[1], 
                                                                                      method = c("Kendall", "ML")[2], time.varying = TRUE, 
                                                                                      transformation = c("parametric", "empirical", "spd")[3]))
  
  fit1 = cgarchfit(spec1, data = Dat, cluster = cluster,
                   fit.control = list(eval.se=FALSE))
  
  
  sim1 = cgarchsim(fit1, n.sim = 1000, m.sim = 10, startMethod = "sample", cluster = cluster)
  sim2 = cgarchsim(fit1, n.sim = 2, m.sim = 1000, startMethod = "sample", cluster = cluster)
  # 2-ahead mean covariance (1-ahead has no uncertainty).
  meanH = matrix(0,  3, 3)
  for(i in 1:1000) meanH = meanH + sim2@msim$simH[[i]][,,2]
  meanH = meanH/1000
  ebars = matrix(NA, ncol = 3, nrow = 1000)
  ebars[,1] = sapply(sim2@msim$simH, FUN = function(x) x[1,2,2])
  ebars[,2] = sapply(sim2@msim$simH, FUN = function(x) x[1,3,2])
  ebars[,3] = sapply(sim2@msim$simH, FUN = function(x) x[2,3,2])
  
  postscript("test3f1.eps", width = 10, height = 8)
  boxplot(ebars, notch = TRUE, names = c("H12", "H13", "H23"), main = "Simulated 2-ahead Covariance",
          col = 2:4)
  dev.off()
  
  
  # 1-ahead rolling forecast exercise
  fit3 = cgarchfit(spec1, data = Dat, out.sample = 100, cluster = cluster,
                   fit.control = list(eval.se=FALSE))
  T = dim(Dat)[1]-100
  simMu = simS = filtMu = filtS = matrix(NA, ncol = 3, nrow = 100)
  simCor = simC = filtC = filtCor = array(NA, dim = c(3,3,100))
  colSd = function(x) apply(x, 2, "sd")
  specx = spec1
  for(i in 1:3) specx@umodel$fixed.pars[[i]] = as.list(fit3@model$mpars[fit3@model$midx[,i]==1,i])
  setfixed(specx)<-as.list(fit3@model$mpars[fit3@model$midx[,4]==1,4])
  
  for(i in 1:100){
    if(i==1){
      presigma = matrix(tail(sigma(fit3), 2), ncol = 3)
      # arma = c(2,1) therefore need 2 lags
      prereturns = matrix(unlist(Dat[(T-1):T, ]), ncol = 3, nrow = 2)
      preresiduals = matrix(tail(residuals(fit3),2), ncol = 3, nrow = 2)
      preR = last(rcor(fit3))[,,1]
      diag(preR) = 1
      preQ = fit3@mfit$Qt[[length(fit3@mfit$Qt)]]
      preZ = tail(fit3@mfit$Z, 1)
      tmp = cgarchfilter(specx, Dat[1:(T+1), ], filter.control = list(n.old = T), varcoef = fit3@model$varcoef)
      filtMu[i,] = tail(fitted(tmp), 1)
      filtS[i,] = tail(sigma(tmp), 1)
      filtC[,,i] = last(rcov(tmp))[,,1]
      filtCor[,,i] = last(rcor(tmp))[,,1]
    } else{
      presigma = matrix(tail(sigma(tmp), 2), ncol = 3)
      # arma = c(2,1) therefore need 2 lags
      prereturns = matrix(unlist(Dat[(T+i-2):(T+i-1), ]), ncol = 3, nrow = 2)
      preresiduals = matrix(tail(residuals(tmp),2), ncol = 3, nrow = 2)
      preR = last(rcor(tmp))[,,1]
      diag(preR) = 1
      preQ = tmp@mfilter$Qt[[length(tmp@mfilter$Qt)]]
      preZ = tail(tmp@mfilter$Z, 1)
      
      tmp = cgarchfilter(specx, Dat[1:(T+i), ], filter.control = list(n.old = T), varcoef = fit3@model$varcoef)			
      filtMu[i,] = tail(fitted(tmp), 1)
      filtS[i,] = tail(sigma(tmp), 1)
      filtC[,,i] = last(rcov(tmp))[,,1]
      filtCor[,,i] = last(rcor(tmp))[,,1]
    }
    sim3 = cgarchsim(fit3, n.sim = 1, m.sim = 2000, startMethod = "sample", preR = preR, preQ = preQ, preZ = preZ,
                     prereturns = prereturns, presigma = presigma, preresiduals = preresiduals, cluster = cluster)
    simx = t(sapply(sim3@msim$simX, FUN = function(x) x[1,]))
    simMu[i,] = colMeans(simx)
    # Note: There is no uncertainty for the 1-ahead simulation of cov adn cor
    simC[,,i] = sim3@msim$simH[[1]][,,1]
    simCor[,,i] = sim3@msim$simR[[1]][,,1]
    simS[i,] = sqrt(diag(simC[,,i]))
    
    print(i)
    # CHECK:
    # X[t+1] = mu[t+1] + e[t+1]
    # sim3@msim$simX[[i]][1,] - sim3@msim$simRes[[i]][1,]
    # is equal to filtMu[i,]
    if(i < 3){
      print(all.equal(sim3@msim$simX[[2]][1,] - (sim3@msim$simZ[,,2]*sqrt(diag(sim3@msim$simH[[2]][,,1]))), 
                      filtMu[i,]))
      print(all.equal(sim3@msim$simX[[2000]][1,] - (sim3@msim$simZ[,,2000]*sqrt(diag(sim3@msim$simH[[2000]][,,1]))), 
                      filtMu[i,]))
    }
  }
  
  
  postscript("test3f2.eps", width = 10, height = 8)
  par(mfrow = c(2,2))
  plot(simMu[,1], type = "l", main = "AA Conditional Mean\nRolling Forecast",
       ylab = "mu", xlab = "Time")
  lines(filtMu[,1], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simMu[,2], type = "l", main = "AXP Conditional Mean\nRolling Forecast",
       ylab = "mu", xlab = "Time")
  lines(filtMu[,2], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simMu[,3], type = "l", main = "BA Conditional Mean\nRolling Forecast",
       ylab = "mu", xlab = "Time")
  lines(filtMu[,3], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  dev.off()
  
  postscript("test3f3.eps", width = 10, height = 8)
  par(mfrow = c(2,2))
  plot(simS[,1], type = "l", main = "AA Conditional Sigma\nRolling Forecast",
       ylab = "sigma", xlab = "Time")
  lines(filtS[,1], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simS[,2], type = "l", main = "AXP Conditional Sigma\nRolling Forecast",
       ylab = "sigma", xlab = "Time")
  lines(filtS[,2], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simS[,3], type = "l", main = "BA Conditional Sigma\nRolling Forecast",
       ylab = "sigma", xlab = "Time")
  lines(filtS[,3], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  dev.off()
  
  postscript("test3f4.eps", width = 10, height = 8)
  par(mfrow = c(2,2))
  plot(simC[1,2,], type = "l", main = "AA-AXP Conditional Covariance\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtC[1,2,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simC[1,3,], type = "l", main = "AA-BA Conditional Covariance\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtC[1,3,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  
  plot(simC[2,3,], type = "l", main = "AXP-BA Conditional Covariance\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtC[2,3,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  dev.off()
  
  postscript("test3f5.eps", width = 10, height = 8)
  par(mfrow = c(2,2))
  plot(simCor[1,2,], type = "l", main = "AA-AXP Conditional Correlation\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtCor[1,2,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simCor[1,3,], type = "l", main = "AA-BA Conditional Correlation\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtCor[1,3,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  
  plot(simCor[2,3,], type = "l", main = "AXP-BA Conditional Correlation\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtCor[2,3,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  dev.off()
  
  
  # n-ahead forecast exercise
  # when m.sim == 1, no need to supply any pre-data since the startMethod 
  # indicates that we extract the last values from the fit object
  
  sim4 = cgarchsim(fit3, n.sim = 1000, n.start = 500, m.sim = 100, startMethod = "sample", cluster = cluster)
  
  mR = matrix(0, nrow = 100, ncol = 3)
  for(i in 1:100){
    rc = rcor(sim4, sim = i)
    meanR = matrix(0, 3, 3)
    for(j in 1:1000) meanR = meanR + rc[,,j]
    # simulated average correlation
    meanR = meanR/1000
    mR[i,] = c(meanR[1,2], meanR[1,3], meanR[2,3])
    #print(i)
  }
  UQ = fit3@mfit$Qbar*(1-sum(coef(fit3, "dcc")))
  UR = UQ/(sqrt(diag(UQ)) %*% t(sqrt(diag(UQ)) ))
  
  postscript("test3f6.eps", width = 10, height = 8)
  par(mfrow = c(2,2))
  hist(mR[,1], main = "AA-AXP Simulated \nUnconditional Correlation", xlab = "cor", cex.main=0.8)
  abline(v = UR[1,2], col = "orange", lwd = 2)
  legend("topright", "Unconditional \n(Analytical)", col = "orange", lwd=2, bty = "n", 
         cex = 0.7)
  
  hist(mR[,2], main = "AA-BA Simulated \nUnconditional Correlation", xlab = "cor", cex.main=0.8)
  abline(v = UR[1,3], col = "orange", lwd = 2)
  legend("topright", "Unconditional \n(Analytical)", col = "orange", lwd=2, bty = "n", 
         cex = 0.7)
  
  hist(mR[,3], main = "AXP-BA Simulated \nUnconditional Correlation", xlab = "cor", cex.main=0.8)
  abline(v = UR[2,3], col = "orange", lwd = 2)
  legend("topright", "Unconditional \n(Analytical)", col = "orange", lwd=2, bty = "n", 
         cex = 0.7)
  dev.off()
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}

## Simulation dynamic copula (empirical with ARMA and DCC asymmetric and MVT)
rmgarch.test3g = function(cluster = NULL)
{
  tic = Sys.time()
  data(dji30retw)
  Dat = dji30retw[, 1:3, drop = FALSE]
  
  
  # GARCH-Normal MVN
  uspec = ugarchspec(mean.model = list(armaOrder = c(2,1)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "jsu")
  spec1 = cgarchspec(uspec = multispec( replicate(3, uspec) ), VAR = FALSE, robust = FALSE, lag = 2, lag.max = NULL, 
                     lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL,
                     robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
                     dccOrder = c(1,1), asymmetric = TRUE, distribution.model = list(copula = c("mvnorm", "mvt")[2], 
                                                                                     method = c("Kendall", "ML")[2], time.varying = TRUE,
                                                                                     transformation = c("parametric", "empirical", "spd")[2]))
  
  fit1 = cgarchfit(spec1, data = Dat, cluster = cluster,
                   fit.control = list(eval.se=FALSE))
  
  
  sim1 = cgarchsim(fit1, n.sim = 1000, m.sim = 10, startMethod = "sample")
  sim2 = cgarchsim(fit1, n.sim = 2, m.sim = 1000, startMethod = "sample", cluster = cluster)
  # 2-ahead mean covariance (1-ahead has no uncertainty).
  meanH = matrix(0,  3, 3)
  for(i in 1:1000) meanH = meanH + sim2@msim$simH[[i]][,,2]
  meanH = meanH/1000
  ebars = matrix(NA, ncol = 3, nrow = 1000)
  ebars[,1] = sapply(sim2@msim$simH, FUN = function(x) x[1,2,2])
  ebars[,2] = sapply(sim2@msim$simH, FUN = function(x) x[1,3,2])
  ebars[,3] = sapply(sim2@msim$simH, FUN = function(x) x[2,3,2])
  
  postscript("test3g1.eps", width = 10, height = 8)
  boxplot(ebars, notch = TRUE, names = c("H12", "H13", "H23"), main = "Simulated 2-ahead Covariance",
          col = 2:4)
  dev.off()
  
  
  # 1-ahead rolling forecast exercise
  fit3 = cgarchfit(spec1, data = Dat, out.sample = 100, cluster = cluster,
                   fit.control = list(eval.se=FALSE))
  T = dim(Dat)[1]-100
  simMu = simS = filtMu = filtS = matrix(NA, ncol = 3, nrow = 25)
  simCor = simC = filtC = filtCor = array(NA, dim = c(3,3,25))
  colSd = function(x) apply(x, 2, "sd")
  specx = spec1
  for(i in 1:3) specx@umodel$fixed.pars[[i]] = as.list(fit3@model$mpars[fit3@model$midx[,i]==1,i])
  setfixed(specx)<-as.list(fit3@model$mpars[fit3@model$midx[,4]==1,4])
  
  for(i in 1:25){
    if(i==1){
      presigma = matrix(tail(sigma(fit3), 2), ncol = 3)
      # arma = c(2,1) therefore need 2 lags
      prereturns = matrix(unlist(Dat[(T-1):T, ]), ncol = 3, nrow = 2)
      preresiduals = matrix(tail(residuals(fit3),2), ncol = 3, nrow = 2)
      preR = last(rcor(fit3))[,,1]
      diag(preR) = 1
      preQ = fit3@mfit$Qt[[length(fit3@mfit$Qt)]]
      preZ = tail(fit3@mfit$Z, 1)
      tmp = cgarchfilter(specx, Dat[1:(T+1), ], filter.control = list(n.old = T))
      filtMu[i,] = tail(fitted(tmp), 1)
      filtS[i,] = tail(sigma(tmp), 1)
      filtC[,,i] = last(rcov(tmp))[,,1]
      filtCor[,,i] = last(rcor(tmp))[,,1]
    } else{
      presigma = matrix(tail(sigma(tmp), 2), ncol = 3)
      # arma = c(2,1) therefore need 2 lags
      prereturns = matrix(unlist(Dat[(T+i-2):(T+i-1), ]), ncol = 3, nrow = 2)
      preresiduals = matrix(tail(residuals(tmp),2), ncol = 3, nrow = 2)
      preR = last(rcor(tmp))[,,1]
      diag(preR) = 1
      preQ = tmp@mfilter$Qt[[length(tmp@mfilter$Qt)]]
      preZ = tail(tmp@mfilter$Z, 1)
      
      tmp = cgarchfilter(specx, Dat[1:(T+i), ], filter.control = list(n.old = T))			
      filtMu[i,] = tail(fitted(tmp), 1)
      filtS[i,] = tail(sigma(tmp), 1)
      filtC[,,i] = last(rcov(tmp))[,,1]
      filtCor[,,i] = last(rcor(tmp))[,,1]
    }
    sim3 = cgarchsim(fit3, n.sim = 1, m.sim = 2000, startMethod = "sample", preR = preR, preQ = preQ, preZ = preZ,
                     prereturns = prereturns, presigma = presigma, preresiduals = preresiduals)
    simx = t(sapply(sim3@msim$simX, FUN = function(x) x[1,]))
    simMu[i,] = colMeans(simx)
    # Note: There is no uncertainty for the 1-ahead simulation of cov
    simC[,,i] = sim3@msim$simH[[1]][,,1]
    simS[i,] = sqrt(diag(simC[,,i]))
    simCor[,,i] = sim3@msim$simR[[1]][,,1]
    print(i)
    # CHECK:
    # X[t+1] = mu[t+1] + e[t+1]
    # sim3@msim$simX[[i]][1,] - sim3@msim$simRes[[i]][1,]
    # is equal to filtMu[i,]
    if(i < 4 ){
      print(all.equal(sim3@msim$simX[[2]][1,] - (sim3@msim$simZ[,,2]*sqrt(diag(sim3@msim$simH[[2]][,,1]))), 
                      filtMu[i,]))
      print(all.equal(sim3@msim$simX[[2000]][1,] - (sim3@msim$simZ[,,2000]*sqrt(diag(sim3@msim$simH[[2000]][,,1]))), 
                      filtMu[i,]))
    }
  }
  
  
  postscript("test3g2.eps", width = 10, height = 8)
  par(mfrow = c(2,2))
  plot(simMu[,1], type = "l", main = "AA Conditional Mean\nRolling Forecast",
       ylab = "mu", xlab = "Time")
  lines(filtMu[,1], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simMu[,2], type = "l", main = "AXP Conditional Mean\nRolling Forecast",
       ylab = "mu", xlab = "Time")
  lines(filtMu[,2], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simMu[,3], type = "l", main = "BA Conditional Mean\nRolling Forecast",
       ylab = "mu", xlab = "Time")
  lines(filtMu[,3], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  dev.off()
  
  postscript("test3g3.eps", width = 10, height = 8)
  par(mfrow = c(2,2))
  plot(simS[,1], type = "l", main = "AA Conditional Sigma\nRolling Forecast",
       ylab = "sigma", xlab = "Time")
  lines(filtS[,1], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simS[,2], type = "l", main = "AXP Conditional Sigma\nRolling Forecast",
       ylab = "sigma", xlab = "Time")
  lines(filtS[,2], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simS[,3], type = "l", main = "BA Conditional Sigma\nRolling Forecast",
       ylab = "sigma", xlab = "Time")
  lines(filtS[,3], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  dev.off()
  
  postscript("test3g4.eps", width = 10, height = 8)
  par(mfrow = c(2,2))
  plot(simC[1,2,], type = "l", main = "AA-AXP Conditional Covariance\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtC[1,2,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simC[1,3,], type = "l", main = "AA-BA Conditional Covariance\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtC[1,3,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  
  plot(simC[2,3,], type = "l", main = "AXP-BA Conditional Covariance\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtC[2,3,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  dev.off()
  
  postscript("test3g5.eps", width = 10, height = 8)
  par(mfrow = c(2,2))
  plot(simCor[1,2,], type = "l", main = "AA-AXP Conditional Correlation\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtCor[1,2,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  plot(simCor[1,3,], type = "l", main = "AA-BA Conditional Correlation\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtCor[1,3,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  
  plot(simCor[2,3,], type = "l", main = "AXP-BA Conditional Correlation\nRolling Forecast",
       ylab = "cov", xlab = "Time")
  lines(filtCor[2,3,], col = 2, lty = 2)
  legend("topleft", legend =c("Simulated", "Filtered"), col = 1:2, lty = 1:2)
  dev.off()
  
  
  # n-ahead forecast exercise
  # when m.sim == 1, no need to supply any pre-data since the startMethod 
  # indicates that we extract the last values from the fit object
  
  sim4 = cgarchsim(fit3, n.sim = 2000, n.start = 500, m.sim = 200, startMethod = "sample", cluster = cluster)
  
  mR = matrix(0, nrow = 200, ncol = 3)
  for(i in 1:200){
    rc = rcor(sim4, sim = i)
    meanR = matrix(0, 3, 3)
    for(j in 1:2000) meanR = meanR + rc[,,j]
    # simulated average correlation
    meanR = meanR/2000
    mR[i,] = c(meanR[1,2], meanR[1,3], meanR[2,3])
    #print(i)
  }
  UQ = fit3@mfit$Qbar*(1-sum(coef(fit3, "dcc")[1:2])) - fit3@mfit$Nbar*coef(fit3, "dcc")[3]
  UR = UQ/(sqrt(diag(UQ)) %*% t(sqrt(diag(UQ)) ))
  
  postscript("test3g6.eps", width = 10, height = 8)
  par(mfrow = c(2,2))
  hist(mR[,1], main = "AA-AXP Simulated \nUnconditional Correlation", xlab = "cor", cex.main=0.8)
  abline(v = UR[1,2], col = "orange", lwd = 2)
  legend("topright", "Unconditional \n(Analytical)", col = "orange", lwd=2, bty = "n", 
         cex = 0.7)
  
  hist(mR[,2], main = "AA-BA Simulated \nUnconditional Correlation", xlab = "cor", cex.main=0.8)
  abline(v = UR[1,3], col = "orange", lwd = 2)
  legend("topright", "Unconditional \n(Analytical)", col = "orange", lwd=2, bty = "n", 
         cex = 0.7)
  
  hist(mR[,3], main = "AXP-BA Simulated \nUnconditional Correlation", xlab = "cor", cex.main=0.8)
  abline(v = UR[2,3], col = "orange", lwd = 2)
  legend("topright", "Unconditional \n(Analytical)", col = "orange", lwd=2, bty = "n", 
         cex = 0.7)
  dev.off()
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}

rmgarch.test3h = function(cluster = NULL){
  # DCC Test
  tic = Sys.time()
  
  data(dji30retw)
  test = DCCtest(Data = dji30retw, garchOrder = c(1,1), n.lags = 1, solver = "solnp", 
                 solver.control = list(), cluster = cluster, Z = NULL)
  
  options(width = 120)
  zz <- file("test3h1.txt", open="wt")
  sink(zz)
  print(test)
  sink(type="message")
  sink()
  close(zz)
  
  # Independent Sample (reject correlation)
  spec = ugarchspec(mean.model = list(armaOrder=c(0,0), include.mean=FALSE))
  setfixed(spec)<-list(omega=6e-06, alpha1 = 0.04, beta1 = 0.9)
  sim = ugarchpath(spec, m.sim = 10, n.sim = 1000)
  X = fitted(sim)
  rownames(X)<-NULL
  test = DCCtest(Data = as.matrix(X), garchOrder = c(1,1), n.lags = 1, solver = "solnp", 
                 solver.control = list(), cluster = cluster, Z = NULL)
  
  options(width = 120)
  zz <- file("test3h2.txt", open="wt")
  sink(zz)
  print(test)
  sink(type="message")
  sink()
  close(zz)
  
  toc = Sys.time()-tic
  cat("Elapsed:", toc, "\n")
  return(toc)
}
