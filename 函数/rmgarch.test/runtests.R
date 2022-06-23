## source files' folder
## https://bitbucket.org/alexiosg/rmgarch/src/ce811acd0c7b/inst/rmgarch.tests/?at=beta

## source file
## https://bitbucket.org/alexiosg/rmgarch/src/ce811acd0c7bdda4a61dda2aba4d63c506987448/inst/rmgarch.tests/runtests.R?at=beta&fileviewer=file-view-default

#################################################################################
##
##   R package rmgarch by Alexios Ghalanos Copyright (C) 2008, 2009, 2010, 2011
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
.testattr = function(test = 1, subtest = "a"){
  
  if(is.na(match(test, 1:9))) stop("\nInvalid test number (valid range is 1:9)")
  alp = c("a", "b", "c", "d", "e", "f", "g", "h", "i")
  zt = match(subtest, alp)
  if(is.na(zt)) stop("\nInvalid subtest number (valid range is a:i)")
  
  testdesc = vector(mode = "list", length = 3)
  
  testdesc[[1]][1] = "GOGARCH: Fit Tests"
  testdesc[[1]][2] = "GOGARCH: Mean Spec Tests"
  testdesc[[1]][3] = "GOGARCH: ICA Tests"
  testdesc[[1]][4] = "GOGARCH: Filter Tests"
  testdesc[[1]][5] = "GOGARCH: Methods Tests I"
  testdesc[[1]][6] = "GOGARCH: Methods Tests II"
  testdesc[[1]][7] = "GOGARCH: Forecast Tests"
  testdesc[[1]][8] = "GOGARCH: Rolling Forecast Tests"
  testdesc[[1]][9] = "GOGARCH: Moment Tests"
  
  testdesc[[2]][1] = "DCC: Fit Tests"
  testdesc[[2]][2] = "DCC: Alternative Distribution Tests"
  testdesc[[2]][3] = "DCC: VAR-DCC Test"
  testdesc[[2]][4] = "DCC: Filter Tests"
  testdesc[[2]][5] = "DCC: Simulation Tests"
  testdesc[[2]][6] = "DCC: Forecast Tests"
  testdesc[[2]][7] = "DCC: Rolling Forecast Tests"
  
  testdesc[[3]][1] = "CGARCH: Fit and Filtering Tests"
  testdesc[[3]][2] = "CGARCH: Static Copula"
  testdesc[[3]][3] = "CGARCH: Filtering and Out-of-Sample Tests"
  testdesc[[3]][4] = "CGARCH: Static Copula Simulation Tests"
  testdesc[[3]][5] = "CGARCH: Dynamic Copula Simulation (Parametric) Tests"
  testdesc[[3]][6] = "CGARCH: Dynamic Copula Simulation (SPD) Tests"
  testdesc[[3]][7] = "CGARCH: Dynamic Copula Simulation (Empirical) Tests"
  
  
  if(is.na(testdesc[[test]][zt])) stop(paste("\nInvalid subtest number (valid range is a:", alp[length(testdesc[[test]])], ")", sep="")) 
  ans = paste(testdesc[[test]][zt], sep = "")
  return(ans)
}

rmgarch.runtests = function(test = 1, subtest = "a", wdir = getwd(), parallel = FALSE, 
                            parallel.control = list(pkg  = c("snowfall", "multicore"), cores = 2))
{
  tmp = try(setwd(wdir), silent = TRUE)
  if(inherits(tmp, "try-error")) stop("\nInvalid wdir...")
  tat = .testattr(test, subtest)
  cat("\n")
  cat(tat)
  cat("\n")
  tstname = paste("rmgarch.test", test, subtest,"(parallel =", parallel,", parallel.control=list(pkg='", parallel.control$pkg,"', cores = ",
                  parallel.control$cores,"))", sep="")
  ans = eval(parse(text=tstname))
  return(ans)
}

