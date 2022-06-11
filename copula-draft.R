dim(HiLoCl.base)
require('BBmisc', quietly = TRUE)
pkgs <- c('rugarch', 'rmgarch', 'forecast', 'plyr', 'dplyr', 'quantmod', 'matrixcalc', 'MASS')
suppressAll(lib(pkgs))
rm(pkgs)

Dat <- HiLoCl.base[,, drop = FALSE]
#Dat <- llply(mbase, Cl)
#Dat %<>% do.call('cbind', .)
#Dat <- do.call('cbind', list(Op(mbase[[7]]), Hi(mbase[[7]]), 
#                             Lo(mbase[[7]]), Cl(mbase[[7]])))
Dat %<>% .[,, drop = FALSE]

dateID <- index(Dat)# %>% .[. >= ymd('2013-01-01')]
out <- length(dateID) - which(dateID == ymd('2013-01-01'))
msm <- 100

#data(dji30retw) 
#Dat = dji30retw[, 1:9, drop = FALSE] 
armaorder <- llply(Dat, function(x) {
  arimaorder(auto.arima(x))
  })

model <- llply(armaorder, function(x) {
  ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1), 
                                 variance.targeting=FALSE), 
             mean.model=list(armaOrder=c(x[1],x[3]), include.mean=F, archm=F, 
                             archpow=1, arfima = TRUE), 
             fixed.pars = list(arfima = x[2]), distribution.model="norm")
  })
spec = cgarchspec(uspec = multispec(model), VAR = FALSE, robust = FALSE, 
                  lag.criterion = "AIC", external.regressors = NULL, 
                  dccOrder = c(1,1), asymmetric = TRUE, 
                  distribution.model = list(copula = "mvt", method = "ML", 
                                            time.varying = TRUE, 
                                            transformation = "spd"))
fit = cgarchfit(spec, data = Dat, out.sample = out, cluster = NULL, 
                spd.control = list(lower = 0.1, upper = 0.9, type = "mle", 
                                   kernel = "normal"), 
                fit.control = list(eval.se=FALSE))
T = dim(Dat)[1]-out
simMu = simS = filtMu = filtS = matrix(NA, ncol = ncol(Dat), nrow = out)
simCor = simC = filtC = filtCor = array(NA, dim = c(ncol(Dat), ncol(Dat), out))
colSd = function(x) apply(x, ncol(Dat), "sd")
specx = spec

for(i in 1:ncol(Dat)) {
  specx@umodel$fixed.pars[[i]] = as.list(fit@model$mpars[fit@model$midx[,i]==1,i])
  }
setfixed(specx)<-as.list(fit@model$mpars[fit@model$midx[,ncol(Dat)+1]==1,ncol(Dat)+1])
simulatedreturns <- array(dim=c(out, ncol(Dat), msm))

for(i in 1:out){
  if(i==1){
    presigma = matrix(tail(sigma(fit), 1), ncol = ncol(Dat))
    prereturns = matrix(unlist(Dat[T, ]), ncol = ncol(Dat), nrow = 1)
    preresiduals = matrix(tail(residuals(fit),1), ncol = ncol(Dat), nrow = 1)
    preR = rcor(fit)[,,1]
    diag(preR) = 1
    preQ = fit@mfit$Qt[[length(fit@mfit$Qt)]]
    preZ = tail(fit@mfit$Z, 1)
    tmp = cgarchfilter(specx, Dat[2:(T+1), ], filter.control = list(n.old = T))
    filtMu[i,] = tail(fitted(tmp), 1)
    filtS[i,] = tail(sigma(tmp), 1)
    filtC[,,i] = rcov(tmp)[,,1]
    filtCor[,,i] = rcor(tmp)[,,1]
  } else{
    presigma = matrix(tail(sigma(tmp), 1), ncol = ncol(Dat))
    prereturns = matrix(unlist(Dat[(T+i-1), ]), ncol = ncol(Dat), nrow = 1)
    preresiduals = matrix(tail(residuals(tmp),1), ncol = ncol(Dat), nrow = 1)
    preR = rcor(tmp)[,,1]
    diag(preR) = 1
    preQ = tmp@mfilter$Qt[[length(tmp@mfilter$Qt)]]
    preZ = tail(tmp@mfilter$Z, 1)
    
    tmp = cgarchfilter(specx, Dat[(i+1):(T+i), ], filter.control = list(n.old = T))
    filtMu[i,] = tail(fitted(tmp), 1)
    filtS[i,] = tail(sigma(tmp), 1)
    filtC[,,i] = rcov(tmp)[,,1]
    filtCor[,,i] = rcor(tmp)[,,1]
  }
  
  sim = cgarchsim(fit, n.sim = 1, m.sim = msm, startMethod = "sample", 
                  preR = preR, preQ = preQ, preZ = preZ, 
                  prereturns = prereturns, presigma = presigma, 
                  preresiduals = preresiduals, cluster = NULL)
  simx = t(sapply(sim@msim$simX, FUN = function(x) x[1,]))
  simMu[i,] = colMeans(simx, na.rm = TRUE)
  simC[,,i] = sim@msim$simH[[1]][,,1]
  simCor[,,i] = sim@msim$simR[[1]][,,1]
  simS[i,] = sqrt(diag(simC[,,i]), na.rm = TRUE)
  simulatedreturns[i,,]=simx
  }
#rm(tmp, simx, simMu, simC, simCor, simS, simulatedreturns)

## R语言多元COPULA GARCH 模型时间序列预测
## https://tecdat.cn/r%E8%AF%AD%E8%A8%80%E5%A4%9A%E5%85%83copula-garch-%E6%A8%A1%E5%9E%8B%E6%97%B6%E9%97%B4%E5%BA%8F%E5%88%97%E9%A2%84%E6%B5%8B
## 
## R语言ARMA-GARCH-COPULA模型和金融时间序列案例
## http://tecdat.cn/r%e8%af%ad%e8%a8%80copulas%e5%92%8c%e9%87%91%e8%9e%8d%e6%97%b6%e9%97%b4%e5%ba%8f%e5%88%97%e6%a1%88%e4%be%8b
## 




