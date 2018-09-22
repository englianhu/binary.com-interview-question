dim(HiLoCl.base)
require('BBmisc', quietly = TRUE)
pkgs <- c('rugarch', 'rmgarch', 'forecast', 'plyr', 'dplyr', 'quantmod', 'matrixcalc', 'MASS')
suppressAll(lib(pkgs))
rm(pkgs)

md <- c('Kendall', 'ML')                     ## choose `Kendall`
trams <- c('parametric', 'empirical', 'spd') ## choose `parametric`
.dist.models <- c('mvnorm', 'mvt')           ##choose `mvt`
sv <- c('solnp')#, 'gosolnp')

.VAR = TRUE; .tram = trams[3]
.dist.model = .dist.models[2]

Dat <- HiLoCl.base[,, drop = FALSE]

dateID <- index(Dat)# %>% .[. >= ymd('2013-01-01')]
out <- length(dateID) - which(dateID == ymd('2013-01-01'))
msm <- 100

## .dist.model = 'mvt' since mvt produced most accurate outcome.
speclist <- filter_spec(Dat, .currency = 'JPY=X', .price_type = 'HLC', 
                        var.target = FALSE)
mspec <- multispec(speclist)

cSpec <- cgarchspec(
  mspec, VAR = .VAR, lag = 1, 
  lag.criterion = c('AIC', 'HQ', 'SC', 'FPE'), 
  external.regressors = NULL, #external.regressors = VAREXO, 
  dccOrder = c(1, 1), asymmetric = FALSE, ##wether use `aDCC` or normal `DCC` model.
  distribution.model = list(
    time.varying = TRUE, copula = .dist.model, 
    method = .model, transformation = .tram), 
  start.pars = list(), fixed.pars = list())

vfit = varxfit(X = Dat, p = 1, exogen = NULL, robust = FALSE, 
               gamma = 0.25, delta = 0.01, nc = 10, ns = 500, 
               postpad = 'constant')

fit <- cgarchfit(cSpec, data = Dat, solver = .solver, cluster = cl, 
                 VAR.fit = vfit)

fc <- varxforecast(X = Dat, Bcoef = fit@mfit$stdresid, p = 4, 
                   out.sample = msm, n.ahead = .ahead, n.roll = 0, mregfor = NULL)

