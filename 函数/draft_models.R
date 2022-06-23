## Comparison among models
##
library('glmnet')
source('./function/h.R')

## gaussian
## daily mean 2 is the (opening price + closing price) / 2.
fitgs <- llply(seq(0, 10), function(i) {
  glmnet(x = h(LADDT, family = 'gaussian', xy.matrix = 'h2', setform = 'l4', 
               yv = 'daily.mean2')$x, 
         y = h(LADDT, family = 'gaussian', xy.matrix = 'h2', setform = 'l4', 
               yv = 'daily.mean2')$y, 
         family = 'gaussian', alpha = i/10)
  })

## poisson
## daily mean 2 is the (opening price + closing price) / 2.
fitps <- llply(seq(0, 10), function(i) {
  glmnet(x = h(LADDT, family = 'poisson', xy.matrix = 'h2', setform = 'l4', 
               yv = 'daily.mean2')$x, 
         y = h(LADDT, family = 'poisson', xy.matrix = 'h2', setform = 'l4', 
               yv = 'daily.mean2')$y, 
         family = 'poisson', alpha = i/10)
})

## binomial
## daily mean 2 is the (opening price + closing price) / 2.
fitbn <- llply(seq(0, 10), function(i) {
  glmnet(x = h(LADDT, family = 'binomial', xy.matrix = 'h2', setform = 'l4', 
           yv = 'daily.mean2')$x, 
         y = h(LADDT, family = 'binomial', xy.matrix = 'h2', setform = 'l4', 
           yv = 'daily.mean2')$y, 
         family = 'binomial', alpha = i/10)
  })

## multinomial
## daily mean 2 is the (opening price + closing price) / 2.
fitmn <- llply(seq(0, 10), function(i) {
  glmnet(x = h(LADDT, family = 'multinomial', xy.matrix = 'h2', setform = 'l4', 
           yv = 'daily.mean2')$x, 
         y = h(LADDT, family = 'multinomial', xy.matrix = 'h2', setform = 'l4', 
           yv = 'daily.mean2')$y, 
         family = 'binomial', alpha = i/10)
  })

## Mean Square Error
## Due to the stocks price is a continuous time linear models.
data.frame(
  gaum1 = ldply(fitgs, function(data) {
    mean((xy$y - predict(data, newx = xy$x, s = data$lambda.1se, pred.type = 'class'))^2)}), 
  
  gaum2 = ldply(fitgs, function(data) {
    mean((xy$y - predict(data, newx = xy$x, s = data$lambda.min, pred.type = 'class'))^2)}), 
  
  posm1 = ldply(fitps, function(data) {
    mean((xy$y - predict(data, newx = xy$x, s = data$lambda.1se, pred.type = 'class'))^2)}), 
  
  posm2 = ldply(fitps, function(data) {
    mean((xy$y - predict(data, newx = xy$x, s = data$lambda.min, pred.type = 'class'))^2)}), 
  
  bnmm1 = ldply(fitbn, function(data) {
    mean((xy$y - predict(data, newx = xy$x, s = data$lambda.1se, pred.type = 'class'))^2)}), 
  
  bnmm2 = ldply(fitbn, function(data) {
    mean((xy$y - predict(data, newx = xy$x, s = data$lambda.min, pred.type = 'class'))^2)}), 
  
  munm1 = ldply(fitmn, function(data) {
    mean((xy$y - predict(data, newx = xy$x, s = data$lambda.1se, pred.type = 'class'))^2)}), 
  
  munm2 = ldply(fitmn, function(data) {
    mean((xy$y - predict(data, newx = xy$x, s = data$lambda.min, pred.type = 'class'))^2)}))
#          V1      V1.1     V1.2     V1.3     V1.4     V1.5     V1.6     V1.7
#1  30.305394 30.305394 6728.770 6728.770 7461.911 7461.911 7461.911 7461.911
#2  12.607816 12.607816 6728.640 6728.640 7459.746 7459.746 7459.746 7459.746
#3  10.697834 10.697834 6728.645 6728.645 7456.854 7456.854 7456.854 7456.854
#4   9.966396  9.966396 6728.644 6728.644 7454.339 7454.339 7454.339 7454.339
#5   9.584780  9.584780 6728.645 6728.645 7452.062 7452.062 7452.062 7452.062
#6   9.264099  9.264099 6728.646 6728.646 7449.944 7449.944 7449.944 7449.944
#7   9.093308  9.093308 6728.647 6728.647 7447.973 7447.973 7447.973 7447.973
#8   9.015583  9.015583 6728.647 6728.647 7446.265 7446.265 7446.265 7446.265
#9   9.000147  9.000147 6728.647 6728.647 7445.366 7445.366 7445.366 7445.366
#10  9.252308  9.252308 6728.646 6728.646 7448.566 7448.566 7448.566 7448.566
#11  9.278999  9.278999 6728.648 6728.648 7482.474 7482.474 7482.474 7482.474

## futher works
# For my opinion :
# Due to the binomial and multinomial only count logical value which is 1 or 0, here we need to 
#   times the baseline stock price in order to make it valid.
# where : baseline = first(LADDT$LAD.Open)



## http://faculty.chicagobooth.edu/max.farrell/bus41100/week8-Rcode.R
## slide 21
## LASSO selection
## install.packages("glmnet") #just run this once (on each computer)
#'@ library(glmnet)
#'@ X <- model.matrix(formula(full), credit)
## need to subtract the intercept
#'@ X <- X[,-1]
# 
#'@ set.seed(1) #so cross-validation results are the same every time.
#'@ cvfit <- cv.glmnet(x = X[train,], y = credit$GoodCredit[train], family="binomial", alpha=1, standardize=TRUE)
## sometimes adding type.measure="auc" is useful for logistic regression, sometimes not
#'@ betas <- coef(cvfit, s = "lambda.1se")
#'@ model.1se <- which(betas[2:length(betas)]!=0)


