## ========= Load Packages =================================
suppressPackageStartupMessages(library("BBmisc"))
suppressAll(library('devtools'))
suppressAll(library('lubridate'))
suppressAll(library('plyr'))
suppressAll(library('stringr'))
suppressAll(library('magrittr'))
suppressAll(library('dplyr'))
suppressAll(library('tidyr'))
suppressAll(library('readr'))
suppressAll(library('tidyverse')) #load c(dplyr, tidyr, stringr, readr) due to system doesn't work.
suppressAll(library("shiny"))
suppressAll(library("shinyjs"))
suppressAll(library('shinyBS'))
suppressAll(library('DT'))
suppressAll(library('quantmod'))
suppressAll(library('formattable'))
suppressAll(library('highcharter'))
suppressAll(library('PerformanceAnalytics'))
suppressAll(library('memoise'))
suppressAll(source('./function/glmPrice.R', local = TRUE))

## =================================== Read Data =====================================
tryCatch({
  suppressAll(getSymbols('LAD', from = '2015-01-01'))
  if(exists('LAD')) saveRDS(LAD, file = './data/LAD.rds')
}, error = function(e) LAD <- read_rds(path = './data/LAD.rds'))

if(!exists('LAD')) {
  if(readRDS('./data/LAD.rds') %>% attributes %>% .$updated) {
    
  }
  LAD <- read_rds(path = './data/LAD.rds')
}

LADDT <- LAD %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
  tbl_df %>% mutate(Date = ymd(Date))#, 
#LAD.Volume = formattable::digits(
#LAD.Volume, 0, format = 'd', big.mark = ','))
dateID <- LADDT$Date

## ============================== Regression Models ==================================
## 
## https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
## 
## --------------------------- Setting Variable --------------------------------------
xy.matrix = 'h1'        #xy.matrix %in% c('h1', 'h2')
maxit = 1000            #1 < maxit < Inf
parallel = TRUE         #parallel is logical vaiable
weight.date = FALSE     #weight.date is ...
weight.volume = FALSE   #weight.volume is ...
pred.type = 'class'     #pred.type %in% c('link', 'response', 'coefficients', 'nonzero', 'class')
nfolds = 10

## args(glmPrice) setting for all models.
#'@ mbase, family = 'gaussian', xy.matrix = 'h1', alpha = 0:10, 
#'@ yv = 'daily.mean', tmeasure = 'deviance', tmultinomial = 'grouped', 
#'@ maxit = 1000, pred.type = 'class', nfolds = 10, foldid = NULL, 
#'@ s = 'lambda.min', weight.date = FALSE, weight.volume = FALSE, 
#'@ parallel = TRUE, .log = FALSE
## 

## ------------------ 1. Linear Regression : Gaussian --------------------------------
## ------ 1. 1A gaussian (daily.mean + deviance + lambda.min) ------------------------
lm1AAA <- glmPrice(LADDT, family = 'gaussian', xy.matrix = xy.matrix, yv = 'daily.mean', 
                   tmeasure = 'deviance', maxit = maxit, nfolds = 10, s = 'lambda.min', 
                   weight.date = weight.date, weight.volume = weight.volume, parallel = parallel)

## ------ 2. 1B gaussian (daily.mean + deviance + lambda.1se) ------------------------
lm1AAB <- glmPrice(LADDT, family = 'gaussian', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 3. 2A gaussian (daily.mean + mse + lambda.min) ------------------------
lm1ABA <- glmPrice(LADDT, family = 'gaussian', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 4. 2B gaussian (daily.mean + mse + lambda.1se) ------------------------
lm1ABB <- glmPrice(LADDT, family = 'gaussian', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 5. 3A gaussian (daily.mean + mae + lambda.min) ------------------------
lm1ACA <- glmPrice(LADDT, family = 'gaussian', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 6. 3B gaussian (daily.mean + mae + lambda.1se) ------------------------
lm1ACB <- glmPrice(LADDT, family = 'gaussian', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 7. 4A gaussian (baseline + deviance + lambda.min) ------------------------
lm1BAA <- glmPrice(LADDT, family = 'gaussian', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 8. 4B gaussian (baseline + deviance + lambda.1se) ------------------------
lm1BAB <- glmPrice(LADDT, family = 'gaussian', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 9. 5A gaussian (baseline + mse + lambda.min) ------------------------
lm1BBA <- glmPrice(LADDT, family = 'gaussian', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 10. 5B gaussian (baseline + mse + lambda.1se) ------------------------
lm1BBB <- glmPrice(LADDT, family = 'gaussian', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 11. 6A gaussian (baseline + mae + lambda.min) ------------------------
lm1BCA <- glmPrice(LADDT, family = 'gaussian', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 12. 6B gaussian (baseline + mae + lambda.1se) ------------------------
lm1BCB <- glmPrice(LADDT, family = 'gaussian', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 13. 7A gaussian (mixed + deviance + lambda.min) ------------------------
lm1CAA <- glmPrice(LADDT, family = 'gaussian', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 14. 7B gaussian (mixed + deviance + lambda.1se) ------------------------
lm1CAB <- glmPrice(LADDT, family = 'gaussian', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 15. 8A gaussian (mixed + mse + lambda.min) ------------------------
lm1CBA <- glmPrice(LADDT, family = 'gaussian', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 16. 8B gaussian (mixed + mse + lambda.1se) ------------------------
lm1CBB <- glmPrice(LADDT, family = 'gaussian', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 17. 9A gaussian (mixed + mae + lambda.min) ------------------------
lm1CCA <- glmPrice(LADDT, family = 'gaussian', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 18. 9B gaussian (mixed + mae + lambda.1se) ------------------------
lm1CCB <- glmPrice(LADDT, family = 'gaussian', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ----------------- 2. Logistic Regression : Binomial --------------------------------
## ------ 19. 1A binomial (daily.mean + deviance + lambda.min) ------------------------
lm2AAA <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 20. 1B binomial (daily.mean + deviance + lambda.1se) ------------------------
lm2AAB <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 21. 2A binomial (daily.mean + mse + lambda.min) ------------------------
lm2ABA <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 22. 2B binomial (daily.mean + mse + lambda.1se) ------------------------
lm2ABB <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 23. 3A binomial (daily.mean + mae + lambda.min) ------------------------
lm2ACA <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 24. 3B binomial (daily.mean + mae + lambda.1se) ------------------------
lm2ACB <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 25. 4A binomial (daily.mean + class + lambda.min) ------------------------
lm2ADA <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'class', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 26. 4B binomial (daily.mean + class + lambda.1se) ------------------------
lm2ADB <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'class', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 27. 5A binomial (daily.mean + auc + lambda.min) ------------------------
lm2AEA <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'auc', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 28. 5B binomial (daily.mean + auc + lambda.1se) ------------------------
lm2AEB <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'auc', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 29. 6A binomial (baseline + deviance + lambda.min) ------------------------
lm2BAA <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 30. 6B binomial (baseline + deviance + lambda.1se) ------------------------
lm2BAB <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 31. 7A binomial (baseline + mse + lambda.min) ------------------------
lm2BBA <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 32. 7B binomial (baseline + mse + lambda.1se) ------------------------
lm2BBB <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 33. 8A binomial (baseline + mae + lambda.min) ------------------------
lm2BCA <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 34. 8B binomial (baseline + mae + lambda.1se) ------------------------
lm2BCB <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 35. 9A binomial (baseline + class + lambda.min) ------------------------
lm2BDA <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'class', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 36. 9B binomial (baseline + class + lambda.1se) ------------------------
lm2BDB <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'class', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 37. 10A binomial (baseline + auc + lambda.min) ------------------------
lm2BEA <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'auc', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 38. 10B binomial (baseline + auc + lambda.1se) ------------------------
lm2BEB <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'auc', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 39. 11A binomial (mixed + deviance + lambda.min) ------------------------
lm2CAA <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 40. 11B binomial (mixed + deviance + lambda.1se) ------------------------
lm2CAB <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 41. 12A binomial (mixed + mse + lambda.min) ------------------------
lm2CBA <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 42. 12B binomial (mixed + mse + lambda.1se) ------------------------
lm2CBB <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 43. 13A binomial (mixed + mae + lambda.min) ------------------------
lm2CCA <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 44. 13B binomial (mixed + mae + lambda.1se) ------------------------
lm2CCB <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 45. 14A binomial (mixed + class + lambda.min) ------------------------
lm2CDA <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'class', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 46. 14B binomial (mixed + class + lambda.1se) ------------------------
lm2CDB <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'class', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 47. 15A binomial (mixed + auc + lambda.min) ------------------------
lm2CEA <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'auc', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 48. 15B binomial (mixed + auc + lambda.1se) ------------------------
lm2CEB <- glmPrice(LADDT, family = 'binomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'auc', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## -------------------------------- 3. Poisson --------------------------------------
## ------ 49. 1A poisson (daily.mean + deviance + lambda.min) ------------------------
lm3AAA <- glmPrice(LADDT, family = 'poisson', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 50. 1B poisson (daily.mean + deviance + lambda.1se) ------------------------
lm3AAB <- glmPrice(LADDT, family = 'poisson', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 51. 2A poisson (daily.mean + mse + lambda.min) ------------------------
lm3ABA <- glmPrice(LADDT, family = 'poisson', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 52. 2B poisson (daily.mean + mse + lambda.1se) ------------------------
lm3ABB <- glmPrice(LADDT, family = 'poisson', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 53. 3A poisson (daily.mean + mae + lambda.min) ------------------------
lm3ACA <- glmPrice(LADDT, family = 'poisson', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 54. 3B poisson (daily.mean + mae + lambda.1se) ------------------------
lm3ACB <- glmPrice(LADDT, family = 'poisson', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 55. 4A poisson (baseline + deviance + lambda.min) ------------------------
lm3BAA <- glmPrice(LADDT, family = 'poisson', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 56. 4B poisson (baseline + deviance + lambda.1se) ------------------------
lm3BAB <- glmPrice(LADDT, family = 'poisson', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 57. 5A poisson (baseline + mse + lambda.min) ------------------------
lm3BBA <- glmPrice(LADDT, family = 'poisson', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 58. 5B poisson (baseline + mse + lambda.1se) ------------------------
lm3BBB <- glmPrice(LADDT, family = 'poisson', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 59. 6A poisson (baseline + mae + lambda.min) ------------------------
lm3BCA <- glmPrice(LADDT, family = 'poisson', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 60. 6B poisson (baseline + mae + lambda.1se) ------------------------
lm3BCB <- glmPrice(LADDT, family = 'poisson', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 61. 7A poisson (mixed + deviance + lambda.min) ------------------------
lm3CAA <- glmPrice(LADDT, family = 'poisson', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 62. 7B poisson (mixed + deviance + lambda.1se) ------------------------
lm3CAB <- glmPrice(LADDT, family = 'poisson', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 63. 8A poisson (mixed + mse + lambda.min) ------------------------
lm3CBA <- glmPrice(LADDT, family = 'poisson', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 64. 8B poisson (mixed + mse + lambda.1se) ------------------------
lm3CBB <- glmPrice(LADDT, family = 'poisson', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 65. 9A poisson (mixed + mae + lambda.min) ------------------------
lm3CCA <- glmPrice(LADDT, family = 'poisson', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 66. 9B poisson (mixed + mae + lambda.1se) ------------------------
lm3CCB <- glmPrice(LADDT, family = 'poisson', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ---------------- 4. Logistic Regression : Multinomial --------------------------------
## ------ 67. 1AA multinomial (daily.mean + deviance + grouped + lambda.min) ------------
lm4AAAA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'deviance', 
                    maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'grouped', parallel = parallel)

## ------ 68. 1AB multinomial (daily.mean + deviance + grouped + lambda.1se) ------------
lm4AAAB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'deviance', 
                    maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'grouped', parallel = parallel)

## ------ 69. 1BA multinomial (daily.mean + deviance + ungrouped + lambda.min) ----------
lm4AABA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'deviance', 
                    maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 70. 1BB multinomial (daily.mean + deviance + ungrouped + lambda.1se) ----------
lm4AABB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'deviance', 
                    maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 71. 2AA multinomial (daily.mean + mse + grouped + lambda.min) ------------------
lm4ABAA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 72. 2AB multinomial (daily.mean + mse + grouped + lambda.1se) ------------------
lm4ABAB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 73. 2BA multinomial (daily.mean + mse + ungrouped + lambda.min) ----------
lm4ABBA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mse', 
                    maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 74. 2BB multinomial (daily.mean + mse + ungrouped + lambda.1se) ----------
lm4ABBB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mse', 
                    maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 75. 3AA multinomial (daily.mean + mae + grouped + lambda.min) ------------------------
lm4ACAA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 76. 3AB multinomial (daily.mean + mae + grouped + lambda.1se) ------------------------
lm4ACAB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 77. 3BA multinomial (daily.mean + mae + ungrouped + lambda.min) ----------
lm4ACBA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mae', 
                    maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 78. 3BB multinomial (daily.mean + mae + ungrouped + lambda.1se) ----------
lm4ACBB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'mae', 
                    maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 79. 4AA multinomial (daily.mean + class + grouped + lambda.min) ------------------------
lm4ADAA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'class', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 80. 4AB multinomial (daily.mean + class + grouped + lambda.1se) ------------------------
lm4ADAB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'class', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 81. 4BA multinomial (daily.mean + class + ungrouped + lambda.min) ----------
lm4ADBA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'class', 
                    maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 82. 4BB multinomial (daily.mean + class + ungrouped + lambda.1se) ----------
lm4ADBB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'daily.mean', tmeasure = 'class', 
                    maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 83. 6AA multinomial (baseline + deviance + grouped + lambda.min) ------------------------
lm4BAAA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 84. 6AB multinomial (baseline + deviance + grouped + lambda.1se) ------------------------
lm4BAAB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 85. 6BA multinomial (baseline + deviance + ungrouped + lambda.min) ----------
lm4BABA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'deviance', 
                    maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 86. 6BB multinomial (baseline + deviance + ungrouped + lambda.1se) ----------
lm4BABB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'deviance', 
                    maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 87. 7AA multinomial (baseline + mse + grouped + lambda.min) ------------------------
lm4BBAA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 88. 7AB multinomial (baseline + mse + grouped + lambda.1se) ------------------------
lm4BBAB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 89. 7BA multinomial (baseline + mse + ungrouped + lambda.min) ----------
lm4BBBA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mse', 
                    maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 90. 7BB multinomial (baseline + mse + ungrouped + lambda.1se) ----------
lm4BBBB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mse', 
                    maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 91. 8AA multinomial (baseline + mae + grouped + lambda.min) ------------------------
lm4BCAA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 92. 8AB multinomial (baseline + mae + grouped + lambda.1se) ------------------------
lm4BCAB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 93. 8BA multinomial (baseline + mae + ungrouped + lambda.min) ----------
lm4BCBA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mae', 
                    maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 94. 8BB multinomial (baseline + mae + ungrouped + lambda.1se) ----------
lm4BCBB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'mae', 
                    maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 95. 9AA multinomial (baseline + class + grouped + lambda.min) ------------------------
lm4BDAA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'class', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 96. 9AB multinomial (baseline + class + grouped + lambda.1se) ------------------------
lm4BDAB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'class', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 97. 9BA multinomial (baseline + class + ungrouped + lambda.min) ----------
lm4BDBA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'class', 
                    maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 98. 9BB multinomial (baseline + class + ungrouped + lambda.1se) ----------
lm4BDBB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'baseline', tmeasure = 'class', 
                    maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 99. 11AA multinomial (mixed + deviance + grouped + lambda.min) ------------------------
lm4CAAA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 100. 11AB multinomial (mixed + deviance + grouped + lambda.1se) ------------------------
lm4CAAB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'deviance', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 101. 11BA multinomial (mixed + deviance + ungrouped + lambda.min) ----------
lm4CABA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'deviance', 
                    maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 102. 11BB multinomial (mixed + deviance + ungrouped + lambda.1se) ----------
lm4CABB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'deviance', 
                    maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 103. 12AA multinomial (mixed + mse + grouped + lambda.min) ------------------------
lm4CBAA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 104. 12AB multinomial (mixed + mse + grouped + lambda.1se) ------------------------
lm4CBAB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mse', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 105. 12BA multinomial (mixed + mse + ungrouped + lambda.min) ----------
lm4CBBA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mse', 
                    maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 106. 12BB multinomial (mixed + mse + ungrouped + lambda.1se) ----------
lm4CBBB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mse', 
                    maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 107. 13AA multinomial (mixed + mae + grouped + lambda.min) ------------------------
lm4CCAA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 108. 13AB multinomial (mixed + mae + grouped + lambda.1se) ------------------------
lm4CCAB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mae', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 109. 13BA multinomial (mixed + mae + ungrouped + lambda.min) ----------
lm4CCBA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mae', 
                    maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 110. 13BB multinomial (mixed + mae + ungrouped + lambda.1se) ----------
lm4CCBB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'mae', 
                    maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 111. 14AA multinomial (mixed + class + grouped + lambda.min) ------------------------
lm4CDAA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'class', 
                   maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 112. 14AB multinomial (mixed + class + grouped + lambda.1se) ------------------------
lm4CDAB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'class', 
                   maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                   weight.volume = weight.volume, parallel = parallel)

## ------ 113. 14BA multinomial (mixed + class + ungrouped + lambda.min) ----------
lm4CDBA <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'class', 
                    maxit = maxit, nfolds = 10, s = 'lambda.min', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ------ 114. 14BB multinomial (mixed + class + ungrouped + lambda.1se) ----------
lm4CDBB <- glmPrice(LADDT, family = 'multinomial', xy.matrix = xy.matrix, yv = 'mixed', tmeasure = 'class', 
                    maxit = maxit, nfolds = 10, s = 'lambda.1se', weight.date = weight.date, 
                    weight.volume = weight.volume, tmultinomial = 'ungrouped', parallel = parallel)

## ============================== Comparison Models ==================================
suppressPackageStartupMessages(library('rlist'))

ls(envir = .GlobalEnv, pattern = "lm1")
compr <- paste(ls(envir = .GlobalEnv, pattern = "lm1"), collapse = ',') %>% 
  paste0('list(', ., ')') %>% parse(text = .) %>% eval

mmse <- llply(compr, function(x) x$mse)
names(mmse) <- ls(envir = .GlobalEnv, pattern = "lm1")
mmse %<>% ldply %>% tbl_df
filter(mmse, mse == min(mse))


###############################################################################################
## ============================== Modified Comparison Models ==================================
###############################################################################################
suppressPackageStartupMessages(library("BBmisc"))
suppressAll(library('devtools'))
suppressAll(library('lubridate'))
suppressAll(library('plyr'))
suppressAll(library('stringr'))
suppressAll(library('magrittr'))
suppressAll(library('dplyr'))
suppressAll(library('tidyr'))
suppressAll(library('readr'))
suppressAll(library('tidyverse')) #load c(dplyr, tidyr, stringr, readr) due to system doesn't work.
suppressAll(library('tidyquant'))
suppressAll(library("shiny"))
suppressAll(library("shinyjs"))
suppressAll(library('shinyBS'))
suppressAll(library('DT'))
suppressAll(library('quantmod'))
suppressAll(library('formattable'))
suppressAll(library('highcharter'))
suppressAll(library('PerformanceAnalytics'))
suppressAll(library('memoise'))
#'@ suppressAll(source('./function/glmPrice.R'))
suppressMessages(source('./function/compStocks.R'))

## check if the saved dataset is today's data? if previous day then need to scrap from website.
if(file.exists('./data/LAD.rds')) {
  if(readRDS('./data/LAD.rds') %>% attributes %>% .$updated %>% as.Date < today()) {
    tryCatch({
      suppressAll(getSymbols('LAD', from = '2015-01-01'))
    }, error = function(e) stop('Kindly restart the shiny app.'))
  } else {
    LAD <- read_rds(path = './data/LAD.rds')
  }
} else {
  suppressAll(getSymbols('LAD', from = '2015-01-01'))
  saveRDS(LAD, file = './data/LAD.rds')
}

#'@ tryCatch({
#'@   suppressAll(getSymbols('LAD', from = '2015-01-01'))
#'@   if(exists('LAD')) saveRDS(LAD, file = './data/LAD.rds')
#'@   }, error = function(e) LAD <- read_rds(path = './data/LAD.rds'))

#'@ if(!exists('LAD')) LAD <- read_rds(path = './data/LAD.rds')

LADDT <- LAD %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
  tbl_df %>% mutate(Date = ymd(Date)) %>% arrange(Date)
#mutate(LAD.Volume = formattable::digits(
#       LAD.Volume, 0, format = 'd', big.mark = ','))
dateID <- LADDT$Date

## list the cv.glmnet models.
families <- c('gaussian', 'binomial', 'poisson', 'multinomial', 'cox', 'mgaussian', 'all')
gsfit <- compStocks(LADDT, family = families[1], maxit = 1000, .print = TRUE)
bnfit <- compStocks(LADDT, family = families[2], maxit = 1000, .print = TRUE)
psfit <- compStocks(LADDT, family = families[3], maxit = 1000, .print = TRUE)
mnfit <- compStocks(LADDT, family = families[4], maxit = 1000, .print = TRUE)
cxfit <- compStocks(LADDT, family = families[5], maxit = 1000, .print = TRUE) #not yet ready
mgfit <- compStocks(LADDT, family = families[6], maxit = 1000, .print = TRUE) #not yet ready
alfit <- compStocks(LADDT, family = families[7], maxit = 1000, .print = TRUE)

saveRDS(gsfit, file = './data/gsfit.rds')
saveRDS(bnfit, file = './data/bnfit.rds')
saveRDS(psfit, file = './data/psfit.rds')
saveRDS(mnfit, file = './data/mnfit.rds')
saveRDS(cxfit, file = './data/cxfit.rds')
saveRDS(mgfit, file = './data/mgfit.rds')
saveRDS(alfit, file = './data/alfit.rds')

ldply(gsfit$fit, function(x) x$mse) %>% tbl_df
ldply(bnfit$fit, function(x) x$mse) %>% tbl_df
ldply(psfit$fit, function(x) x$mse) %>% tbl_df
ldply(mnfit$fit, function(x) x$mse) %>% tbl_df
ldply(cxfit$fit, function(x) x$mse) %>% tbl_df
ldply(mgfit$fit, function(x) x$mse) %>% tbl_df
ldply(alfit$fit, function(x) x$mse) %>% tbl_df

ldply(gsfit$fit, function(x) x$mse) %>% tbl_df %>% filter(mse == min(mse))
# A tibble: 18 × 3
#.id  model          mse
#<chr> <fctr>        <dbl>
#1   fitgaum97   mse0 9.847582e-06
#2   fitgaum98   mse0 9.847582e-06
#3   fitgaum99   mse0 9.847582e-06
#4  fitgaum100   mse0 9.847582e-06
#5  fitgaum103   mse0 9.847582e-06
#6  fitgaum104   mse0 9.847582e-06
#7  fitgaum105   mse0 9.847582e-06
#8  fitgaum106   mse0 9.847582e-06
#9  fitgaum107   mse0 9.847582e-06
#10 fitgaum108   mse0 9.847582e-06
#11 fitgaum111   mse0 9.847582e-06
#12 fitgaum112   mse0 9.847582e-06
#13 fitgaum113   mse0 9.847582e-06
#14 fitgaum114   mse0 9.847582e-06
#15 fitgaum115   mse0 9.847582e-06
#16 fitgaum116   mse0 9.847582e-06
#17 fitgaum119   mse0 9.847582e-06
#18 fitgaum120   mse0 9.847582e-06
gsfit$formula1[c(97:100, 103:108, 111:116, 119:120)]


#> ldply(psfit$fit, function(x) x$mse) %>% tbl_df
# A tibble: 1,188 × 3
#.id  model      mse
#<chr> <fctr>    <dbl>
#1  fitpoim1   mse0 11713.62
#2  fitpoim1   mse1 11713.62
#3  fitpoim1   mse2 11713.62
#4  fitpoim1   mse3 11713.62
#5  fitpoim1   mse4 11713.62
#6  fitpoim1   mse5 11713.62
#7  fitpoim1   mse6 11713.62
#8  fitpoim1   mse7 11713.62
#9  fitpoim1   mse8 11713.62
#10 fitpoim1   mse9 11713.62
# ... with 1,178 more rows

#> ldply(fitpoim, function(x) x$mse) %>% tbl_df %>% filter(mse == min(mse))
# A tibble: 2 × 3
#.id  model          mse
#<chr> <fctr>        <dbl>
#1 fitpoim75   mse2 2.375528e-05
#2 fitpoim81   mse2 2.375528e-05

#> poim[c(75, 81)]
#[1] "glmPrice(mbase, family = family, xy.matrix = 'h2', alpha = alpha, yv = 'baseline', tmeasure = 'deviance', maxit = maxit, pred.type = 'response', nfolds = nfolds, foldid = foldid, s = 'lambda.min', weight.date = 'FALSE', weight.volume = 'FALSE', parallel = parallel, .log = .log)"
#[2] "glmPrice(mbase, family = family, xy.matrix = 'h2', alpha = alpha, yv = 'baseline', tmeasure = 'mse', maxit = maxit, pred.type = 'response', nfolds = nfolds, foldid = foldid, s = 'lambda.min', weight.date = 'FALSE', weight.volume = 'FALSE', parallel = parallel, .log = .log)"

## checking the number of observation.
llply(gsfit$fit, function(x) {
  ldly(x$yhat, function(y) {
    nrow(y)
  })
})

llply(gsfit$fit, function(x) {
  ldply(x$yhat, function(y) {
    nrow(y)
  })
}) %>% ldply %>% tbl_df %>% filter(V1 == 514)

## Gaussian models - filtered only take nrow same with ibservation (due to saved time, will take all observation includes 2056 obs to compare some other days.)
name514gs <- llply(gsfit$fit, function(x) {
  ldply(x$yhat, function(y) {
    nrow(y)
  })
}) %>% ldply %>% tbl_df %>% filter(V1 == 514) %>% .$.id %>% unique

gsfit$fit[name514gs]
ldply(gsfit$fit[name514gs], function(x) x$mse) %>% tbl_df %>% filter(mse == min(mse))

## Poisson models - filtered only take nrow same with observation (due to saved time, will take all observation includes 2056 obs to compare some other days.)
name514ps <- llply(psfit$fit, function(x) {
  ldply(x$yhat, function(y) {
    nrow(y)
  })
}) %>% ldply %>% tbl_df %>% filter(V1 == 514) %>% .$.id %>% unique

psfit$fit[name514ps]
ldply(psfit$fit[name514ps], function(x) x$mse) %>% tbl_df %>% filter(mse == min(mse))

### ----------------- shiny app temporarily models ---------------------
## 
gsfit <- read_rds(path = './data/gsfit.rds')

## Gaussian models - filtered only take nrow same with ibservation (due to saved time, will take all observation includes 2056 obs to compare some other days.)
name514gs <- llply(gsfit$fit, function(x) {
  ldply(x$yhat, function(y) {
    nrow(y)
  })
}) %>% ldply %>% tbl_df %>% filter(V1 == 514) %>% .$.id %>% unique

tmpsumgs <- ldply(gsfit$fit[name514gs], function(x) x$mse) %>% tbl_df
saveRDS(tmpsumgs, file = './data/tmpsumgs.rds')

## check
ldply(gsfit$fit[name514gs], function(x) x$mse) %>% tbl_df %>% filter(mse == min(mse))
# A tibble: 2 × 3
#.id  model          mse
#<chr> <fctr>        <dbl>
#1 fitgaum1410   mse7 2.530952e-05
#2 fitgaum1425   mse7 2.530952e-05
saveRDS(gsfit$fit$fitgaum1410, file = './data/fitgaum1410.rds')
saveRDS(gsfit$fit$fitgaum1425, file = './data/fitgaum1425.rds')

#> data.frame(LADDT, g1410 = gsfit$fit$fitgaum1410$yhat[[7]], g1425 = gsfit$fit$fitgaum1425$yhat[[7]]) %>% tbl_df %>% rename(g1410 = X1, g1425 = X1.1) %>% mutate(g1410.Price = g1410 * first(LAD.Open), g1425.Price = g1425 * first(LAD.Open)) %>% select(-LAD.Volume) %>% tail
# A tibble: 6 × 10
#Date LAD.Open LAD.High LAD.Low LAD.Close LAD.Adjusted    g1410    g1425 g1410.Price g1425.Price
#<date>    <dbl>    <dbl>   <dbl>     <dbl>        <dbl>    <dbl>    <dbl>       <dbl>       <dbl>
#1 2017-01-09    99.55   100.40   98.26     99.81        99.81 1.141922 1.141922    99.42713    99.42713
#2 2017-01-10    99.74   102.73   99.74    100.94       100.94 1.152180 1.152180   100.32035   100.32035
#3 2017-01-11   100.67   102.11   99.87    102.08       102.08 1.157380 1.157380   100.77310   100.77310
#4 2017-01-12   101.87   103.13   99.08    103.02       103.02 1.166039 1.166039   101.52698   101.52698
#5 2017-01-13   102.94   104.44  100.96    101.33       101.33 1.180093 1.180093   102.75074   102.75074
#6 2017-01-17   101.53   105.32  101.01    101.67       101.67 1.173030 1.173030   102.13569   102.13569

## the baseline method only get the trend which as the trend of the price.
tmptable <- data.frame(LADDT, g1410 = gsfit$fit$fitgaum1410$yhat[[7]], 
           g1425 = gsfit$fit$fitgaum1425$yhat[[7]]) %>% tbl_df %>% 
  rename(g1410 = X1, g1425 = X1.1) %>% 
  mutate(g1410.Price = g1410 * first(LAD.Open), g1425.Price = g1425 * first(LAD.Open))
saveRDS(tmptable, file = './data/tmptable.rds')

## ------------------- Testing -----------------------------------
## Testing 
gsfit <- compStocks(LADDT, family = 'gaussian', xy.matrix = 'h2', maxit = 100000, 
                    yv = c('daily.mean1', 'daily.mean2', 'daily.mean3', 'mixed1', 
                           'mixed2', 'mixed3'), .print = TRUE)

mse1 <- ldply(gsfit$fit, function(x) x$mse) %>% tbl_df %>% filter(mse == min(mse))
name514gs <- unique(mse1$.id)
tmpsumgs <- ldply(gsfit$fit[name514gs], function(x) x$mse) %>% tbl_df
saveRDS(tmpsumgs, file = './data/tmpsumgs.rds')

tmpgsfit <- gsfit$fit[name514gs]
saveRDS(tmpgsfit, file = './data/tmpgsfit.rds')

tmpgsform <- gsfit$formula1[str_replace_all(name514gs, 'fitgaum', '') %>% as.numeric]
saveRDS(tmpgsform, file = './data/tmpgsform.rds')

## table
lmse <- tmpsumgs %>% filter(mse == min(mse)) %>% .$model %>% 
  str_replace_all('mse', '') %>% as.numeric
tmptable <- llply(seq(lmse), function(i) {
    y <- data.frame(tmpgsfit[[i]]$yhat) %>% tbl_df %>% bind_cols
    names(y) <- paste0('alpha', (0:10)/10, '.Close')
    y[paste0('alpha', lmse[[i]]/10)]
    })
tmptable %<>% bind_cols %>% tbl_df
names(tmptable) <- paste0(names(tmpgsfit), '.', names(tmptable))

tmptable <- data.frame(LADDT, tmptable) %>% tbl_df
saveRDS(tmptable, file = './data/tmptable.rds')

## from below comparison, we can concludes that the tmpgsform is same and it only different in predict type.
#'@ tmp <- llply(seq(lmse), function(i) {
#'@   y <- data.frame(tmpgsfit[[i]]$yhat) %>% tbl_df %>% bind_cols
#'@   names(y) <- paste0('alpha', (0:10)/10)
#'@   y[paste0('alpha', lmse[[i]]/10)]
#'@ }) %>% bind_cols
#'@ > as.matrix(tmp)[, 1] - as.matrix(tmp) %>% as.matrix %>% data.frame %>% tbl_df %>% unlist %>% unique
#[1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#[54] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#[107] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#[160] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#[213] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#[266] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#[319] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#[372] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#[425] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#[478] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

#now I try to take one among them (since all exactly same) to plot.
fitgaum193 <- gsfit$fit['fitgaum193'] # gsfit$fit[name514gs[1]]
saveRDS(fitgaum193, file = './data/fitgaum193.rds')

fitgaum193 <- read_rds(path = './data/fitgaum193.rds')
## plot a graph to compare the alpha 0 to 1 (rigde, alpha 0.1~0.9, lasso)
tblgaum193 <- llply(fitgaum193, function(x) {
  y <- x$yhat %>% data.frame %>% tbl_df
  names(y) <- paste0('alpha', str_replace_all((0:10)/10, '\\.', ''), '.Close')
  y
}) %>% data.frame(LADDT, .) %>% tbl_df
gaum193.price <- xts(tblgaum193[, -1], tblgaum193$Date)
saveRDS(gaum193.price, file = './data/gaum193.price.rds')
gaum193.price <- read_rds(path = './data/gaum193.price.rds')

fitgaum193.alpha08 <- list(fit = fitgaum193$fit$fit[[8]], 
                           yhat = fitgaum193$fitgaum193$yhat[[8]], 
                           mse = fitgaum193$fitgaum193$mse)
saveRDS(fitgaum193.alpha08, file = './data/fitgaum193.alpha08.rds')
fitgaum193.alpha08 <- read_rds(path = './data/fitgaum193.alpha08.rds')


## 

