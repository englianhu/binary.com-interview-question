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
suppressAll(source('./function/lmStocks.R'))

## =================================== Read Data =====================================
tryCatch({
  suppressAll(getSymbols('AAPL', from = '2015-01-01'))
  if(exists('AAPL')) saveRDS(AAPL, file = './data/AAPL.rds')
}, error = function(e) AAPL <- read_rds(path = './data/AAPL.rds'))

if(!exists('AAPL')) AAPL <- read_rds(path = './data/AAPL.rds')

AAPLDT <- AAPL %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
  tbl_df %>% mutate(Date = ymd(Date))#, 
#AAPL.Volume = formattable::digits(
#AAPL.Volume, 0, format = 'd', big.mark = ','))
dateID <- AAPLDT$Date

## ============================== Regression Models ==================================

## -------------------------------- 1. Gaussian --------------------------------------
## ------ 1. 1A gaussian (daily.mean + deviance + lambda.min) ------------------------
lm1AAA <- lmStocks(AAPLDT, family = 'gaussian', yv = 'daily.mean', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 2. 1B gaussian (daily.mean + deviance + lambda.1se) ------------------------
lm1AAB <- lmStocks(AAPLDT, family = 'gaussian', yv = 'daily.mean', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 3. 2A gaussian (daily.mean + mse + lambda.min) ------------------------
lm1ABA <- lmStocks(AAPLDT, family = 'gaussian', yv = 'daily.mean', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 4. 2B gaussian (daily.mean + mse + lambda.1se) ------------------------
lm1ABB <- lmStocks(AAPLDT, family = 'gaussian', yv = 'daily.mean', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 5. 3A gaussian (daily.mean + mae + lambda.min) ------------------------
lm1ACA <- lmStocks(AAPLDT, family = 'gaussian', yv = 'daily.mean', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 6. 3B gaussian (daily.mean + mae + lambda.1se) ------------------------
lm1ACB <- lmStocks(AAPLDT, family = 'gaussian', yv = 'daily.mean', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 7. 4A gaussian (baseline + deviance + lambda.min) ------------------------
lm1BAA <- lmStocks(AAPLDT, family = 'gaussian', yv = 'baseline', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 8. 4B gaussian (baseline + deviance + lambda.1se) ------------------------
lm1BAB <- lmStocks(AAPLDT, family = 'gaussian', yv = 'baseline', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 9. 5A gaussian (baseline + mse + lambda.min) ------------------------
lm1BBA <- lmStocks(AAPLDT, family = 'gaussian', yv = 'baseline', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 10. 5B gaussian (baseline + mse + lambda.1se) ------------------------
lm1BBB <- lmStocks(AAPLDT, family = 'gaussian', yv = 'baseline', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 11. 6A gaussian (baseline + mae + lambda.min) ------------------------
lm1BCA <- lmStocks(AAPLDT, family = 'gaussian', yv = 'baseline', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 12. 6B gaussian (baseline + mae + lambda.1se) ------------------------
lm1BCB <- lmStocks(AAPLDT, family = 'gaussian', yv = 'baseline', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 13. 7A gaussian (mixed + deviance + lambda.min) ------------------------
lm1CAA <- lmStocks(AAPLDT, family = 'gaussian', yv = 'mixed', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 14. 7B gaussian (mixed + deviance + lambda.1se) ------------------------
lm1CAB <- lmStocks(AAPLDT, family = 'gaussian', yv = 'mixed', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 15. 8A gaussian (mixed + mse + lambda.min) ------------------------
lm1CBA <- lmStocks(AAPLDT, family = 'gaussian', yv = 'mixed', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 16. 8B gaussian (mixed + mse + lambda.1se) ------------------------
lm1CBB <- lmStocks(AAPLDT, family = 'gaussian', yv = 'mixed', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 17. 9A gaussian (mixed + mae + lambda.min) ------------------------
lm1CCA <- lmStocks(AAPLDT, family = 'gaussian', yv = 'mixed', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 18. 9B gaussian (mixed + mae + lambda.1se) ------------------------
lm1CCB <- lmStocks(AAPLDT, family = 'gaussian', yv = 'mixed', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## -------------------------------- 2. Binomial --------------------------------------
## ------ 19. 1A binomial (daily.mean + deviance + lambda.min) ------------------------
lm2AAA <- lmStocks(AAPLDT, family = 'binomial', yv = 'daily.mean', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 20. 1B binomial (daily.mean + deviance + lambda.1se) ------------------------
lm2AAB <- lmStocks(AAPLDT, family = 'binomial', yv = 'daily.mean', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 21. 2A binomial (daily.mean + mse + lambda.min) ------------------------
lm2ABA <- lmStocks(AAPLDT, family = 'binomial', yv = 'daily.mean', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 22. 2B binomial (daily.mean + mse + lambda.1se) ------------------------
lm2ABB <- lmStocks(AAPLDT, family = 'binomial', yv = 'daily.mean', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 23. 3A binomial (daily.mean + mae + lambda.min) ------------------------
lm2ACA <- lmStocks(AAPLDT, family = 'binomial', yv = 'daily.mean', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 24. 3B binomial (daily.mean + mae + lambda.1se) ------------------------
lm2ACB <- lmStocks(AAPLDT, family = 'binomial', yv = 'daily.mean', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 25. 4A binomial (daily.mean + class + lambda.min) ------------------------
lm2ADA <- lmStocks(AAPLDT, family = 'binomial', yv = 'daily.mean', tmeasure = 'class', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 26. 4B binomial (daily.mean + class + lambda.1se) ------------------------
lm2ADB <- lmStocks(AAPLDT, family = 'binomial', yv = 'daily.mean', tmeasure = 'class', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 27. 5A binomial (daily.mean + auc + lambda.min) ------------------------
lm2AEA <- lmStocks(AAPLDT, family = 'binomial', yv = 'daily.mean', tmeasure = 'auc', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 28. 5B binomial (daily.mean + auc + lambda.1se) ------------------------
lm2AEB <- lmStocks(AAPLDT, family = 'binomial', yv = 'daily.mean', tmeasure = 'auc', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 29. 6A binomial (baseline + deviance + lambda.min) ------------------------
lm2BAA <- lmStocks(AAPLDT, family = 'binomial', yv = 'baseline', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 30. 6B binomial (baseline + deviance + lambda.1se) ------------------------
lm2BAB <- lmStocks(AAPLDT, family = 'binomial', yv = 'baseline', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 31. 7A binomial (baseline + mse + lambda.min) ------------------------
lm2BBA <- lmStocks(AAPLDT, family = 'binomial', yv = 'baseline', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 32. 7B binomial (baseline + mse + lambda.1se) ------------------------
lm2BBB <- lmStocks(AAPLDT, family = 'binomial', yv = 'baseline', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 33. 8A binomial (baseline + mae + lambda.min) ------------------------
lm2BCA <- lmStocks(AAPLDT, family = 'binomial', yv = 'baseline', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 34. 8B binomial (baseline + mae + lambda.1se) ------------------------
lm2BCB <- lmStocks(AAPLDT, family = 'binomial', yv = 'baseline', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 35. 9A binomial (baseline + class + lambda.min) ------------------------
lm2BDA <- lmStocks(AAPLDT, family = 'binomial', yv = 'baseline', tmeasure = 'class', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 36. 9B binomial (baseline + class + lambda.1se) ------------------------
lm2BDB <- lmStocks(AAPLDT, family = 'binomial', yv = 'baseline', tmeasure = 'class', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 37. 10A binomial (baseline + auc + lambda.min) ------------------------
lm2BEA <- lmStocks(AAPLDT, family = 'binomial', yv = 'baseline', tmeasure = 'auc', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 38. 10B binomial (baseline + auc + lambda.1se) ------------------------
lm2BEB <- lmStocks(AAPLDT, family = 'binomial', yv = 'baseline', tmeasure = 'auc', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 39. 11A binomial (mixed + deviance + lambda.min) ------------------------
lm2CAA <- lmStocks(AAPLDT, family = 'binomial', yv = 'mixed', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 40. 11B binomial (mixed + deviance + lambda.1se) ------------------------
lm2CAB <- lmStocks(AAPLDT, family = 'binomial', yv = 'mixed', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 41. 12A binomial (mixed + mse + lambda.min) ------------------------
lm2CBA <- lmStocks(AAPLDT, family = 'binomial', yv = 'mixed', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 42. 12B binomial (mixed + mse + lambda.1se) ------------------------
lm2CBB <- lmStocks(AAPLDT, family = 'binomial', yv = 'mixed', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 43. 13A binomial (mixed + mae + lambda.min) ------------------------
lm2CCA <- lmStocks(AAPLDT, family = 'binomial', yv = 'mixed', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 44. 13B binomial (mixed + mae + lambda.1se) ------------------------
lm2CCB <- lmStocks(AAPLDT, family = 'binomial', yv = 'mixed', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 45. 14A binomial (mixed + class + lambda.min) ------------------------
lm2CDA <- lmStocks(AAPLDT, family = 'binomial', yv = 'mixed', tmeasure = 'class', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 46. 14B binomial (mixed + class + lambda.1se) ------------------------
lm2CDB <- lmStocks(AAPLDT, family = 'binomial', yv = 'mixed', tmeasure = 'class', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 47. 15A binomial (mixed + auc + lambda.min) ------------------------
lm2CEA <- lmStocks(AAPLDT, family = 'binomial', yv = 'mixed', tmeasure = 'auc', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 48. 15B binomial (mixed + auc + lambda.1se) ------------------------
lm2CEB <- lmStocks(AAPLDT, family = 'binomial', yv = 'mixed', tmeasure = 'auc', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## -------------------------------- 3. Poisson --------------------------------------
## ------ 49. 1A poisson (daily.mean + deviance + lambda.min) ------------------------
lm3AAA <- lmStocks(AAPLDT, family = 'poisson', yv = 'daily.mean', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 50. 1B poisson (daily.mean + deviance + lambda.1se) ------------------------
lm3AAB <- lmStocks(AAPLDT, family = 'poisson', yv = 'daily.mean', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 51. 2A poisson (daily.mean + mse + lambda.min) ------------------------
lm3ABA <- lmStocks(AAPLDT, family = 'poisson', yv = 'daily.mean', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 52. 2B poisson (daily.mean + mse + lambda.1se) ------------------------
lm3ABB <- lmStocks(AAPLDT, family = 'poisson', yv = 'daily.mean', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 53. 3A poisson (daily.mean + mae + lambda.min) ------------------------
lm3ACA <- lmStocks(AAPLDT, family = 'poisson', yv = 'daily.mean', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 54. 3B poisson (daily.mean + mae + lambda.1se) ------------------------
lm3ACB <- lmStocks(AAPLDT, family = 'poisson', yv = 'daily.mean', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 55. 4A poisson (baseline + deviance + lambda.min) ------------------------
lm3BAA <- lmStocks(AAPLDT, family = 'poisson', yv = 'baseline', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 56. 4B poisson (baseline + deviance + lambda.1se) ------------------------
lm3BAB <- lmStocks(AAPLDT, family = 'poisson', yv = 'baseline', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 57. 5A poisson (baseline + mse + lambda.min) ------------------------
lm3BBA <- lmStocks(AAPLDT, family = 'poisson', yv = 'baseline', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 58. 5B poisson (baseline + mse + lambda.1se) ------------------------
lm3BBB <- lmStocks(AAPLDT, family = 'poisson', yv = 'baseline', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 59. 6A poisson (baseline + mae + lambda.min) ------------------------
lm3BCA <- lmStocks(AAPLDT, family = 'poisson', yv = 'baseline', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 60. 6B poisson (baseline + mae + lambda.1se) ------------------------
lm3BCB <- lmStocks(AAPLDT, family = 'poisson', yv = 'baseline', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 61. 7A poisson (mixed + deviance + lambda.min) ------------------------
lm3CAA <- lmStocks(AAPLDT, family = 'poisson', yv = 'mixed', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 62. 7B poisson (mixed + deviance + lambda.1se) ------------------------
lm3CAB <- lmStocks(AAPLDT, family = 'poisson', yv = 'mixed', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 63. 8A poisson (mixed + mse + lambda.min) ------------------------
lm3CBA <- lmStocks(AAPLDT, family = 'poisson', yv = 'mixed', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 64. 8B poisson (mixed + mse + lambda.1se) ------------------------
lm3CBB <- lmStocks(AAPLDT, family = 'poisson', yv = 'mixed', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 65. 9A poisson (mixed + mae + lambda.min) ------------------------
lm3CCA <- lmStocks(AAPLDT, family = 'poisson', yv = 'mixed', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 66. 9B poisson (mixed + mae + lambda.1se) ------------------------
lm3CCB <- lmStocks(AAPLDT, family = 'poisson', yv = 'mixed', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## -------------------------------- 4. Multinomial --------------------------------------
## ------ 67. 1AA multinomial (daily.mean + deviance + grouped + lambda.min) ------------
lm4AAAA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'daily.mean', tmeasure = 'deviance', 
                    maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'grouped', parallel = TRUE)

## ------ 68. 1AB multinomial (daily.mean + deviance + grouped + lambda.1se) ------------
lm4AAAB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'daily.mean', tmeasure = 'deviance', 
                    maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'grouped', parallel = TRUE)

## ------ 69. 1BA multinomial (daily.mean + deviance + ungrouped + lambda.min) ----------
lm4AABA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'daily.mean', tmeasure = 'deviance', 
                    maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 70. 1BB multinomial (daily.mean + deviance + ungrouped + lambda.1se) ----------
lm4AABB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'daily.mean', tmeasure = 'deviance', 
                    maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 71. 2AA multinomial (daily.mean + mse + grouped + lambda.min) ------------------
lm4ABAA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'daily.mean', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 72. 2AB multinomial (daily.mean + mse + grouped + lambda.1se) ------------------
lm4ABAB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'daily.mean', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 73. 2BA multinomial (daily.mean + mse + ungrouped + lambda.min) ----------
lm4ABBA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'daily.mean', tmeasure = 'mse', 
                    maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 74. 2BB multinomial (daily.mean + mse + ungrouped + lambda.1se) ----------
lm4ABBB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'daily.mean', tmeasure = 'mse', 
                    maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 75. 3AA multinomial (daily.mean + mae + grouped + lambda.min) ------------------------
lm4ACAA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'daily.mean', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 76. 3AB multinomial (daily.mean + mae + grouped + lambda.1se) ------------------------
lm4ACAB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'daily.mean', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 77. 3BA multinomial (daily.mean + mae + ungrouped + lambda.min) ----------
lm4ACBA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'daily.mean', tmeasure = 'mae', 
                    maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 78. 3BB multinomial (daily.mean + mae + ungrouped + lambda.1se) ----------
lm4ACBB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'daily.mean', tmeasure = 'mae', 
                    maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 79. 4AA multinomial (daily.mean + class + grouped + lambda.min) ------------------------
lm4ADAA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'daily.mean', tmeasure = 'class', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 80. 4AB multinomial (daily.mean + class + grouped + lambda.1se) ------------------------
lm4ADAB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'daily.mean', tmeasure = 'class', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 81. 4BA multinomial (daily.mean + class + ungrouped + lambda.min) ----------
lm4ADBA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'daily.mean', tmeasure = 'class', 
                    maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 82. 4BB multinomial (daily.mean + class + ungrouped + lambda.1se) ----------
lm4ADBB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'daily.mean', tmeasure = 'class', 
                    maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 83. 6AA multinomial (baseline + deviance + grouped + lambda.min) ------------------------
lm4BAAA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'baseline', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 84. 6AB multinomial (baseline + deviance + grouped + lambda.1se) ------------------------
lm4BAAB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'baseline', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 85. 6BA multinomial (baseline + deviance + ungrouped + lambda.min) ----------
lm4BABA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'baseline', tmeasure = 'deviance', 
                    maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 86. 6BB multinomial (baseline + deviance + ungrouped + lambda.1se) ----------
lm4BABB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'baseline', tmeasure = 'deviance', 
                    maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 87. 7AA multinomial (baseline + mse + grouped + lambda.min) ------------------------
lm4BBAA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'baseline', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 88. 7AB multinomial (baseline + mse + grouped + lambda.1se) ------------------------
lm4BBAB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'baseline', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 89. 7BA multinomial (baseline + mse + ungrouped + lambda.min) ----------
lm4BBBA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'baseline', tmeasure = 'mse', 
                    maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 90. 7BB multinomial (baseline + mse + ungrouped + lambda.1se) ----------
lm4BBBB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'baseline', tmeasure = 'mse', 
                    maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 91. 8AA multinomial (baseline + mae + grouped + lambda.min) ------------------------
lm4BCAA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'baseline', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 92. 8AB multinomial (baseline + mae + grouped + lambda.1se) ------------------------
lm4BCAB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'baseline', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 93. 8BA multinomial (baseline + mae + ungrouped + lambda.min) ----------
lm4BCBA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'baseline', tmeasure = 'mae', 
                    maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 94. 8BB multinomial (baseline + mae + ungrouped + lambda.1se) ----------
lm4BCBB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'baseline', tmeasure = 'mae', 
                    maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 95. 9AA multinomial (baseline + class + grouped + lambda.min) ------------------------
lm4BDAA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'baseline', tmeasure = 'class', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 96. 9AB multinomial (baseline + class + grouped + lambda.1se) ------------------------
lm4BDAB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'baseline', tmeasure = 'class', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 97. 9BA multinomial (baseline + class + ungrouped + lambda.min) ----------
lm4BDBA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'baseline', tmeasure = 'class', 
                    maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 98. 9BB multinomial (baseline + class + ungrouped + lambda.1se) ----------
lm4BDBB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'baseline', tmeasure = 'class', 
                    maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 99. 11AA multinomial (mixed + deviance + grouped + lambda.min) ------------------------
lm4CAAA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'mixed', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 100. 11AB multinomial (mixed + deviance + grouped + lambda.1se) ------------------------
lm4CAAB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'mixed', tmeasure = 'deviance', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 101. 11BA multinomial (mixed + deviance + ungrouped + lambda.min) ----------
lm4CABA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'mixed', tmeasure = 'deviance', 
                    maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 102. 11BB multinomial (mixed + deviance + ungrouped + lambda.1se) ----------
lm4CABB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'mixed', tmeasure = 'deviance', 
                    maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 103. 12AA multinomial (mixed + mse + grouped + lambda.min) ------------------------
lm4CBAA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'mixed', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 104. 12AB multinomial (mixed + mse + grouped + lambda.1se) ------------------------
lm4CBAB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'mixed', tmeasure = 'mse', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 105. 12BA multinomial (mixed + mse + ungrouped + lambda.min) ----------
lm4CBBA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'mixed', tmeasure = 'mse', 
                    maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 106. 12BB multinomial (mixed + mse + ungrouped + lambda.1se) ----------
lm4CBBB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'mixed', tmeasure = 'mse', 
                    maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 107. 13AA multinomial (mixed + mae + grouped + lambda.min) ------------------------
lm4CCAA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'mixed', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 108. 13AB multinomial (mixed + mae + grouped + lambda.1se) ------------------------
lm4CCAB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'mixed', tmeasure = 'mae', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 109. 13BA multinomial (mixed + mae + ungrouped + lambda.min) ----------
lm4CCBA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'mixed', tmeasure = 'mae', 
                    maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 110. 13BB multinomial (mixed + mae + ungrouped + lambda.1se) ----------
lm4CCBB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'mixed', tmeasure = 'mae', 
                    maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 111. 14AA multinomial (mixed + class + grouped + lambda.min) ------------------------
lm4CDAA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'mixed', tmeasure = 'class', 
                   maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 112. 14AB multinomial (mixed + class + grouped + lambda.1se) ------------------------
lm4CDAB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'mixed', tmeasure = 'class', 
                   maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                   weight.volume = FALSE, parallel = TRUE)

## ------ 113. 14BA multinomial (mixed + class + ungrouped + lambda.min) ----------
lm4CDBA <- lmStocks(AAPLDT, family = 'multinomial', yv = 'mixed', tmeasure = 'class', 
                    maxit = 1000, nfolds = 10, s = 'lambda.min', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ------ 114. 14BB multinomial (mixed + class + ungrouped + lambda.1se) ----------
lm4CDBB <- lmStocks(AAPLDT, family = 'multinomial', yv = 'mixed', tmeasure = 'class', 
                    maxit = 1000, nfolds = 10, s = 'lambda.1se', weight.date = FALSE, 
                    weight.volume = FALSE, tmultinomial = 'ungrouped', parallel = TRUE)

## ============================== Comparison Models ==================================
suppressPackageStartupMessages(library('rlist'))

ls(envir = .GlobalEnv, pattern = "lm1")
compr <- paste(ls(envir = .GlobalEnv, pattern = "lm1"), collapse = ',') %>% 
  paste0('list(', ., ')') %>% parse(text = .) %>% eval

mmse <- llply(compr, function(x) x$mse)
names(mmse) <- ls(envir = .GlobalEnv, pattern = "lm1")
mmse %<>% ldply %>% tbl_df
filter(mmse, mse == min(mse))
