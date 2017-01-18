compStocks <- function(mbase, family = 'gaussian', maxit = 1000, .print = FALSE) {
  ## ========================= Load Packages ===================================
  source('./function/lmStocks.R')
  
  ## ========================= Set Arguments ===================================
  mbase <- LADDT
  families <- c('gaussian', 'binomial', 'poisson', 'multinomial', 'cox', 'mgaussian', 'all')
  xy.matrix <- c('h1', 'h2')
  alpha <- 0:10
  
  yv <- c('baseline', 'daily.mean1', 'daily.mean2', 'daily.mean3', 'mixed1', 'mixed2', 'mixed3')
  setform <- c('l1', 'l2', 'l3', 'l4')
  pred.type <- c('link', 'response', 'coefficients', 'nonzero', 'class')
  
  nfolds = 10
  foldid = NULL
  s = c('lambda.min', 'lambda.1se')
  
  weight.date = FALSE     #weight.date is ...
  weight.volume = FALSE   #weight.volume is ...
  parallel = FALSE
  .log = FALSE
  
  ## ======================= Data Validation ===================================
  if(family %in% families) {
    family <- family
  } else {
    stop("family must be within c('gaussian', 'binomial', 'poisson', 'multinomial', 'cox', 'mgaussian', 'all').")
  }
  alpha <- ifelse(length(alpha) > 1, paste(range(alpha), collapse = ':'), alpha)
  nfolds <- ifelse(length(nfolds) > 1, paste(range(nfolds), collapse = ':'), nfolds)
  
  if(is.null(foldid)) {
    foldid <- 'NULL'
  } else {
    foldid <- ifelse(length(foldid) > 1, paste(range(foldid), collapse = ':'), foldid)
  }
  
  ## ======================== Regression Models ================================
  if((family == 'gaussian')|(family == 'all')) {
    ## -------------------------- gaussian --------------------------------
    ## 3 models : tmeasure = 'deviance' or tmeasure = 'mse' or tmeasure = 'mae'.
    tmeasure <- c('deviance', 'mse', 'mae')
    family <- 'gaussian'
    pred.type <- c('link', 'response', 'nonzero', 'class')
    wt.control = c(TRUE, FALSE)
    
    gaum <- sapply(xy.matrix, function(x) {
      sapply(yv, function(y) {
        sapply(tmeasure, function(z) {
          sapply(pred.type, function(xx) {
            sapply(s, function(xy) {
              sapply(setform, function(xz) {
                sapply(wt.control, function(yx) {
                  paste0('lmStocks(mbase, family = \'', family, '\', xy.matrix = \'', x, 
                         '\', setform = \'', xz, '\', yv = \'', y, '\', tmeasure = \'', z, 
                         '\', maxit = ', maxit, ', pred.type = \'', xx, 
                         '\', alpha = ', alpha, ', nfolds = ', nfolds, 
                         ', foldid = ', foldid, ', s = \'', xy, 
                         '\', weight.date = ', weight.date, ', weight.volume = ', 
                         weight.volume, ', wt.control = ', yx, 
                         ', parallel = ', parallel, ', .log = ', .log, ')')
                })
              })
            })
          })
        })
      })
    }) %>% as.character
    
    gm <- paste(paste0(
      "fitgaum", seq(gaum), " <- ", gaum, 
      "; if(.print == TRUE) cat('gaussian model ", seq(gaum), "/", length(gaum), " calculated.\n')"), 
      collapse = "; ")
    
    ## start algorithmic calculation.
    eval(parse(text = gm)); rm(gm)
    
    ## combine all fitgaum1 to fitgaum~ into a list named fitgaum
    eval(parse(text = paste0('fitgaum <- list(', 
                             paste(paste0('fitgaum', seq(gaum), ' = fitgaum', seq(gaum)), 
                                   collapse = ', '), ')')))
    
    ## rm all fitgaum1 to fitgaum~
    eval(parse(text = paste0('rm(', paste0(paste0('fitgaum', seq(gaum)), 
                                           collapse = ', '), ')')))
    
    tmp1 <- list(formula1 = gaum, fit = fitgaum)
    
    ## return if single family
    if(family == 'gaussian') return(tmp1)
    
    
  } else if((family == 'binomial')|(family == 'all')) {
    ## -------------------------- binomial --------------------------------
    ## 5 models : tmeasure = 'deviance' or tmeasure = 'mse' or 
    ##            tmeasure = 'mae' or tmeasure = 'class' or 
    ##            tmeasure = 'auc'.
    tmeasure <- c('deviance', 'mse', 'mae', 'class', 'auc')
    family <- 'binomial'
    wt.control = FALSE #wt.control not applicable in binomial nor multinomial.
    
    binm <- sapply(xy.matrix, function(x) {
      sapply(yv, function(y) {
        sapply(tmeasure, function(z) {
          sapply(pred.type, function(xx) {
            sapply(s, function(xy) {
              sapply(setform, function(xz) {
                sapply(wt.control, function(yx) {
                  paste0('lmStocks(mbase, family = \'', family, '\', xy.matrix = \'', x, 
                         '\', setform = \'', xz, '\', yv = \'', y, '\', tmeasure = \'', z, 
                         '\', maxit = ', maxit, ', pred.type = \'', xx, 
                         '\', alpha = ', alpha, ', nfolds = ', nfolds, 
                         ', foldid = ', foldid, ', s = \'', xy, 
                         '\', weight.date = ', weight.date, ', weight.volume = ', 
                         weight.volume, ', wt.control = ', yx, 
                         ', parallel = ', parallel, ', .log = ', .log, ')')
                })
              })
            })
          })
        })
      })
    }) %>% as.character
    
    ## start algorithmic calculation.
    eval(parse(text = paste(paste0(
      "fitbinm", seq(binm), " <- ", binm, 
      "; if(.print == TRUE) cat('binomial model ", seq(binm), "/", length(binm), " calculated.\n')"), 
      collapse = "; ")))
    
    ## combine all fitbinm1 to fitbinm~ into a list named fitbinm
    eval(parse(text = paste0('fitbinm <- list(', 
                             paste(paste0('fitbinm', seq(binm), ' = fitbinm', seq(binm)), 
                                   collapse = ', '), ')')))
    
    ## rm all fitbinm1 to fitbinm~
    eval(parse(text = paste0('rm(', paste0(paste0('fitbinm', seq(binm)), 
                                           collapse = ', '), ')')))
    
    tmp2 <- list(formula1 = binm, fit = fitbinm)
    
    ## return if single family
    if(family == 'binomial') return(tmp2)
    
    
  } else if((family == 'poisson')|(family == 'all')) {
    ## -------------------------- poisson ---------------------------------
    ## 3 models : tmeasure = 'deviance' or tmeasure = 'mse' or tmeasure = 'mae'.
    tmeasure <- c('deviance', 'mse', 'mae')
    family <- 'poisson'
    pred.type <- c('link', 'response', 'nonzero')
    wt.control = c(TRUE, FALSE)
    
    poim <- sapply(xy.matrix, function(x) {
      sapply(yv, function(y) {
        sapply(tmeasure, function(z) {
          sapply(pred.type, function(xx) {
            sapply(s, function(xy) {
              sapply(setform, function(xz) {
                sapply(wt.control, function(yx) {
                  paste0('lmStocks(mbase, family = \'', family, '\', xy.matrix = \'', x, 
                         '\', setform = \'', xz, '\', yv = \'', y, '\', tmeasure = \'', z, 
                         '\', maxit = ', maxit, ', pred.type = \'', xx, 
                         '\', alpha = ', alpha, ', nfolds = ', nfolds, 
                         ', foldid = ', foldid, ', s = \'', xy, 
                         '\', weight.date = ', weight.date, ', weight.volume = ', 
                         weight.volume, ', wt.control = ', yx, 
                         ', parallel = ', parallel, ', .log = .log)')
                })
              })
            })
          })
        })
      })
    }) %>% as.character
    
    ## start algorithmic calculation.
    eval(parse(text = paste(paste0(
      "fitpoim", seq(poim), " <- ", poim, 
      "; if(.print == TRUE) cat('poisson model ", seq(poim), "/", length(poim), " calculated.\n')"), 
      collapse = "; ")))
    
    ## combine all fitpoim1 to fitpoim~ into a list named fitpoim
    eval(parse(text = paste0('fitpoim <- list(', 
                             paste(paste0('fitpoim', seq(poim), ' = fitpoim', seq(poim)), 
                                   collapse = ', '), ')')))
    
    ## rm all fitpoim1 to fitpoim~
    eval(parse(text = paste0('rm(', paste0(paste0('fitpoim', seq(poim)), 
                                           collapse = ', '), ')')))
    
    tmp3 <- list(formula1 = poim, fit = fitpoim)
    
    ## return if single family
    if(family == 'poisson') return(tmp3)
    
    
  } else if((family == 'multinomial')|(family == 'all')) {
    ## ----------------------- multinomial --------------------------------
    ## 4 models : tmeasure = 'deviance' or tmeasure = 'mse' or 
    ##            tmeasure = 'mae' or tmeasure = 'class'.
    ## 2 models : tmultinomial = 'grouped' or tmultinomial = 'ungrouped'.
    tmeasure <- c('deviance', 'mse', 'mae', 'class')
    tmultinomial <- c('grouped', 'ungrouped')
    family <- 'multinomial'
    wt.control = FALSE #wt.control not applicable in binomial nor multinomial.
    
    mnmm <- sapply(xy.matrix, function(x) {
      sapply(yv, function(y) {
        sapply(tmeasure, function(z) {
          sapply(pred.type, function(xx) {
            sapply(s, function(xy) {
              sapply(tmultinomial, function(tm) {
                sapply(setform, function(xz) {
                  sapply(wt.control, function(yx) {
                    paste0('lmStocks(mbase, family = \'', family, '\', xy.matrix = \'', x, 
                           '\', setform = \'', xz, '\', yv = \'', y, '\', tmeasure = \'', z, 
                           '\', tmultinomial = ', tm, '\', maxit = ', maxit, ', pred.type = \'', xx, 
                           '\', alpha = ', alpha, ', nfolds = ', nfolds, 
                           ', foldid = ', foldid, ', s = \'', xy, 
                           '\', weight.date = ', weight.date, ', weight.volume = ', 
                           weight.volume, ', wt.control = ', yx, 
                           ', parallel = ', parallel, ', .log = .log)')
                  })
                })
              })
            })
          })
        })
      })
    }) %>% as.character
    
    ## start algorithmic calculation.
    eval(parse(text = paste(paste0(
      "fitmnmm", seq(mnmm), " <- ", mnmm, 
      "; if(.print == TRUE) cat('multinomial model ", seq(mnmm), "/", length(mnmm), " calculated.\n')"), 
      collapse = "; ")))
    
    ## combine all fitmnmm1 to fitmnmm~ into a list named fitmnmm
    eval(parse(text = paste0('fitmnmm <- list(', 
                             paste(paste0('fitmnmm', seq(mnmm), ' = fitmnmm', seq(mnmm)), 
                                   collapse = ', '), ')')))
    
    ## rm all fitmnmm1 to fitmnmm~
    eval(parse(text = paste0('rm(', paste0(paste0('fitmnmm', seq(mnmm)), 
                                           collapse = ', '), ')')))
    
    tmp4 <- list(formula1 = mnmm, fit = fitmnmm)
    
    ## return if single family
    if(family == 'multinomial') return(tmp4)
    
    
  } else if((family == 'cox')|(family == 'all')) {
    ## ------------------------------ cox ---------------------------------
    ## 1 model : tmeasure = 'cox'
    tmeasure <- 'cox'
    family <- 'cox'
    wt.control = c(TRUE, FALSE)
    
    coxm <- sapply(xy.matrix, function(x) {
      sapply(yv, function(y) {
        sapply(tmeasure, function(z) {
          sapply(pred.type, function(xx) {
            sapply(s, function(xy) {
              sapply(setform, function(xz) {
                sapply(wt.control, function(yx) {
                  paste0('lmStocks(mbase, family = \'', family, '\', xy.matrix = \'', x, 
                         '\', setform = \'', xz, '\', yv = \'', y, '\', tmeasure = \'', z, 
                         '\', maxit = ', maxit, ', pred.type = \'', xx, 
                         '\', alpha = ', alpha, ', nfolds = ', nfolds, 
                         ', foldid = ', foldid, ', s = \'', xy, 
                         '\', weight.date = ', weight.date, ', weight.volume = ', 
                         weight.volume, ', wt.control = ', yx, 
                         ', parallel = ', parallel, ', .log = .log)')
                })
              })
            })
          })
        })
      })
    }) %>% as.character
    
    ## start algorithmic calculation.
    eval(parse(text = paste(paste0(
      "fitcoxm", seq(coxm), " <- ", coxm, 
      "; if(.print == TRUE) cat('cox model ", seq(coxm), "/", length(coxm), " calculated.\n')"), 
      collapse = "; ")))
    
    ## combine all fitcoxm1 to fitcoxm~ into a list named fitcoxm
    eval(parse(text = paste0('fitcoxm <- list(', 
                             paste(paste0('fitcoxm', seq(coxm), ' = fitcoxm', seq(coxm)), 
                                   collapse = ', '), ')')))
    
    ## rm all fitcoxm1 to fitcoxm~
    eval(parse(text = paste0('rm(', paste0(paste0('fitcoxm', seq(coxm)), 
                                           collapse = ', '), ')')))
    
    tmp5 <- list(formula1 = coxm, fit = fitcoxm)
    
    ## return if single family
    if(family == 'cox') return(tmp5)
    
    
  }  else if((family == 'mgaussian')|(family == 'all')) {
    ## ------------------------- mgaussian --------------------------------
    ## 1 model : tmeasure = 'mgaussian'
    tmeasure <- 'mgaussian'
    family <- 'mgaussian'
    wt.control = c(TRUE, FALSE)
    
    mgam <- sapply(xy.matrix, function(x) {
      sapply(yv, function(y) {
        sapply(tmeasure, function(z) {
          sapply(pred.type, function(xx) {
            sapply(s, function(xy) {
              sapply(setform, function(xz) {
                sapply(wt.control, function(yx) {
                  paste0('lmStocks(mbase, family = \'', family, '\', xy.matrix = \'', x, 
                         '\', setform = \'', xz, '\', yv = \'', y, '\', tmeasure = \'', z, 
                         '\', maxit = ', maxit, ', pred.type = \'', xx, 
                         '\', alpha = ', alpha, ', nfolds = ', nfolds, 
                         ', foldid = ', foldid, ', s = \'', xy, 
                         '\', weight.date = ', weight.date, ', weight.volume = ', 
                         weight.volume, ', wt.control = ', yx, 
                         ', parallel = ', parallel, ', .log = .log)')
                })
              })
            })
          })
        })
      })
    }) %>% as.character
    
    ## start algorithmic calculation.
    eval(parse(text = paste(paste0(
      "fitmgam", seq(mgam), " <- ", mgam, 
      "; if(.print == TRUE) cat('mgaussian model ", seq(mgam), "/", length(mgam), " calculated.\n')"), 
      collapse = "; ")))
    
    ## combine all fitmgam1 to fitmgam~ into a list named fitmgam
    eval(parse(text = paste0('fitmgam <- list(', 
                             paste(paste0('fitmgam', seq(mgam), ' = fitmgam', seq(mgam)), 
                                   collapse = ', '), ')')))
    
    ## rm all fitmgam1 to fitmgam~
    eval(parse(text = paste0('rm(', paste0(paste0('fitmgam', seq(mgam)), 
                                           collapse = ', '), ')')))
    
    tmp6 <- list(formula1 = mgam, fit = fitmgam)
    
    ## return if single family
    if(family == 'mgaussian') return(tmp6)
    
    
    }else {
    stop('Kindly select family = "gaussian", family = "binomial", family = "poisson", family = "multinomial", family = "cox" or family = "mgaussian".')
  }
  
  ## ======================== Return Function ================================
  ## return all famalies.
  tmp <- list(fitgaum = tmp1, fitbinm = tmp2, fitpoim = tmp3, 
              fitmnmm = tmp4, fitcoxm = tmp5, fitmgam = tmp6)
  rm(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)
  
  return(tmp)
}

