h <- function(ddt, family, yv = 'baseline', logistic.yv = FALSE, wt = NULL, 
              wt.control = TRUE, xy.matrix = 'h1', setform = 'l1', .log = FALSE) {
  ## mbase = default LAD or in data frame format.
  ## 
  ## family = gaussian', 'binomial', 'poisson', 'multinomial', 'cox' and 'mgaussian'.
  ## 
  ## xy.matrix = 'h1' or xy.matrix = 'h2'. setting x and y variables.
  ## 
  ## yv %in% c('baseline', 'close1', 'close2', 'daily.mean1', 'daily.mean2', 'daily.mean3', 
  ##   'mixed1', 'mixed2', 'mixed3') #to model the y (respondence variables)
  ##   baseline only use first element of opening price as baseline.
  ##   close1 = X = data.frame(Op(LAD), Hi(LAD), Lo(LAD)) and Y = Cl(LAD), 
  ##   close2 = X = data.frame(Op(LAD), Hi(LAD), Lo(LAD), Cl(LAD)) and Y = Cl(LAD), 
  ##   daily.mean1 = mean(Op(LAD), Cl(LAD)), daily.mean2 = mean(Hi(LAD), Lo(LAD)), 
  ##   daily.mean3 = mean(Op(LAD), Hi(LAD), Lo(LAD), Cl(LAD)).
  ##   mixed1,2,3 are the Y = baseline * daily.mean1,2,3
  ##   For binomial and multinomial, the dmean1,2,3 or mixed1,2,3 values greater than 
  ##   opening price will be set as 1 and lower will be 0.
  ## 
  ## logistic.yv = TRUE will convert the greater value of X 
  ##   into 1,0 mode but yv == baseline the price of levels c(Op, Hi, Lo, Cl) will be 
  ##   set as 1,2,3,4.
  ## 
  ## setform %in% c('l1', 'l2', 'l3', 'l4') will set a formula for build.x() and build.y().
  ## 
  ## wt is 1 column data frame (a vector) of internal weight values on build.y() Y values. 
  ## 
  ## wt.control = TRUE if internal weighted values applied. For both binomial and 
  ##   multinomial will not be applicable and skip it due to logical Y cannot mlultiplied 
  ##   by a vector of weighted parameters.
  ## 
  
  ## ========================= Load Packages ===================================
  ## h() is a function which build a response variables as refer to below article.
  # https://github.com/englianhu/binary.com-interview-question/blob/master/reference/Stock%20Market%20Forecasting%20Using%20LASSO%20Linear%20Regression%20Model.pdf 
  ##
  ## While the book title : "R for Everyone: Advanced Analytics and Graphics" 
  ##   page275 introduced that the build.x() and build.y() for binomial.
  # https://books.google.co.jp/books?id=EkpvAgAAQBAJ&pg=PA285&dq=cv.glmnet++binomial&hl=en&sa=X&redir_esc=y#v=onepage&q=cv.glmnet%20%20binomial&f=false
  ##
  
  ## http://www.ats.ucla.edu/stat/r/library/contrast_coding.htm
  ## set the first dat as baseline.
  #
  #'@ hsb2 = read.table('http://www.ats.ucla.edu/stat/data/hsb2.csv', header=T, sep=",")
  #
  # creating the factor variable race.f
  #'@ hsb2$race.f = factor(hsb2$race, labels = c("Hispanic", "Asian", "African-Am", "Caucasian"))
  # Before considering any analyses, let's look at the mean of the dependent variable, 
  #  write, for each level of race.  This will help in interpreting the output from later analyses.
  #
  #tapply(hsb2$write, hsb2$race.f, mean)
  #Hispanic Asian African-Am Caucasian 
  #46.45833    58       48.2  54.05517
  #
  #the contrast matrix for categorical variable with four levels
  #contr.treatment(4)
  #2 3 4 
  #1 0 0 0
  #2 1 0 0
  #3 0 1 0
  #4 0 0 1
  #
  ##assigning the treatment contrasts to race.f
  #contrasts(hsb2$race.f) = contr.treatment(4)
  ##the regression
  #summary(lm(write ~ race.f, hsb2))
  
  # http://amunategui.github.io/sparse-matrix-glmnet/
  # {Matrix} - creates sparse/dense matrices
  # {glmnet} - generalized linear models
  # {pROC} - ROC tools
  
  # useful::build.x() will convert the matrix into a dummy variable matrix.
  # https://books.google.co.jp/books?id=EkpvAgAAQBAJ&pg=PA285&dq=cv.glmnet++binomial&hl=en&sa=X&redir_esc=y#v=onepage&q=cv.glmnet%20%20binomial&f=false
  # page273:
  # matrix.model() and sparse.matrix.model() both create a factor class into an 
  #   ordered levels but will but only 0 and 1 if more than 2 levels. However, 
  #   it is generally considered undesirable for the predictor matrix to be 
  #   designed this way for the Elastic Net. It is possible to have model.matrix() 
  #   return indicator variables for all levels of a factor, although doing so can 
  #   take some creative coding. To make the rocess easier we incorporated a solution 
  #   in the build.x() in the 'useful' package.
  options(warn = -1)
  suppressPackageStartupMessages(library("BBmisc"))
  suppressAll(library('plyr'))
  suppressAll(library('tidyverse'))
  suppressAll(library('useful'))
  suppressAll(library('Matrix'))
  
  ## ========================= Data Validation ===================================
  if(is.xts(ddt)) {
    ddt <- as.matrix(ddt) %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
      tbl_df %>% mutate(Date = ymd(Date)) %>% arrange(Date)
    
  } else if(is.data.frame(ddt)) {
    ddt <- ddt
  } else {
    stop('Kindly apply filterLAD and fit the dataset into the function.')
  }
  
  families <- c('gaussian', 'binomial', 'poisson', 'multinomial', 'cox', 'mgaussian')
  if(family %in% families) {
    family <- family
  } else {
    stop('family must be one among c(\'', paste(families, collapse = '\', \''), '\').')
  }
  
  yvs <- c('baseline', 'close1', 'close2', 'daily.mean1', 'daily.mean2', 'daily.mean3', 
           'mixed1', 'mixed2', 'mixed3')
  if(yv %in% yvs) {
    yv <- yv
  } else {
    stop('yv must be one among c(\'', paste(yvs, collapse = '\', \''), '\').')
  }
  
  setforms <- c('l1', 'l2', 'l3', 'l4')
  if(setform %in% setforms) {
    setform <- setform
  } else {
    stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
  }
  
  xy.matries <- c('h1', 'h2')
  if(xy.matrix %in% xy.matries) {
    xy.matrix <- xy.matrix
  } else {
    stop('xy.matrix must be one among c(\'', paste(xy.matries, collapse = '\', \''), '\').')
  }
  
  wt <- wt
  if(!is.null(wt)) {
    if(is.numeric(wt)) {
      ddt$wt <- wt
      rm(wt)
    }
    
    if(is.data.frame(wt)) {
      ddt %<>% cbind(., wt)
    }
    #'@ } else if(anyNA(suppressWarnings(as.numeric(wt)))) {
    #'@   stop('Kindly select a numeric vector as weight parameters.')
    #'@ }
  } else {
    ddt$wt <- 1
    rm(wt)
  }
  
  ## ============================= Regression Model ===================================
  if(family %in% families[c(1, 3)]) {
    ## -------------------------- gaussian or poisson ---------------------------------
    if(yv %in% c('baseline', 'close1', 'close2')) {
      X <- ddt[c('Date', 'LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                 'LAD.Volume', 'wt')] %>% tbl_df
      
    } else if(yv %in%  c('daily.mean1', 'mixed1')) {
      X <- ddt[c('Date', 'LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                 'LAD.Volume', 'wt')] %>% tbl_df %>% 
        mutate(dmean1 = (LAD.Open + LAD.Close) / 2)
      
    } else if(yv %in% c('daily.mean2', 'mixed2')) {
      X <- ddt[c('Date', 'LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                 'LAD.Volume', 'wt')] %>% tbl_df %>% 
        mutate(dmean2 = (LAD.High + LAD.Low) / 2)
      
    } else {
      X <- ddt[c('Date', 'LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                 'LAD.Volume', 'wt')] %>% tbl_df %>% 
        mutate(dmean3 = (LAD.Open + LAD.High + LAD.Low + LAD.Close) / 4)
    }
    
    if(xy.matrix == 'h1') {
      ## ---------------------------------- h1 ----------------------------------------
      ## h1 is long format converted from default quantmmod xts.
      
      if(yv == 'baseline') {
        X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
          mutate(Category = factor(
            Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
          arrange(Date) %>% mutate(b0 = Price / first(Price))
        #let `Date` be numeric variable as convert by system.
        
        wt <- X['wt'] %>% tbl_df
        Y <- X[c('wt', 'b0')] %>% tbl_df
        X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
        
        if(wt.control == TRUE) {
          Y %<>% mutate(b0 = wt * b0)
          Y %<>% .['b0'] %>% tbl_df
        }
        
        if(wt.control == FALSE) {
          Y %<>% .['b0'] %>% tbl_df
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## ------------ baseline formula ---------------------------
        if(setform == 'l1'){
          tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
          
        } else if(setform == 'l2'){
          X <- cbind(X, Y)
          tmp <- list(x = build.x(b0 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), y = Y, wt = wt)
          
        } else if(setform == 'l3'){
          X <- Y <- cbind(X, Y)
          tmp <- list(x = build.x(b0 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = build.y(~ -1 + ., tbl_df(Y['b0'])), wt = wt)
          
        } else if(setform == 'l4'){
          X <- Y <- cbind(X, Y)
          tmp <- list(x = build.x(b0 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = build.y(b0 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
          
        } else {
          stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
        }
      }
      
      if(yv == 'close1') {
        X %<>% gather(Category, Price, LAD.Open:LAD.Low) %>% 
          mutate(Category = factor(
            Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low'))) %>% 
          arrange(Date)
        
        wt <- X['wt'] %>% tbl_df
        Y <- X[c('wt', 'LAD.Close')] %>% tbl_df
        X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
        
        if(wt.control == TRUE) {
          Y %<>% mutate(LAD.Close = wt * LAD.Close)
          Y %<>% .['LAD.Close'] %>% tbl_df
        }
        
        if(wt.control == FALSE) {
          Y %<>% .['LAD.Close'] %>% tbl_df
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## ------------ close1 formula ---------------------------
        if(setform == 'l1'){
          tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
          
        } else if(setform == 'l2'){
          X <- cbind(X, Y)
          tmp <- list(x = build.x(LAD.Close ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = tbl_df(Y['LAD.Close']), wt = wt)
          
        } else if(setform == 'l3'){
          X <- Y <- cbind(X, Y)
          tmp <- list(x = build.x(LAD.Close ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = build.y(~ -1 + ., tbl_df(Y['LAD.Close'])), wt = wt)
          
        } else if(setform == 'l4'){
          X <- Y <- cbind(X, Y)
          tmp <- list(x = build.x(LAD.Close ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = build.y(LAD.Close ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
          
        } else {
          stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
        }
      }
      
      if(yv == 'close2') {
        X %<>% mutate(LAD.Close2 = LAD.Close) %>% 
          gather(Category, Price, LAD.Open:LAD.Close) %>% 
          mutate(Category = factor(
            Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
          arrange(Date)
        
        wt <- X['wt'] %>% tbl_df
        Y <- X %>% mutate(LAD.Close2 = ifelse(LAD.Close2 >= Price, 1, 0))
        Y %<>% .[c('wt', 'LAD.Close2')] %>% tbl_df
        X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
        
        if(wt.control == TRUE) {
          Y %<>% mutate(LAD.Close2 = wt * LAD.Close2)
          Y %<>% .['LAD.Close2'] %>% tbl_df
        }
        
        if(wt.control == FALSE) {
          Y %<>% .['LAD.Close2'] %>% tbl_df
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## ------------ close2 formula ---------------------------
        if(setform == 'l1'){
          tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
          
        } else if(setform == 'l2'){
          X <- cbind(X, Y)
          tmp <- list(x = build.x(LAD.Close2 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = tbl_df(Y['LAD.Close2']), wt = wt)
          
        } else if(setform == 'l3'){
          X <- Y <- cbind(X, Y)
          tmp <- list(x = build.x(LAD.Close2 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = build.y(~ -1 + ., tbl_df(Y['LAD.Close2'])), wt = wt)
          
        } else if(setform == 'l4'){
          X <- Y <- cbind(X, Y)
          tmp <- list(x = build.x(LAD.Close2 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = build.y(LAD.Close2 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
          
        } else {
          stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
        }
      }
      
      if(yv == 'daily.mean1') {
        X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
          mutate(Category = factor(
            Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
          arrange(Date)
        #let `Date` be numeric variable as convert by system.
        
        wt <- X['wt'] %>% tbl_df
        Y <- X[c('wt', 'dmean1')] %>% tbl_df
        X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
        
        if(wt.control == TRUE) {
          Y %<>% mutate(dmean1 = wt * dmean1)
          Y %<>% .['dmean1'] %>% tbl_df
        }
        
        if(wt.control == FALSE) {
          Y %<>% .['dmean1'] %>% tbl_df
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## ------------ daily.mean1 formula ---------------------------
        if(setform == 'l1'){
          tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
          
        } else if(setform == 'l2'){
          X <- cbind(X, Y)
          tmp <- list(x = build.x(dmean1 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = tbl_df(Y['dmean1']), wt = wt)
          
        } else if(setform == 'l3'){
          X <- Y <- cbind(X, Y)
          tmp <- list(x = build.x(dmean1 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = build.y(~ -1 + ., tbl_df(Y['dmean1'])), wt = wt)
          
        } else if(setform == 'l4'){
          X <- Y <- cbind(X, Y)
          tmp <- list(x = build.x(dmean1 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = build.y(dmean1 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
          
        } else {
          stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
        }
      }
      
      if(yv == 'daily.mean2') {
        X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
          mutate(Category = factor(
            Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
          arrange(Date)
        #let `Date` be numeric variable as convert by system.
        
        wt <- X['wt'] %>% tbl_df
        Y <- X[c('wt', 'dmean2')] %>% tbl_df
        X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
        
        if(wt.control == TRUE) {
          Y %<>% mutate(dmean2 = wt * dmean2)
          Y %<>% .['dmean2'] %>% tbl_df
        }
        
        if(wt.control == FALSE) {
          Y %<>% .['dmean2'] %>% tbl_df
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## ------------ daily.mean2 formula ---------------------------
        if(setform == 'l1'){
          tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
          
        } else if(setform == 'l2'){
          X <- cbind(X, Y)
          tmp <- list(x = build.x(dmean2 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = tbl_df(Y['dmean2']), wt = wt)
          
        } else if(setform == 'l3'){
          X <- Y <- cbind(X, Y)
          tmp <- list(x = build.x(dmean2 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = build.y(~ -1 + ., tbl_df(Y['dmean2'])), wt = wt)
          
        } else if(setform == 'l4'){
          X <- Y <- cbind(X, Y)
          tmp <- list(x = build.x(dmean2 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = build.y(dmean2 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
          
        } else {
          stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
        }
      }
      
      if(yv == 'daily.mean3') {
        X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
          mutate(Category = factor(
            Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
          arrange(Date)
        #let `Date` be numeric variable as convert by system.
        
        wt <- X['wt'] %>% tbl_df
        Y <- X[c('wt', 'dmean3')] %>% tbl_df
        X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
        
        if(wt.control == TRUE) {
          Y %<>% mutate(dmean3 = wt * dmean3)
          Y %<>% .['dmean3'] %>% tbl_df
        }
        
        if(wt.control == FALSE) {
          Y %<>% .['dmean3'] %>% tbl_df
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## ------------ daily.mean3 formula ---------------------------
        if(setform == 'l1'){
          tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
          
        } else if(setform == 'l2'){
          X <- cbind(X, Y)
          tmp <- list(x = build.x(dmean3 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = tbl_df(Y['dmean3']), wt = wt)
          
        } else if(setform == 'l3'){
          X <- Y <- cbind(X, Y)
          tmp <- list(x = build.x(dmean3 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = build.y(~ -1 + ., tbl_df(Y['dmean3'])), wt = wt)
          
        } else if(setform == 'l4'){
          X <- Y <- cbind(X, Y)
          tmp <- list(x = build.x(dmean3 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = build.y(dmean3 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
          
        } else {
          stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
        }
      }
      
      if(yv == 'mixed1') {
        X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
          mutate(Category = factor(
            Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
          arrange(Date)
        #let `Date` be numeric variable as convert by system.
        
        wt <- X['wt'] %>% tbl_df
        Y <- X %>% mutate(mixed1 = (Price / first(Price)) * dmean1)
        Y %<>% .[c('wt', 'mixed1')] %>% tbl_df
        X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
        
        if(wt.control == TRUE) {
          Y %<>% mutate(mixed1 = wt * mixed1)
          Y %<>% .['mixed1'] %>% tbl_df
        }
        
        if(wt.control == FALSE) {
          Y %<>% .['mixed1'] %>% tbl_df
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## ------------ mixed1 formula ---------------------------
        if(setform == 'l1'){
          tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
          
        } else if(setform == 'l2'){
          X <- cbind(X, Y)
          tmp <- list(x = build.x(mixed1 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = tbl_df(Y['mixed1']), wt = wt)
          
        } else if(setform == 'l3'){
          X <- Y <- cbind(X, Y)
          tmp <- list(x = build.x(mixed1 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = build.y(~ -1 + ., tbl_df(Y['mixed1'])), wt = wt)
          
        } else if(setform == 'l4'){
          X <- Y <- cbind(X, Y)
          tmp <- list(x = build.x(mixed1 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = build.y(mixed1 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
          
        } else {
          stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
        }
      }
      
      if(yv == 'mixed2') {
        X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
          mutate(Category = factor(
            Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
          arrange(Date)
        #let `Date` be numeric variable as convert by system.
        
        wt <- X['wt'] %>% tbl_df
        Y <- X %>% mutate(mixed2 = (Price / first(Price)) * dmean2)
        Y %<>% .[c('wt', 'mixed2')] %>% tbl_df
        X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
        
        if(wt.control == TRUE) {
          Y %<>% mutate(mixed2 = wt * mixed2)
          Y %<>% .['mixed2'] %>% tbl_df
        }
        
        if(wt.control == FALSE) {
          Y %<>% .['mixed2'] %>% tbl_df
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## ------------ mixed2 formula ---------------------------
        if(setform == 'l1'){
          tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
          
        } else if(setform == 'l2'){
          X <- cbind(X, Y)
          tmp <- list(x = build.x(mixed2 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = tbl_df(Y['mixed2']), wt = wt)
          
        } else if(setform == 'l3'){
          X <- Y <- cbind(X, Y)
          tmp <- list(x = build.x(mixed2 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = build.y(~ -1 + ., tbl_df(Y['mixed2'])), wt = wt)
          
        } else if(setform == 'l4'){
          X <- Y <- cbind(X, Y)
          tmp <- list(x = build.x(mixed2 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = build.y(mixed2 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
          
        } else {
          stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
        }
      }
      
      if(yv == 'mixed3') {
        X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
          mutate(Category = factor(
            Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
          arrange(Date)
        #let `Date` be numeric variable as convert by system.
        
        wt <- X['wt'] %>% tbl_df
        Y <- X %>% mutate(mixed3 = (Price / first(Price)) * dmean3)
        Y %<>% .[c('wt', 'mixed3')] %>% tbl_df
        X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
        
        if(wt.control == TRUE) {
          Y %<>% mutate(mixed3 = wt * mixed3)
          Y %<>% .['mixed3'] %>% tbl_df
        }
        
        if(wt.control == FALSE) {
          Y %<>% .['mixed3'] %>% tbl_df
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## ------------ mixed3 formula ---------------------------
        if(setform == 'l1'){
          tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
          
        } else if(setform == 'l2'){
          X <- cbind(X, Y)
          tmp <- list(x = build.x(mixed3 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = tbl_df(Y['mixed3']), wt = wt)
          
        } else if(setform == 'l3'){
          X <- Y <- cbind(X, Y)
          tmp <- list(x = build.x(mixed3 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = build.y(~ -1 + ., tbl_df(Y['mixed3'])), wt = wt)
          
        } else if(setform == 'l4'){
          X <- Y <- cbind(X, Y)
          tmp <- list(x = build.x(mixed3 ~ LAD.Volume + Category + Price - 1, X, 
                                  contrasts = TRUE, sparse = TRUE), 
                      y = build.y(mixed3 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
          
        } else {
          stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
        }
      }
      
      #'@ if(.log == TRUE) {
      #'@   X %<>% mutate_each(funs(log))
      #'@   Y %<>% mutate_each(funs(log))
      #'@ }
      
      #set X_i0 as baseline (intercept).
      #'@ tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
      #'@ suppressWarnings(build.x(~ -1 + ., ddt_DF, contrasts = TRUE, sparse = TRUE))
      
    } else if(xy.matrix == 'h2') {
      ## ---------------------------------- h2 ---------------------------------------
      ## h2 is wide format or default quantmmod xts.
      
      if(yv == 'baseline') {
        wt <- X['wt'] %>% tbl_df
        Y <- X %>% mutate(b0 = LAD.Open / first(LAD.Open))
        Y %<>% .[c('wt', 'b0')] %>% tbl_df
        X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                   'LAD.Volume')] %>% tbl_df
        
        if(wt.control == TRUE) {
          Y %<>% mutate(b0 = wt * b0)
          Y %<>% .['b0'] %>% tbl_df
        }
        
        if(wt.control == FALSE) {
          Y %<>% .['b0'] %>% tbl_df
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## ------------ baseline formula ---------------------------
        if(setform == 'l1'){
          tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
          
        } else if(setform == 'l2'){
          X <- cbind(X, Y)
          tmp <- list(
            x = build.x(b0 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = tbl_df(Y['b0']), wt = wt)
          
        } else if(setform == 'l3'){
          X <- Y <- cbind(X, Y)
          tmp <- list(
            x = build.x(b0 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = build.y(~ -1 + ., tbl_df(Y['b0'])), wt = wt)
          
        } else if(setform == 'l4'){
          X <- Y <- cbind(X, Y)
          tmp <- list(
            x = build.x(b0 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = build.y(b0 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, Y), wt = wt)
          
        } else {
          stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
        }
      }
      
      if(yv == 'close1') {
        wt <- X['wt'] %>% tbl_df
        Y <- X[c('wt', 'LAD.Close')] %>% tbl_df
        X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Volume')] %>% tbl_df
        
        if(wt.control == TRUE) {
          Y %<>% mutate(LAD.Close = wt * LAD.Close)
          Y %<>% .['LAD.Close'] %>% tbl_df
        }
        
        if(wt.control == FALSE) {
          Y %<>% .['LAD.Close'] %>% tbl_df
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## ------------ close1 formula ---------------------------
        if(setform == 'l1'){
          tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
          
        } else if(setform == 'l2'){
          X <- cbind(X, Y)
          tmp <- list(
            x = build.x(LAD.Close ~ LAD.Open + LAD.High + LAD.Low + LAD.Volume - 1, X, 
                        contrasts = TRUE, sparse = TRUE), 
            y = tbl_df(Y['LAD.Close']), wt = wt)
          
        } else if(setform == 'l3'){
          X <- Y <- cbind(X, Y)
          tmp <- list(
            x = build.x(LAD.Close ~ LAD.Open + LAD.High + LAD.Low + LAD.Volume - 1, X, 
                        contrasts = TRUE, sparse = TRUE), 
            y = build.y(~ -1 + ., tbl_df(Y['LAD.Close'])), wt = wt)
          
        } else if(setform == 'l4'){
          X <- Y <- cbind(X, Y)
          tmp <- list(
            x = build.x(LAD.Close ~ LAD.Open + LAD.High + LAD.Low + LAD.Volume - 1, X, 
                        contrasts = TRUE, sparse = TRUE), 
            y = build.y(LAD.Close ~ LAD.Open + LAD.High + LAD.Low + LAD.Volume - 1, Y), wt = wt)
          
        } else {
          stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
        }
      }
      
      if(yv == 'close2') {
        wt <- X['wt'] %>% tbl_df
        Y <- X %>% mutate(LAD.Close2 = LAD.Close) %>% .[c('wt', 'LAD.Close2')] %>% tbl_df
        X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                   'LAD.Volume')] %>% tbl_df
        
        if(wt.control == TRUE) {
          Y %<>% mutate(LAD.Close2 = wt * LAD.Close2)
          Y %<>% .['LAD.Close2'] %>% tbl_df
        }
        
        if(wt.control == FALSE) {
          Y %<>% .['LAD.Close2'] %>% tbl_df
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## ------------ close2 formula ---------------------------
        if(setform == 'l1'){
          tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
          
        } else if(setform == 'l2'){
          X <- cbind(X, Y)
          tmp <- list(
            x = build.x(LAD.Close2 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = tbl_df(Y['LAD.Close2']), wt = wt)
          
        } else if(setform == 'l3'){
          X <- Y <- cbind(X, Y)
          tmp <- list(
            x = build.x(LAD.Close2 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = build.y(~ -1 + ., tbl_df(Y['LAD.Close2'])), wt = wt)
          
        } else if(setform == 'l4'){
          X <- Y <- cbind(X, Y)
          tmp <- list(
            x = build.x(LAD.Close2 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = build.y(LAD.Close2 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, Y), wt = wt)
          
        } else {
          stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
        }
      }
      
      if(yv == 'daily.mean1') {
        wt <- X['wt'] %>% tbl_df
        Y <- X[c('wt', 'dmean1')] %>% tbl_df
        X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                   'LAD.Volume')] %>% tbl_df
        
        if(wt.control == TRUE) {
          Y %<>% mutate(dmean1 = wt * dmean1)
          Y %<>% .['dmean1'] %>% tbl_df
        }
        
        if(wt.control == FALSE) {
          Y %<>% .['dmean1'] %>% tbl_df
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## ------------ daily.mean1 formula ---------------------------
        if(setform == 'l1'){
          tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
          
        } else if(setform == 'l2'){
          X <- cbind(X, Y)
          tmp <- list(
            x = build.x(dmean1 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = tbl_df(Y['dmean1']), wt = wt)
          
        } else if(setform == 'l3'){
          X <- Y <- cbind(X, Y)
          tmp <- list(
            x = build.x(dmean1 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = build.y(~ -1 + ., tbl_df(Y['dmean1'])), wt = wt)
          
        } else if(setform == 'l4'){
          X <- Y <- cbind(X, Y)
          tmp <- list(
            x = build.x(dmean1 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = build.y(dmean1 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, Y), wt = wt)
          
        } else {
          stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
        }
      }
      
      if(yv == 'daily.mean2') {
        wt <- X['wt'] %>% tbl_df
        Y <- X[c('wt', 'dmean2')] %>% tbl_df
        X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                   'LAD.Volume')] %>% tbl_df
        
        if(wt.control == TRUE) {
          Y %<>% mutate(dmean2 = wt * dmean2)
          Y %<>% .['dmean2'] %>% tbl_df
        }
        
        if(wt.control == FALSE) {
          Y %<>% .['dmean2'] %>% tbl_df
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## ------------ daily.mean2 formula ---------------------------
        if(setform == 'l1'){
          tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
          
        } else if(setform == 'l2'){
          X <- cbind(X, Y)
          tmp <- list(
            x = build.x(dmean2 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = tbl_df(Y['dmean2']), wt = wt)
          
        } else if(setform == 'l3'){
          X <- Y <- cbind(X, Y)
          tmp <- list(
            x = build.x(dmean2 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = build.y(~ -1 + ., tbl_df(Y['dmean2'])), wt = wt)
          
        } else if(setform == 'l4'){
          X <- Y <- cbind(X, Y)
          tmp <- list(
            x = build.x(dmean2 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = build.y(dmean2 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, Y), wt = wt)
          
        } else {
          stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
        }
      }
      
      if(yv == 'daily.mean3') {
        wt <- X['wt'] %>% tbl_df
        Y <- X[c('wt', 'dmean3')] %>% tbl_df
        X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                   'LAD.Volume')] %>% tbl_df
        
        if(wt.control == TRUE) {
          Y %<>% mutate(dmean3 = wt * dmean3)
          Y %<>% .['dmean3'] %>% tbl_df
        }
        
        if(wt.control == FALSE) {
          Y %<>% .['dmean3'] %>% tbl_df
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## ------------ daily.mean3 formula ---------------------------
        if(setform == 'l1'){
          tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
          
        } else if(setform == 'l2'){
          X <- cbind(X, Y)
          tmp <- list(
            x = build.x(dmean3 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = tbl_df(Y['dmean3']), wt = wt)
          
        } else if(setform == 'l3'){
          X <- Y <- cbind(X, Y)
          tmp <- list(
            x = build.x(dmean3 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = build.y(~ -1 + ., tbl_df(Y['dmean3'])), wt = wt)
          
        } else if(setform == 'l4'){
          X <- Y <- cbind(X, Y)
          tmp <- list(
            x = build.x(dmean3 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = build.y(dmean3 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, Y), wt = wt)
          
        } else {
          stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
        }
      }
      
      if(yv == 'mixed1') {
        wt <- X['wt'] %>% tbl_df
        Y <- X %>% mutate(mixed1 = (LAD.Close / first(LAD.Open)) * dmean1)
        Y %<>% .[c('wt', 'mixed1')] %>% tbl_df
        X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                   'LAD.Volume')] %>% tbl_df
        
        if(wt.control == TRUE) {
          Y %<>% mutate(mixed1 = wt * mixed1)
          Y %<>% .['mixed1'] %>% tbl_df
        }
        
        if(wt.control == FALSE) {
          Y %<>% .['mixed1'] %>% tbl_df
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## ------------ mixed1 formula ---------------------------
        if(setform == 'l1'){
          tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
          
        } else if(setform == 'l2'){
          X <- cbind(X, Y)
          tmp <- list(
            x = build.x(mixed1 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = tbl_df(Y['mixed1']), wt = wt)
          
        } else if(setform == 'l3'){
          X <- Y <- cbind(X, Y)
          tmp <- list(
            x = build.x(mixed1 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = build.y(~ -1 + ., tbl_df(Y['mixed1'])), wt = wt)
          
        } else if(setform == 'l4'){
          X <- Y <- cbind(X, Y)
          tmp <- list(
            x = build.x(mixed1 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = build.y(mixed1 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, Y), wt = wt)
          
        } else {
          stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
        }
      }
      
      if(yv == 'mixed2') {
        wt <- X['wt'] %>% tbl_df
        Y <- X %>% mutate(mixed2 = (LAD.Close / first(LAD.Open)) * dmean2)
        Y %<>% .[c('wt', 'mixed2')] %>% tbl_df
        X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                   'LAD.Volume')] %>% tbl_df
        
        if(wt.control == TRUE) {
          Y %<>% mutate(mixed2 = wt * mixed2)
          Y %<>% .['mixed2'] %>% tbl_df
        }
        
        if(wt.control == FALSE) {
          Y %<>% .['mixed2'] %>% tbl_df
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## ------------ mixed2 formula ---------------------------
        if(setform == 'l1'){
          tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
          
        } else if(setform == 'l2'){
          X <- cbind(X, Y)
          tmp <- list(
            x = build.x(mixed2 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = tbl_df(Y['mixed2']), wt = wt)
          
        } else if(setform == 'l3'){
          X <- Y <- cbind(X, Y)
          tmp <- list(
            x = build.x(mixed2 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = build.y(~ -1 + ., tbl_df(Y['mixed2'])), wt = wt)
          
        } else if(setform == 'l4'){
          X <- Y <- cbind(X, Y)
          tmp <- list(
            x = build.x(mixed2 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = build.y(mixed2 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, Y), wt = wt)
          
        } else {
          stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
        }
      }
      
      if(yv == 'mixed3') {
        wt <- X['wt'] %>% tbl_df
        Y <- X %>% mutate(mixed3 = (LAD.Close / first(LAD.Open)) * dmean3)
        Y %<>% .[c('wt', 'mixed3')] %>% tbl_df
        X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                   'LAD.Volume')] %>% tbl_df
        
        if(wt.control == TRUE) {
          Y %<>% mutate(mixed3 = wt * mixed3)
          Y %<>% .['mixed3'] %>% tbl_df
        }
        
        if(wt.control == FALSE) {
          Y %<>% .['mixed3'] %>% tbl_df
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## ------------ mixed3 formula ---------------------------
        if(setform == 'l1'){
          tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
          
        } else if(setform == 'l2'){
          X <- cbind(X, Y)
          tmp <- list(
            x = build.x(mixed3 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = tbl_df(Y['mixed3']), wt = wt)
          
        } else if(setform == 'l3'){
          X <- Y <- cbind(X, Y)
          tmp <- list(
            x = build.x(mixed3 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = build.y(~ -1 + ., tbl_df(Y['mixed3'])), wt = wt)
          
        } else if(setform == 'l4'){
          X <- Y <- cbind(X, Y)
          tmp <- list(
            x = build.x(mixed3 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, 
                        X, contrasts = TRUE, sparse = TRUE), 
            y = build.y(mixed3 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + LAD.Volume - 1, Y), wt = wt)
          
        } else {
          stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
        }
      }
      
      #'@ if(.log == TRUE) {
      #'@   X %<>% mutate_each(funs(log))
      #'@   Y %<>% mutate_each(funs(log))
      #'@ }
      
      #set X_i0 as baseline (intercept).
      #'@ tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
      #'@ suppressWarnings(build.x(~ -1 + ., ddt_DF, contrasts = TRUE, sparse = TRUE))
      
    } else {
      stop('Kindly set xy.matrix = "h1" or xy.matrix = "h2".')
    }
    
    ## --------------------------- return function ---------------------------------
    options(warn = 0)
    return(tmp)
    
  } else if(family %in% families[c(2, 4)]) {
    ## ----------------------- binomial or multinomial -------------------------------
    if(yv %in% c('baseline', 'close1', 'close2')) {
      X <- ddt[c('Date', 'LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                 'LAD.Volume', 'wt')] %>% tbl_df
      
    } else if(yv %in%  c('daily.mean1', 'mixed1')) {
      X <- ddt[c('Date', 'LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                 'LAD.Volume', 'wt')] %>% tbl_df %>% 
        mutate(dmean1 = (LAD.Open + LAD.Close) / 2)
      
    } else if(yv %in% c('daily.mean2', 'mixed2')) {
      X <- ddt[c('Date', 'LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                 'LAD.Volume', 'wt')] %>% tbl_df %>% 
        mutate(dmean2 = (LAD.High + LAD.Low) / 2)
      
    } else {
      X <- ddt[c('Date', 'LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                 'LAD.Volume', 'wt')] %>% tbl_df %>% 
        mutate(dmean3 = (LAD.Open + LAD.High + LAD.Low + LAD.Close) / 4)
    }
    
    if(logistic.yv == TRUE) {
      if(xy.matrix == 'h1') {
        ## ---------------------------------- h1 ---------------------------------------
        ## h1 is long format converted from default quantmmod xts.
        
        ## http://stats.stackexchange.com/questions/136085/is-it-posible-to-use-factor-categorical-variables-in-glmnet-for-logistic-regre
        ## glmnet cannot take factor directly, you need to transform factor variables to dummies. 
        ##    It is only one simple step using model.matrix, for instance:
        # 
        #'@ x_train <- model.matrix( ~ .-1, train[,features])
        #'@ lm = cv.glmnet(x = x_train, y = as.factor(train$y), intercept = FALSE, 
        #'@                family = "binomial", alpha = 1, nfolds = 7)
        #'@ best_lambda <- lm$lambda[which.min(lm$cvm)]
        # 
        ## alpha=1 will build a LASSO.
        
        if(yv == 'baseline') {
          X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
            mutate(Category = factor(
              Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
            arrange(Date) %>% mutate(b0 = Price / first(Price))
          #let `Date` be numeric variable as convert by system.
          
          wt <- X['wt'] %>% tbl_df
          Y <- X['Category'] %>% tbl_df
          X %<>% .[c('LAD.Volume', 'b0', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ baseline formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(x = build.x(Category ~ LAD.Volume + b0 + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), y = Y, wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(Category ~ LAD.Volume + b0 + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(~ -1 + ., tbl_df(Y['Category'])))
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(Category ~ LAD.Volume + b0 + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(Category ~ LAD.Volume + b0 + Price - 1, Y))
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'close1') {
          X %<>% mutate(LAD.Close = ifelse(LAD.Close > LAD.Open, 1, 0)) %>% 
            gather(Category, Price, LAD.Open:LAD.Low) %>% 
            mutate(Category = factor(
              Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low'))) %>% 
            arrange(Date)
          
          wt <- X['wt'] %>% tbl_df
          Y <- X['LAD.Close'] %>% tbl_df
          X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ close1 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(x = build.x(LAD.Close ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = tbl_df(Y['LAD.Close']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(LAD.Close ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(~ -1 + ., tbl_df(Y['LAD.Close'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(LAD.Close ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(LAD.Close ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'close2') {
          X %<>% mutate(LAD.Close2 = ifelse(LAD.Close > LAD.Open, 1, 0)) %>% 
            gather(Category, Price, LAD.Open:LAD.Close) %>% 
            mutate(Category = factor(
              Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
            arrange(Date)
          
          wt <- X['wt'] %>% tbl_df
          Y <- X['LAD.Close2'] %>% tbl_df
          X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ close2 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(x = build.x(LAD.Close2 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = tbl_df(Y['LAD.Close2']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(LAD.Close2 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(~ -1 + ., tbl_df(Y['LAD.Close2'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(LAD.Close2 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(LAD.Close2 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'daily.mean1') {
          X %<>% mutate(dmean1 = ifelse(dmean1 > LAD.Open, 1, 0)) %>% 
            gather(Category, Price, LAD.Open:LAD.Close) %>% 
            mutate(Category = factor(
              Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
            arrange(Date)
          #let `Date` be numeric variable as convert by system.
          
          wt <- X['wt'] %>% tbl_df
          Y <- X['dmean1'] %>% tbl_df
          X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ daily.mean1 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(x = build.x(dmean1 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = tbl_df(Y['dmean1']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(dmean1 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(~ -1 + ., tbl_df(Y['dmean1'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(dmean1 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(dmean1 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'daily.mean2') {
          X %<>% mutate(dmean2 = ifelse(dmean2 > LAD.Open, 1, 0)) %>% 
            gather(Category, Price, LAD.Open:LAD.Close) %>% 
            mutate(Category = factor(
              Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
            arrange(Date)
          #let `Date` be numeric variable as convert by system.
          
          wt <- X['wt'] %>% tbl_df
          Y <- X['dmean2'] %>% tbl_df
          X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ daily.mean2 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(x = build.x(dmean2 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = tbl_df(Y['dmean2']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(dmean2 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(~ -1 + ., tbl_df(Y['dmean2'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(dmean2 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(dmean2 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'daily.mean3') {
          X %<>% mutate(dmean3 = ifelse(dmean3 > LAD.Open, 1, 0)) %>% 
            gather(Category, Price, LAD.Open:LAD.Close) %>% 
            mutate(Category = factor(
              Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
            arrange(Date)
          #let `Date` be numeric variable as convert by system.
          
          wt <- X['wt'] %>% tbl_df
          Y <- X['dmean3'] %>% tbl_df
          X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ daily.mean3 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(x = build.x(dmean3 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = tbl_df(Y['dmean3']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(dmean3 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(~ -1 + ., tbl_df(Y['dmean3'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(dmean3 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(dmean3 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'mixed1') {
          X %<>% mutate(LAD.Open2 = LAD.Open) %>% 
            gather(Category, Price, LAD.Open:LAD.Close) %>% 
            mutate(Category = factor(
              Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
            arrange(Date)
          #let `Date` be numeric variable as convert by system.
          
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(mixed1 = (Price / first(Price)) * dmean1, 
                            mixed1 = ifelse(mixed1 > LAD.Open2, 1, 0))
          Y %<>% .['mixed1'] %>% tbl_df
          X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ mixed1 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(x = build.x(mixed1 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = tbl_df(Y['mixed1']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(mixed1 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(~ -1 + ., tbl_df(Y['mixed1'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(mixed1 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(mixed1 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'mixed2') {
          X %<>% mutate(LAD.Open2 = LAD.Open) %>% 
            gather(Category, Price, LAD.Open:LAD.Close) %>% 
            mutate(Category = factor(
              Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
            arrange(Date)
          #let `Date` be numeric variable as convert by system.
          
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(mixed2 = (Price / first(Price)) * dmean2, 
                            mixed2 = ifelse(mixed2 > LAD.Open2, 1, 0))
          Y %<>% .['mixed2'] %>% tbl_df
          X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ mixed2 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(x = build.x(mixed2 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = tbl_df(Y['mixed2']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(mixed2 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(~ -1 + ., tbl_df(Y['mixed2'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(mixed2 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(mixed2 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'mixed3') {
          X %<>% mutate(LAD.Open2 = LAD.Open) %>% 
            gather(Category, Price, LAD.Open:LAD.Close) %>% 
            mutate(Category = factor(
              Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
            arrange(Date)
          #let `Date` be numeric variable as convert by system.
          
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(mixed3 = (Price / first(Price)) * dmean3, 
                            mixed3 = ifelse(mixed3 > LAD.Open2, 1, 0))
          Y %<>% .['mixed3'] %>% tbl_df
          X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ mixed3 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(x = build.x(mixed3 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = tbl_df(Y['mixed3']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(mixed3 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(~ -1 + ., tbl_df(Y['mixed3'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(mixed3 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(mixed3 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        ## sample for contrasts :
        #'@ contrasts(LADDT_DF$Category) <- contr.treatment(LADDT_DF$Category)
        #'@ attr(LADDT_DF$Category, 'levels') <- c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')
        #'@ attr(LADDT_DF$Category,'contrasts') <- contrasts(C(factor(LADDT_DF$Category), 
        #'@                                                    'contr.treatment'))
        #'@ tmp <- model.matrix(Category ~ Date + Price + wt + b0, data = LADDT_DF) %>% 
        #'@   tbl_df %>% mutate(Category = LADDT_DF$Category)
        
        # book title : "R for Everyone: Advanced Analytics and Graphics"
        # useful::build.x() will convert the matrix into a dummy variable matrix.
        # https://books.google.co.jp/books?id=EkpvAgAAQBAJ&pg=PA285&dq=cv.glmnet++binomial&hl=en&sa=X&redir_esc=y#v=onepage&q=cv.glmnet%20%20binomial&f=false
        # page273:
        # matrix.model() and sparse.matrix.model() both create a factor class into an 
        #   ordered levels but will but only 0 and 1 if more than 2 levels. However, 
        #   it is generally considered undesirable for the predictor matrix to be 
        #   designed this way for the Elastic Net. It is possible to have model.matrix() 
        #   return indicator variables for all levels of a factor, although doing so can 
        #   take some creative coding. To make the rocess easier we incorporated a solution 
        #   in the build.x() in the 'useful' package.
        # 
        # http://stackoverflow.com/questions/4560459/all-levels-of-a-factor-in-a-model-matrix-in-r
        #'@ testFrame <- data.frame(First = sample(1:10, 20, replace = TRUE),
        #'@                         Second = sample(1:20, 20, replace = TRUE), 
        #'@                         Third = sample(1:10, 20, replace = TRUE),
        #'@                         Fourth = rep(c("Alice", "Bob", "Charlie", "David"), 5),
        #'@                         Fifth = rep(c("Edward", "Frank", "Georgia", "Hank", "Isaac"), 4))
        # 
        # You need to reset the contrasts for the factor variables:
        #'@ model.matrix(~ Fourth + Fifth, data = testFrame, 
        #'@              contrasts.arg = list(Fourth = contrasts(testFrame$Fourth, contrasts = FALSE), 
        #'@                                   Fifth = contrasts(testFrame$Fifth, contrasts = FALSE)))
        # 
        #   or, with a little less typing and without the proper names:
        # 
        #'@ model.matrix(~ Fourth + Fifth, data = testFrame, 
        #'@              contrasts.arg = list(Fourth = diag(nlevels(testFrame$Fourth)), 
        #'@                                   Fifth = diag(nlevels(testFrame$Fifth))))
        # 
        #'@ model.matrix(~ ., data = testFrame, 
        #'@              contrasts.arg = lapply(testFrame[, 4:5], contrasts, contrasts = FALSE))
        # 
        # page274
        #'@ require('useful')
        # always use all levels
        #'@ build.x(First ~ Second + Fourth + Fifth, textFrame, contrasts = FALSE)
        # 
        # just use all levels for Fourth
        #'@ build.x(First ~ Second + Fourth + Fifth, testFrame, 
        #'@         contrasts = c(Fourth = FALSE, Fifth = TRUE))
        # 
        # Using build.x appropriately on `acs` dataset builds a nice predictor matrix for use in 
        #   glmnet. We control the desired matrix by using a formula for our model 
        #   specification just like we would in lm, interactions and all.
        # 
        # make a binary Income variable for building a logistic regression
        #'@ acs$Income <- with(acs, FamilyIncome >= 150000)
        # 
        # page275
        # build predictor matrix
        # do not include the intercept as glmnet will add that automatically
        #'@ acsX <- build.x(Income ~ NumBedrooms + NumChildren + NumPeople + 
        #'@                 NumRooms + NumUnits + NumVehicles + NumWorkers + 
        #'@                 OwnRent + YearBuilt + ElectricBill + FoodStamp + 
        #'@                 HeatingFuel + Insurance + Language - 1, 
        #'@                 data = acs, contrasts = FALSE)
        # 
        # check class and dimensions
        #'@ class(acsX)
        #[1] "matrix"
        #'@ dim(acsX)
        #[1] 22745 44
        # 
        # page275
        # view the top left and top right of the data
        #'@ topleft(acsX, c = 6)
        #'@ topright(acsX, c = 6)
        #
        # build response predictor
        #'@ acsY <- build.y(Income ~ NumBedrooms + NumChildren + NumPeople + 
        #'@                 NumRooms + NumUnits + NumVehicles + NumWorkers + 
        #'@                 OwnRent + YearBuilt + ElectricBill + FoodStamp + 
        #'@                 HeatingFuel + Insurance + Language - 1, data = acs)
        #
        #'@ head(acsY)
        #'@ tail(acsY)
        # 
        
        #'@ if(.log == TRUE) {
        #'@   X %<>% mutate_each(funs(log))
        #'@   Y %<>% mutate_each(funs(log))
        #'@ }
        
        #set X_i0 as baseline (intercept).
        #'@ tmp <- list(x = build.x(~ -1 + ., X), y = Y)
        #'@ suppressWarnings(build.x(~ -1 + ., X, contrasts = TRUE))
        
      } else if(xy.matrix == 'h2') {
        ## ---------------------------------- h2 ---------------------------------------
        ## h2 is wide format or default quantmmod xts.
        
        if(yv == 'baseline') {
          X[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')] <- 
            data.frame(as.matrix(X[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')]) / 
                         first(X$LAD.Open)) %>% tbl_df
          
          X %<>% mutate(
            Price = first(LAD.Open), 
            b0 = ifelse(LAD.Open >= first(LAD.Open), 1, 0), 
            LAD.Open = ifelse(LAD.Open >= first(LAD.Open), 1, 0), 
            LAD.High = ifelse(LAD.High >= first(LAD.Open), 1, 0), 
            LAD.Low = ifelse(LAD.Low >= first(LAD.Open), 1, 0), 
            LAD.Close = ifelse(LAD.Close >= first(LAD.Open), 1, 0))
          
          wt <- X['wt'] %>% tbl_df
          Y <- X['b0'] %>% tbl_df
          X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                     'LAD.Volume', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ baseline formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(
              x = build.x(b0 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + 
                            LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = tbl_df(Y['b0']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(b0 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + 
                            LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(~ -1 + ., tbl_df(Y['b0'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(b0 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + 
                            LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(b0 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + 
                            LAD.Volume + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'close1') {
          X[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')] <- 
            data.frame(as.matrix(X[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')]) / 
                         first(X$LAD.Open)) %>% tbl_df
          
          X %<>% mutate(
            Price = LAD.Open, 
            LAD.Open = ifelse(LAD.Open >= LAD.Open, 1, 0), 
            LAD.High = ifelse(LAD.High >= LAD.Open, 1, 0), 
            LAD.Low = ifelse(LAD.Low >= LAD.Open, 1, 0), 
            LAD.Close = ifelse(LAD.Close >= LAD.Open, 1, 0))
          
          wt <- X['wt'] %>% tbl_df
          Y <- X['LAD.Close'] %>% tbl_df
          X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Volume', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ close1 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(
              x = build.x(LAD.Close ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Volume + Price - 1, X, 
                          contrasts = TRUE, sparse = TRUE), 
              y = tbl_df(Y['LAD.Close']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(LAD.Close ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Volume + Price - 1, X, 
                          contrasts = TRUE, sparse = TRUE), 
              y = build.y(~ -1 + ., tbl_df(Y['LAD.Close'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(LAD.Close ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Volume + Price - 1, X, 
                          contrasts = TRUE, sparse = TRUE), 
              y = build.y(LAD.Close ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Volume + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'close2') {
          X[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')] <- 
            data.frame(as.matrix(X[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')]) / 
                         first(X$LAD.Open)) %>% tbl_df
          
          X %<>% mutate(
            Price = LAD.Open, LAD.Close2 = LAD.Close, 
            LAD.Open = ifelse(LAD.Open >= LAD.Open, 1, 0), 
            LAD.High = ifelse(LAD.High >= LAD.Open, 1, 0), 
            LAD.Low = ifelse(LAD.Low >= LAD.Open, 1, 0), 
            LAD.Close = ifelse(LAD.Close >= LAD.Open, 1, 0))
          
          wt <- X['wt'] %>% tbl_df
          Y <- X['LAD.Close2'] %>% tbl_df
          X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                     'LAD.Volume', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ close2 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(
              x = build.x(LAD.Close2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = tbl_df(Y['LAD.Close2']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(LAD.Close2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(~ -1 + ., tbl_df(Y['LAD.Close2'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(LAD.Close2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(LAD.Close2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'daily.mean1') {
          X[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')] <- 
            data.frame(as.matrix(X[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')]) / 
                         first(X$LAD.Open)) %>% tbl_df
          
          X %<>% mutate(
            Price = LAD.Open, 
            dmean1 = ifelse(dmean1 >= LAD.Open, 1, 0), 
            LAD.Open = ifelse(LAD.Open >= dmean1, 1, 0), 
            LAD.High = ifelse(LAD.High >= dmean1, 1, 0), 
            LAD.Low = ifelse(LAD.Low >= dmean1, 1, 0), 
            LAD.Close = ifelse(LAD.Close >= dmean1, 1, 0))
          
          wt <- X['wt'] %>% tbl_df
          Y <- X['dmean1'] %>% tbl_df
          X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                     'LAD.Volume', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ daily.mean1 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(
              x = build.x(dmean1 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = tbl_df(Y['dmean1']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(dmean1 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(~ -1 + ., tbl_df(Y['dmean1'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(dmean1 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(dmean1 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'daily.mean2') {
          X[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')] <- 
            data.frame(as.matrix(X[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')]) / 
                         first(X$LAD.Open)) %>% tbl_df
          
          X %<>% mutate(
            Price = LAD.Open, 
            dmean2 = ifelse(dmean2 >= LAD.Open, 1, 0), 
            LAD.Open = ifelse(LAD.Open >= dmean2, 1, 0), 
            LAD.High = ifelse(LAD.High >= dmean2, 1, 0), 
            LAD.Low = ifelse(LAD.Low >= dmean2, 1, 0), 
            LAD.Close = ifelse(LAD.Close >= dmean2, 1, 0))
          
          wt <- X['wt'] %>% tbl_df
          Y <- X['dmean2'] %>% tbl_df
          X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                     'LAD.Volume', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ daily.mean2 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(
              x = build.x(dmean2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = tbl_df(Y['dmean2']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(dmean2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(~ -1 + ., tbl_df(Y['dmean2'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(dmean2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(dmean2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'daily.mean3') {
          X[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')] <- 
            data.frame(as.matrix(X[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')]) / 
                         first(X$LAD.Open)) %>% tbl_df
          
          X %<>% mutate(
            Price = LAD.Open, 
            dmean3 = ifelse(dmean3 >= LAD.Open, 1, 0), 
            LAD.Open = ifelse(LAD.Open >= dmean3, 1, 0), 
            LAD.High = ifelse(LAD.High >= dmean3, 1, 0), 
            LAD.Low = ifelse(LAD.Low >= dmean3, 1, 0), 
            LAD.Close = ifelse(LAD.Close >= dmean3, 1, 0))
          
          wt <- X['wt'] %>% tbl_df
          Y <- X['dmean3'] %>% tbl_df
          X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                     'LAD.Volume', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ daily.mean3 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(
              x = build.x(dmean3 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = tbl_df(Y['dmean3']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(dmean3 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(~ -1 + ., tbl_df(Y['dmean3'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(dmean3 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(dmean3 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'mixed1') {
          X[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')] <- 
            data.frame(as.matrix(X[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')]) / 
                         first(X$LAD.Open)) %>% tbl_df
          
          X %<>% mutate(
            Price = LAD.Open, 
            mixed1 = (LAD.Open / first(LAD.Open)) * dmean1, 
            mixed1 = ifelse(mixed1 >= LAD.Open, 1, 0), 
            LAD.Open = ifelse(LAD.Open >= dmean1, 1, 0), 
            LAD.High = ifelse(LAD.High >= dmean1, 1, 0), 
            LAD.Low = ifelse(LAD.Low >= dmean1, 1, 0), 
            LAD.Close = ifelse(LAD.Close >= dmean1, 1, 0))
          
          wt <- X['wt'] %>% tbl_df
          Y <- X['mixed1'] %>% tbl_df
          X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                     'LAD.Volume', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ mixed1 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(
              x = build.x(mixed1 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = tbl_df(Y['mixed1']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(mixed1 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(~ -1 + ., tbl_df(Y['mixed1'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(mixed1 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(mixed1 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'mixed2') {
          X[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')] <- 
            data.frame(as.matrix(X[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')]) / 
                         first(X$LAD.Open)) %>% tbl_df
          
          X %<>% mutate(
            Price = LAD.Open, 
            mixed2 = (LAD.Open / first(LAD.Open)) * dmean2, 
            mixed2 = ifelse(mixed2 >= LAD.Open, 1, 0), 
            LAD.Open = ifelse(LAD.Open >= dmean2, 1, 0), 
            LAD.High = ifelse(LAD.High >= dmean2, 1, 0), 
            LAD.Low = ifelse(LAD.Low >= dmean2, 1, 0), 
            LAD.Close = ifelse(LAD.Close >= dmean2, 1, 0))
          
          wt <- X['wt'] %>% tbl_df
          Y <- X['mixed2'] %>% tbl_df
          X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                     'LAD.Volume', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ mixed2 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(
              x = build.x(mixed2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = tbl_df(Y['mixed2']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(mixed2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(~ -1 + ., tbl_df(Y['mixed2'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(mixed2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(mixed2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'mixed3') {
          X[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')] <- 
            data.frame(as.matrix(X[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')]) / 
                         first(X$LAD.Open)) %>% tbl_df
          
          X %<>% mutate(
            Price = LAD.Open, 
            mixed3 = (LAD.Open / first(LAD.Open)) * dmean3, 
            mixed3 = ifelse(mixed3 >= LAD.Open, 1, 0), 
            LAD.Open = ifelse(LAD.Open >= dmean3, 1, 0), 
            LAD.High = ifelse(LAD.High >= dmean3, 1, 0), 
            LAD.Low = ifelse(LAD.Low >= dmean3, 1, 0), 
            LAD.Close = ifelse(LAD.Close >= dmean3, 1, 0))
          
          wt <- X['wt'] %>% tbl_df
          Y <- X['mixed3'] %>% tbl_df
          X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                     'LAD.Volume', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ mixed3 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(
              x = build.x(mixed3 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = tbl_df(Y['mixed3']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(mixed3 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(~ -1 + ., tbl_df(Y['mixed3'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(mixed3 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(mixed3 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        #'@ if(.log == TRUE) {
        #'@   X %<>% mutate_each(funs(log))
        #'@   Y %<>% mutate_each(funs(log))
        #'@ }
        
        #set X_i0 as baseline (intercept).
        #'@ tmp <- list(x = build.x(~ -1 + ., X), y = Y)
        #'@ suppressWarnings(build.x(~ -1 + ., X, contrasts = TRUE))
        
      } else {
        stop('Kindly set xy.matrix = "h1" or xy.matrix = "h2".')
      }
      
      ## --------------------------- return function ---------------------------------
      options(warn = 0)
      return(tmp)
      
    } else {
      if(xy.matrix == 'h1') {
        ## ---------------------------------- h1 ---------------------------------------
        ## h1 is long format converted from default quantmmod xts.
        
        ## http://stats.stackexchange.com/questions/136085/is-it-posible-to-use-factor-categorical-variables-in-glmnet-for-logistic-regre
        ## glmnet cannot take factor directly, you need to transform factor variables to dummies. 
        ##    It is only one simple step using model.matrix, for instance:
        # 
        #'@ x_train <- model.matrix( ~ .-1, train[,features])
        #'@ lm = cv.glmnet(x = x_train, y = as.factor(train$y), intercept = FALSE, 
        #'@                family = "binomial", alpha = 1, nfolds = 7)
        #'@ best_lambda <- lm$lambda[which.min(lm$cvm)]
        # 
        ## alpha=1 will build a LASSO.
        
        if(yv == 'baseline') {
          X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
            mutate(Category = factor(
              Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
            arrange(Date)
          #let `Date` be numeric variable as convert by system.
          
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(b0 = Price / first(Price), 
                            b0 = ifelse(b0 >= b0[1], 1, 0))
          Y %<>% .['b0'] %>% tbl_df
          X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ baseline formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(x = build.x(b0 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), y = Y, wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(b0 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(~ -1 + ., tbl_df(Y['b0'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(b0 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(b0 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'close1') {
          X %<>% gather(Category, Price, LAD.Open:LAD.Low) %>% 
            mutate(Category = factor(
              Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low'))) %>% 
            arrange(Date)
          
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(LAD.Close = ifelse(LAD.Close >= Price, 1, 0))
          Y %<>% .['LAD.Close'] %>% tbl_df
          X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ close1 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(x = build.x(LAD.Close ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = tbl_df(Y['LAD.Close']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(LAD.Close ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(~ -1 + ., tbl_df(Y['LAD.Close'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(LAD.Close ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(LAD.Close ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'close2') {
          X %<>% mutate(LAD.Close2 = LAD.Close) %>% 
            gather(Category, Price, LAD.Open:LAD.Close) %>% 
            mutate(Category = factor(
              Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
            arrange(Date)
          
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(LAD.Close2 = ifelse(LAD.Close2 >= Price, 1, 0))
          Y %<>% .['LAD.Close2'] %>% tbl_df
          X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ close2 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(x = build.x(LAD.Close2 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = tbl_df(Y['LAD.Close2']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(LAD.Close2 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(~ -1 + ., tbl_df(Y['LAD.Close2'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(LAD.Close2 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(LAD.Close2 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'daily.mean1') {
          X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
            mutate(Category = factor(
              Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
            arrange(Date)
          #let `Date` be numeric variable as convert by system.
          
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(dmean1 = ifelse(dmean1 >= Price, 1, 0))
          Y %<>% .['dmean1'] %>% tbl_df
          X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ daily.mean1 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(x = build.x(dmean1 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = tbl_df(Y['dmean1']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(dmean1 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(~ -1 + ., tbl_df(Y['dmean1'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(dmean1 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(dmean1 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'daily.mean2') {
          X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
            mutate(Category = factor(
              Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
            arrange(Date)
          #let `Date` be numeric variable as convert by system.
          
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(dmean2 = ifelse(dmean2 >= Price, 1, 0))
          Y %<>% .['dmean2'] %>% tbl_df
          X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ daily.mean2 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(x = build.x(dmean2 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = tbl_df(Y['dmean2']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(dmean2 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(~ -1 + ., tbl_df(Y['dmean2'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(dmean2 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(dmean2 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'daily.mean3') {
          X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
            mutate(Category = factor(
              Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
            arrange(Date)
          #let `Date` be numeric variable as convert by system.
          
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(dmean3 = ifelse(dmean3 >= Price, 1, 0))
          Y %<>% .['dmean3'] %>% tbl_df
          X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ daily.mean3 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(x = build.x(dmean3 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = tbl_df(Y['dmean3']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(dmean3 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(~ -1 + ., tbl_df(Y['dmean3'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(dmean3 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(dmean3 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'mixed1') {
          X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
            mutate(Category = factor(
              Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
            arrange(Date)
          #let `Date` be numeric variable as convert by system.
          
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(mixed1 = (Price / first(Price)) * dmean1, 
                            mixed1 = ifelse(mixed1 >= Price, 1, 0))
          Y %<>% .['mixed1'] %>% tbl_df
          X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ mixed1 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(x = build.x(mixed1 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = tbl_df(Y['mixed1']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(mixed1 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(~ -1 + ., tbl_df(Y['mixed1'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(mixed1 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(mixed1 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'mixed2') {
          X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
            mutate(Category = factor(
              Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
            arrange(Date)
          #let `Date` be numeric variable as convert by system.
          
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(mixed2 = (Price / first(Price)) * dmean2, 
                            mixed2 = ifelse(mixed2 >= Price, 1, 0))
          Y %<>% .['mixed2'] %>% tbl_df
          X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ mixed2 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(x = build.x(mixed2 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = tbl_df(Y['mixed2']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(mixed2 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(~ -1 + ., tbl_df(Y['mixed2'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(mixed2 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(mixed2 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'mixed3') {
          X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
            mutate(Category = factor(
              Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
            arrange(Date)
          #let `Date` be numeric variable as convert by system.
          
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(mixed3 = (Price / first(Price)) * dmean3, 
                            mixed3 = ifelse(mixed3 >= Price, 1, 0))
          Y %<>% .['mixed3'] %>% tbl_df
          X %<>% .[c('LAD.Volume', 'Category', 'Price')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ mixed3 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(x = build.x(mixed3 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = tbl_df(Y['mixed3']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(mixed3 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(~ -1 + ., tbl_df(Y['mixed3'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(x = build.x(mixed3 ~ LAD.Volume + Category + Price - 1, X, 
                                    contrasts = TRUE, sparse = TRUE), 
                        y = build.y(mixed3 ~ LAD.Volume + Category + Price - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(.log == TRUE) {
          X %<>% mutate_each(funs(log))
          Y %<>% mutate_each(funs(log))
        }
        
        ## sample for contrasts :
        #'@ contrasts(LADDT_DF$Category) <- contr.treatment(LADDT_DF$Category)
        #'@ attr(LADDT_DF$Category, 'levels') <- c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')
        #'@ attr(LADDT_DF$Category,'contrasts') <- contrasts(C(factor(LADDT_DF$Category), 
        #'@                                                    'contr.treatment'))
        #'@ tmp <- model.matrix(Category ~ Date + Price + wt + b0, data = LADDT_DF) %>% 
        #'@   tbl_df %>% mutate(Category = LADDT_DF$Category)
        
        # book title : "R for Everyone: Advanced Analytics and Graphics"
        # useful::build.x() will convert the matrix into a dummy variable matrix.
        # https://books.google.co.jp/books?id=EkpvAgAAQBAJ&pg=PA285&dq=cv.glmnet++binomial&hl=en&sa=X&redir_esc=y#v=onepage&q=cv.glmnet%20%20binomial&f=false
        # page273:
        # matrix.model() and sparse.matrix.model() both create a factor class into an 
        #   ordered levels but will but only 0 and 1 if more than 2 levels. However, 
        #   it is generally considered undesirable for the predictor matrix to be 
        #   designed this way for the Elastic Net. It is possible to have model.matrix() 
        #   return indicator variables for all levels of a factor, although doing so can 
        #   take some creative coding. To make the rocess easier we incorporated a solution 
        #   in the build.x() in the 'useful' package.
        # 
        # http://stackoverflow.com/questions/4560459/all-levels-of-a-factor-in-a-model-matrix-in-r
        #'@ testFrame <- data.frame(First = sample(1:10, 20, replace = TRUE),
        #'@                         Second = sample(1:20, 20, replace = TRUE), 
        #'@                         Third = sample(1:10, 20, replace = TRUE),
        #'@                         Fourth = rep(c("Alice", "Bob", "Charlie", "David"), 5),
        #'@                         Fifth = rep(c("Edward", "Frank", "Georgia", "Hank", "Isaac"), 4))
        # 
        # You need to reset the contrasts for the factor variables:
        #'@ model.matrix(~ Fourth + Fifth, data = testFrame, 
        #'@              contrasts.arg = list(Fourth = contrasts(testFrame$Fourth, contrasts = FALSE), 
        #'@                                   Fifth = contrasts(testFrame$Fifth, contrasts = FALSE)))
        # 
        #   or, with a little less typing and without the proper names:
        # 
        #'@ model.matrix(~ Fourth + Fifth, data = testFrame, 
        #'@              contrasts.arg = list(Fourth = diag(nlevels(testFrame$Fourth)), 
        #'@                                   Fifth = diag(nlevels(testFrame$Fifth))))
        # 
        #'@ model.matrix(~ ., data = testFrame, 
        #'@              contrasts.arg = lapply(testFrame[, 4:5], contrasts, contrasts = FALSE))
        # 
        # page274
        #'@ require('useful')
        # always use all levels
        #'@ build.x(First ~ Second + Fourth + Fifth, textFrame, contrasts = FALSE)
        # 
        # just use all levels for Fourth
        #'@ build.x(First ~ Second + Fourth + Fifth, testFrame, 
        #'@         contrasts = c(Fourth = FALSE, Fifth = TRUE))
        # 
        # Using build.x appropriately on `acs` dataset builds a nice predictor matrix for use in 
        #   glmnet. We control the desired matrix by using a formula for our model 
        #   specification just like we would in lm, interactions and all.
        # 
        # make a binary Income variable for building a logistic regression
        #'@ acs$Income <- with(acs, FamilyIncome >= 150000)
        # 
        # page275
        # build predictor matrix
        # do not include the intercept as glmnet will add that automatically
        #'@ acsX <- build.x(Income ~ NumBedrooms + NumChildren + NumPeople + 
        #'@                 NumRooms + NumUnits + NumVehicles + NumWorkers + 
        #'@                 OwnRent + YearBuilt + ElectricBill + FoodStamp + 
        #'@                 HeatingFuel + Insurance + Language - 1, 
        #'@                 data = acs, contrasts = FALSE)
        # 
        # check class and dimensions
        #'@ class(acsX)
        #[1] "matrix"
        #'@ dim(acsX)
        #[1] 22745 44
        # 
        # page275
        # view the top left and top right of the data
        #'@ topleft(acsX, c = 6)
        #'@ topright(acsX, c = 6)
        #
        # build response predictor
        #'@ acsY <- build.y(Income ~ NumBedrooms + NumChildren + NumPeople + 
        #'@                 NumRooms + NumUnits + NumVehicles + NumWorkers + 
        #'@                 OwnRent + YearBuilt + ElectricBill + FoodStamp + 
        #'@                 HeatingFuel + Insurance + Language - 1, data = acs)
        #
        #'@ head(acsY)
        #'@ tail(acsY)
        # 
        
        #set X_i0 as baseline (intercept).
        #'@ tmp <- list(x = build.x(~ -1 + ., X), y = Y)
        #'@ suppressWarnings(build.x(~ -1 + ., ddt_DF, contrasts = TRUE))
        
      } else if(xy.matrix == 'h2') {
        ## ---------------------------------- h2 ---------------------------------------
        ## h2 is wide format or default quantmmod xts.
        
        if(yv == 'baseline') {
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(b0 = ifelse(LAD.Open >= first(LAD.Open), 1, 0))
          Y %<>% .['b0'] %>% tbl_df
          X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                     'LAD.Volume')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ baseline formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(
              x = build.x(b0 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + 
                            LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = tbl_df(Y['b0']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(b0 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + 
                            LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(~ -1 + ., tbl_df(Y['b0'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(b0 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + 
                            LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(b0 ~ LAD.Open + LAD.High + LAD.Low + LAD.Close + 
                            LAD.Volume - 1, Y, wt = wt))
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'close1') {
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(LAD.Close = ifelse(LAD.Close >= LAD.Open, 1, 0))
          Y %<>% .['LAD.Close'] %>% tbl_df
          X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Volume')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ close1 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(
              x = build.x(LAD.Close ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Volume - 1, X, 
                          contrasts = TRUE, sparse = TRUE), 
              y = tbl_df(Y['LAD.Close']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(LAD.Close ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Volume - 1, X, 
                          contrasts = TRUE, sparse = TRUE), 
              y = build.y(~ -1 + ., tbl_df(Y['LAD.Close'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(LAD.Close ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Volume - 1, X, 
                          contrasts = TRUE, sparse = TRUE), 
              y = build.y(LAD.Close ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Volume - 1, Y, wt = wt))
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'close2') {
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(LAD.Close2 = ifelse(LAD.Close >= LAD.Open, 1, 0))
          Y %<>% .['LAD.Close2'] %>% tbl_df
          X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                     'LAD.Volume')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ close2 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(
              x = build.x(LAD.Close2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = tbl_df(Y['LAD.Close2']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(LAD.Close2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(~ -1 + ., tbl_df(Y['LAD.Close2'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(LAD.Close2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(LAD.Close2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'daily.mean1') {
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(dmean1 = ifelse(dmean1 >= LAD.Open, 1, 0))
          Y %<>% .['dmean1'] %>% tbl_df
          X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                     'LAD.Volume')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ daily.mean1 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(
              x = build.x(dmean1 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = tbl_df(Y['dmean1']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(dmean1 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(~ -1 + ., tbl_df(Y['dmean1'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(dmean1 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(dmean1 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'daily.mean2') {
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(dmean2 = ifelse(dmean2 >= LAD.Open, 1, 0))
          Y %<>% .['dmean2'] %>% tbl_df
          X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                     'LAD.Volume')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ daily.mean2 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(
              x = build.x(dmean2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = tbl_df(Y['dmean2']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(dmean2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(~ -1 + ., tbl_df(Y['dmean2'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(dmean2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(dmean2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'daily.mean3') {
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(dmean3 = ifelse(dmean3 >= LAD.Open, 1, 0))
          Y %<>% .['dmean3'] %>% tbl_df
          X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                     'LAD.Volume')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ daily.mean3 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(
              x = build.x(dmean3 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = tbl_df(Y['dmean3']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(dmean3 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(~ -1 + ., tbl_df(Y['dmean3'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(dmean3 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(dmean3 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'mixed1') {
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(mixed1 = (LAD.Close / first(LAD.Open)) * dmean1, 
                            mixed1 = ifelse(mixed1 >= LAD.Open, 1, 0))
          Y %<>% .['mixed1'] %>% tbl_df
          X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                     'LAD.Volume')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ mixed1 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(
              x = build.x(mixed1 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = tbl_df(Y['mixed1']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(mixed1 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(~ -1 + ., tbl_df(Y['mixed1'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(mixed1 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(mixed1 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'mixed2') {
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(mixed2 = (LAD.Close / first(LAD.Open)) * dmean2, 
                            mixed2 = ifelse(mixed2 >= LAD.Open, 1, 0))
          Y %<>% .['mixed2'] %>% tbl_df
          X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                     'LAD.Volume')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ mixed2 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(
              x = build.x(mixed2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = tbl_df(Y['mixed2']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(mixed2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(~ -1 + ., tbl_df(Y['mixed2'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(mixed2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(mixed2 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        if(yv == 'mixed3') {
          wt <- X['wt'] %>% tbl_df
          Y <- X %>% mutate(mixed3 = (LAD.Close / first(LAD.Open)) * dmean3, 
                            mixed3 = ifelse(mixed3 >= LAD.Open, 1, 0))
          Y %<>% .['mixed3'] %>% tbl_df
          X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 
                     'LAD.Volume')] %>% tbl_df
          
          if(.log == TRUE) {
            X %<>% mutate_each(funs(log))
            Y %<>% mutate_each(funs(log))
          }
          
          ## ------------ mixed3 formula ---------------------------
          if(setform == 'l1'){
            tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y, wt = wt)
            
          } else if(setform == 'l2'){
            X <- cbind(X, Y)
            tmp <- list(
              x = build.x(mixed3 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = tbl_df(Y['mixed3']), wt = wt)
            
          } else if(setform == 'l3'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(mixed3 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(~ -1 + ., tbl_df(Y['mixed3'])), wt = wt)
            
          } else if(setform == 'l4'){
            X <- Y <- cbind(X, Y)
            tmp <- list(
              x = build.x(mixed3 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, 
                          X, contrasts = TRUE, sparse = TRUE), 
              y = build.y(mixed3 ~ LAD.Open + LAD.High + LAD.Low + 
                            LAD.Close + LAD.Volume - 1, Y), wt = wt)
            
          } else {
            stop('setform must be one among c(\'', paste(setforms, collapse = '\', \''), '\').')
          }
        }
        
        #'@ if(.log == TRUE) {
        #'@   X %<>% mutate_each(funs(log))
        #'@   Y %<>% mutate_each(funs(log))
        #'@ }
        
        #set X_i0 as baseline (intercept).
        #'@ tmp <- list(x = build.x(~ -1 + ., X), y = Y)
        #'@ suppressWarnings(build.x(~ -1 + ., ddt_DF, contrasts = TRUE))
        
      } else {
        stop('Kindly set xy.matrix = "h1" or xy.matrix = "h2".')
      }
      
      ## --------------------------- return function ---------------------------------
      options(warn = 0)
      return(tmp)
      
    }
    
  } else {
    stop('The regression model that you choose not yet ready.')
  }
}
