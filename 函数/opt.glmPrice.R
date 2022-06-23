opt.glmPrice <- function(mbase, family = 'gaussian', xy.matrix = 'h1', setform = 'l1', 
                         yv = 'baseline', logistic.yv = TRUE, tmeasure = 'deviance', 
                         tmultinomial = 'grouped', maxit = 100000, 
                         alpha = 0:10, nfolds = 10, foldid = NULL, s = 'lambda.min', 
                         weight.date = FALSE, weight.volume = FALSE, wt.control = FALSE, 
                         newx = NULL, pred.type = 'class', data.size = 365, fordate = NULL, 
                         parallel = TRUE, .log = FALSE) {
  
  ## mbase = default quantmod xts format or in mbase frame format.
  ## 
  ## family = gaussian', 'binomial', 'poisson', 'multinomial', 'cox' and 'mgaussian'.
  ## 
  ## xy.matrix = 'h1' or xy.matrix = 'h2'. setting x and y variables.
  ## 
  ## setform %in% c('l1', 'l2', 'l3', 'l4') will set a formula for build.x() and build.y().
  ## 
  ## alpha from 0, 0.1, 0.2 ... until 1.0. Ridge is alpha = 0; 
  ##   Elastic Net is 0 < alpha < 1; Lasso is alpha = 1
  ## 
  ## yvs <- c('baseline', 'close1', 'close2', 'daily.mean1', 'daily.mean2', 'daily.mean3', 
  ##   'mixed1', 'mixed2', 'mixed3') #to model the y (respondence variables)
  ##   baseline only use first element of opening price as baseline.
  ##   close1 = X = mbase.frame(Op(LAD), Hi(LAD), Lo(LAD)) and Y = Cl(LAD), 
  ##   close2 = X = mbase.frame(Op(LAD), Hi(LAD), Lo(LAD), Cl(LAD)) and Y = Cl(LAD), 
  ##   daily.mean1 = mean(Op(LAD), Cl(LAD)), daily.mean2 = mean(Hi(LAD), Lo(LAD)), 
  ##   daily.mean3 = mean(Op(LAD), Hi(LAD), Lo(LAD), Cl(LAD)).
  ##   mixed1,2,3 are the Y = baseline * daily.mean1,2,3
  ##   For binomial and multinomial, the dmean1,2,3 or mixed1,2,3 values greater than 
  ##   opening price will be set as 1 and lower will be 0.
  ## 
  ## logistic.yv = TRUE will convert the greater value of X into 1,0 mode but 
  ##   yv == baseline the price of levels c(Op, Hi, Lo, Cl) will be set as 1,2,3,4.
  ## 
  ## tmeasure is type.measure for residuals which is cost function.
  ##   tmeasure = 'deviance', 'mse', 'mae', 'auc', 'class' and 'cox'.
  ## 
  ## tmultinomial is type.multinomial for which is "grouped" or "ungrouped".
  ## 
  ## default glmnet is maxit = 100000 but it will be error if there is high volume mbase.
  ##   therefore I set as maxit = 1000.
  ## 
  ## pred.type = "link", pred.type = "response", pred.type = "coefficients", 
  ## pred.type = "nonzero", pred.type = "class"
  ## 
  ## default nfolds = 10 for cv.glmnet().
  ## 
  ## default foldid = NULL. You can choose foldid among nfolds. Once choose nfold will be missing.
  ## 
  ## Never rely on glmnet's default lambda sequence! Notorious issue. 
  ## Always provide your own sequence. Then get the optimal lambda value 
  ##   afterwards from fit$lambda.min and use it with the s=lambda.min parameter 
  ##   in all calls to predict(), coef() etc.
  ## therefore I set default as s = lambda.min. You can choose s = lambda.1se as well.
  ## 
  ## weight.date = FALSE or TRUE for time series weighted function prediction.
  ## 
  ## weight.volume = FALSE for time series weighted function prediction with volume effect.
  ## 
  ## wt.control = TRUE if internal weighted values applied. For both binomial and 
  ##   multinomial will not be applicable and skip it due to logical Y cannot mlultiplied 
  ##   by a vector of weighted parameters.
  ## 
  ## parallel = TRUE or FALSE for parallel computing.
  ## 
  ## .log = FALSE or TRUE convert the value yv into log or not.
  ## 
  ## newx is newdata or newx which is a dataset same nrow with the fitted model.
  ## 
  ## data.size = 365, the observation unit in days.
  ## 
  ## fordate = NULL, you can enter a forecast date for example : '2015-01-02' while latest 
  ##   or current date is earlier than farecast date.
  ## 
  
  ## ========================= Load Packages ===================================
  # http://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection
  # http://www4.stat.ncsu.edu/~reich/Bigmbase/code/glmnet.html
  
  ## 
  ## https://books.google.co.jp/books?id=uIDOBQAAQBAJ&pg=PA278&dq=cv.glmnet++binomial&hl=en&sa=X&redir_esc=y#v=onepage&q=cv.glmnet%20%20binomial&f=false
  ## page278 : 
  ## Although the LASSO estimates often result in better prediction than the 
  ##   ordinary MLEs, subsequent inference procedures have not yet been fully 
  ##   developed. For example, confidence intervals for regression parameters or 
  ##   predicted values do not tey exists. Therefore the LASSO is used primarily 
  ##   as a variable selection tool or for making predictions where interval 
  ##   estimates are not required.
  
  options(warn = -1)
  suppressPackageStartupMessages(library("BBmisc"))
  suppressAll(library('lubridate'))
  suppressAll(library('MASS'))
  suppressAll(library('glmnet'))
  suppressAll(library('Matrix'))
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
  
  suppressAll(library('useful'))
  suppressAll(library('broom'))
  suppressAll(library('parallel'))
  suppressAll(library('doParallel'))
  suppressAll(library('foreach'))
  suppressMessages(source('./function/glmPrice.R'))
  
  fit01 <- glmPrice(mbase = mbase, family = family, xy.matrix = xy.matrix, 
                    setform = setform, yv = yv, logistic.yv = logistic.yv, 
                    tmeasure = tmeasure, tmultinomial = tmultinomial, maxit = maxit, 
                    alpha = alpha, nfolds = nfolds, foldid = foldid, s = s, 
                    weight.date = weight.date, weight.volume = weight.volume, 
                    wt.control = wt.control, newx = newx, pred.type = pred.type, 
                    parallel = parallel, .log = .log)
  
  ## For my understanding, lambda.min or lambda.1se will be the X_i in below article, 
  ##   and we need to add a weight function into it as Y = X_i * W_i.
  ##   [Stock Market Forecasting Using LASSO Linear Regression Model](https://github.com/englianhu/binary.com-interview-question/blob/master/reference/Stock%20Market%20Forecasting%20Using%20LASSO%20Linear%20Regression%20Model.pdf)
  tmp <- fit
  
  #'@ > predict(fitgaum16.alpha08, newx = xy$x, 
  #'@           +         s = fitgaum16.alpha08$lambda.1se, 
  #'@           +         pred.type = 'class') %>% data.frame(Pred = .) %>% tbl_df %>% 
  #'@   +     mutate(lambda1se = 0.2454598, Diff = c(0, diff(X1))) %>% .$Diff %>% mean
  #'@ [1] 0.08847968
  
  Y = mbase[grep('.Close', names(mbase), value = TRUE)] %>% unlist
  data.frame(Date = mbase$Date, Y, fit01$yhat) %>% tbl_df
  
  paste0('data.frame(y, ', paste(paste0('yhat', alpha), collapse = ', '), ') %>% tbl_df')
  
  ## weighted function
  if(!is.null(fordate)) { 
    if(weight.date != FALSE) {
      if(as.Date(fordate) >= max(mbase$Date)) { #if(as.POSIXct(fordate) >= max(mbase$Date))           
        weightmbase2 <- exp(-abs(xi) * as.numeric(difftime(fordate, mbase$Date, units = 'days'))^2)
        #exp(as.numeric(difftime(c(first(dfm$Date) - 1, lag(dfm$Date, 1)[-1]), last(dfm$Date), units = 'days'))^2)
        weightmbase1 <- rep(weightmbase2,2) 
      } else { stop('forecast date is less than sample dates')}
    } else {           
      weightmbase2 <- rep(1,length(x)); weightmbase1 <- rep(weightmbase2,2)}
  } else {
    if(weight.date != FALSE) {
      fordate <- max(mbase$Date)
      weightmbase2 <- exp(-abs(xi) * as.numeric(difftime(fordate, mbase$Date, units = 'days'))^2)
      weightmbase1 <- rep(weightmbase2,2) 
    } else {
      fordate <- max(mbase$Date)
      weightmbase2 <- rep(1,length(x)); weightmbase1 <- rep(weightmbase2,2)}}
  
  options(warn = 0)
  
  return(tmp)
  }

