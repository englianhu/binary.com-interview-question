lmStocks <- function(mbase, family = 'gaussian', xy.matrix = 'h1', setform = 'l1', 
                     yv = 'baseline', logistic.yv = TRUE, tmeasure = 'deviance', 
                     tmultinomial = 'grouped', maxit = 1000, 
                     alpha = 0:10, nfolds = 10, foldid = NULL, s = 'lambda.min', 
                     weight.date = FALSE, weight.volume = FALSE, wt.control = FALSE, 
                     newx = NULL, pred.type = 'class', parallel = TRUE, .log = FALSE) {
  ## mbase = default quantmod xts format or in data frame format.
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
  ##   close1 = X = data.frame(Op(LAD), Hi(LAD), Lo(LAD)) and Y = Cl(LAD), 
  ##   close2 = X = data.frame(Op(LAD), Hi(LAD), Lo(LAD), Cl(LAD)) and Y = Cl(LAD), 
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
  ## default glmnet is maxit = 100000 but it will be error if there is high volume data.
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
  
  ## ========================= Load Packages ===================================
  # http://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection
  # http://www4.stat.ncsu.edu/~reich/BigData/code/glmnet.html
  
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
  suppressMessages(source('./function/h.R'))
  
  #'@ doParallel::registerDoParallel(cores = parallel::detectCores())
  
  ## https://github.com/tobigithub/R-parallel/wiki/R-parallel-Errors
  ### Register parallel backend
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  getDoParWorkers()
  
  ## ======================= Data Validation ===================================
  if(is.xts(mbase)) {
    mbase <- as.matrix(mbase) %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
      tbl_df %>% mutate(Date = ymd(Date)) %>% arrange(Date)
    
  } else if(is.data.frame(mbase)) {
    mbase <- mbase
  } else {
    stop('Kindly apply filterLAD and fit the dataset into the function.')
  }
  
  dateID <- sort(unique(mbase$Date))
  dateRange <- range(dateID)
  
  families <- c('gaussian', 'binomial', 'poisson', 'multinomial', 'cox', 'mgaussian')
  if(family %in% families) {
    family <- family
  } else {
    stop('family must be one among c(\'', paste(families, collapse = '\', \''), '\').')
  }
  
  yvs <- c('baseline', 'daily.mean1', 'daily.mean2', 'daily.mean3', 'mixed1', 
           'mixed2', 'mixed3')
  if(yv %in% yvs) {
    yv <- yv
  } else {
    stop('yv must be one among c(\'', paste(yvs, collapse = '\', \''), '\').')
  }
  
  xy.matries <- c('h1', 'h2')
  if(xy.matrix %in% xy.matries) {
    xy.matrix <- xy.matrix
  } else {
    stop('xy.matrix must be one among c(\'', paste(xy.matries, collapse = '\', \''), '\').')
  }
  
  if(is.numeric(alpha)) {
    alpha <- alpha
  } else if(is.character(alpha)) {
    if(anyNA(suppressAll(as.numeric(alpha)))) {
      stop('alpha must be minimum 1 to maximum 10 numbers which is within 0:10.')
    } else {
      alpha <- as.numeric(alpha)
    }    
  } else {
    stop('alpha must be minimum 1 to maximum 10 numbers which is within 0:10.')
  }
  
  ## default glmnet is maxit = 100000 but it will be error if there is high volume data.
  ##   therefore I set as maxit = 1000.
  if((is.numeric(maxit)) & (length(maxit) == 1)) {
    maxit <- maxit
  } else if(is.character(maxit)) {
    if(anyNA(suppressAll(as.numeric(maxit)))) {
      stop('maxit must be a numeric number from 1 to Inf.')
    } else {
      maxit <- as.numeric(maxit)
    }    
  } else {
    stop('maxit must be a numeric number from 1 to Inf.')
  }
  
  ## default is nfolds = 10.
  if(!is.null(nfolds)) {
    if((is.numeric(nfolds)) & (length(nfolds) == 1) & (nfolds > 0 & nfolds <= 10)) {
      nfolds <- nfolds
      foldid <- NULL
    } else if(is.character(nfolds)) {
      if(anyNA(suppressAll(as.numeric(nfolds)))) {
        stop('nfolds must be a numeric from 1 to 10.')
      } else {
        nfolds <- as.numeric(nfolds)
        foldid <- NULL
      }
    } else {
      stop('nfolds must be a numeric from 1 to 10.')
    }
  } else  {
    nfolds <- NULL
    foldid <- foldid
  }
  
  ## once set foldid the nfold will be missing.
  if(!is.null(foldid)) {
    if(is.numeric(foldid) * (foldid %in% 1:10)) {
      foldid <- foldid
      nfolds <- NULL
    } else if(is.character(foldid)) {
      if(anyNA(suppressAll(as.numeric(foldid)))) {
        stop('foldid must be a numeric vector from 1 to 10.')
      } else {
        foldid <- as.numeric(foldid)
        nfolds <- NULL
      }
    } else {
      stop('foldid must be a numeric vector from 1 to 10.')
    }
  } else {
    foldid <- NULL
    nfolds <- nfolds
  }
    
  # default is pred.type = 'class'
  if(pred.type == 'link') {
    pred.type <- pred.type
  } else if(pred.type == 'response') {
    pred.type <- pred.type
  } else if(pred.type == 'coefficients') {
    pred.type <- pred.type
  } else if(pred.type == 'nonzero') {
    pred.type <- pred.type
  } else if(pred.type == 'class') {
    pred.type <- pred.type
  } else {
    stop('Kindly select pred.type = "link", pred.type = "response", pred.type = "coefficients", pred.type = "nonzero", pred.type = "class".')
  }
  
  ## ========================= Respondence Variable =================================
  ## weight parameters.
  wt <- data_frame(wt = rep(1, nrow(mbase)))
  if(weight.date == FALSE) {
    wt %<>% mutate(wetd = rep(1, nrow(mbase)))
  }
  
  if(weight.volume == FALSE) {
    wt %<>% mutate(wetv = rep(1, nrow(mbase)))
  }
  
  ## applicable to internal or external or both weight values.
  wt %<>% mutate(wt = wetd * wetv)
  wt %<>% .['wt'] %>% tbl_df
  
  ## http://stackoverflow.com/questions/39863367/ridge-regression-with-glmnet-gives-different-coefficients-than-what-i-compute?answertab=votes#tab-top
  #library(MASS)
  #library(glmnet)
  #
  ## Data dimensions
  #p.tmp <- 100
  #n.tmp <- 100
  #
  ## Data objects
  #set.seed(1)
  #X <- scale(mvrnorm(n.tmp, mu = rep(0, p.tmp), Sigma = diag(p.tmp)))
  #beta <- rep(0, p.tmp)
  #beta[sample(1:p.tmp, 10, replace = FALSE)] <- 10
  #Y.true <- X %*% beta
  #Y <- scale(Y.true + matrix(rnorm(n.tmp)))
  #
  ## Run glmnet 
  #ridge.fit.cv <- cv.glmnet(X, Y, alpha = 0, intercept = FALSE)
  #ridge.fit.lambda <- ridge.fit.cv$lambda.1se
  #
  ## Extract coefficient values for lambda.1se (without intercept)
  #ridge.coef <- (coef(ridge.fit.cv, s = ridge.fit.lambda))[-1]
  #
  ## Get coefficients "by definition"
  #ridge.coef.DEF <- drop(solve(crossprod(X) + diag(n.tmp * ridge.fit.lambda, p.tmp), 
  #   crossprod(X, Y)))
  #
  ## Plot estimates
  #plot(ridge.coef, type = "l", ylim = range(c(ridge.coef, ridge.coef.DEF)),
  #     main = "black: Ridge `glmnet`\nred: Ridge by definition")
  #lines(ridge.coef.DEF, col = "red")
  #x <- sparse.model.matrix(~., mbase[, 2:5])
  #wt <- rep(0, nrow(x))
  #y.true <- x %*% wt
  #x <- h()
  #y <- LADDT['LAD.Volume']
  #y <- ddply(x, .(Category), summarise, 
  #           resp = sum(((Price * wt * b0) - Price)^2) / nrow(x)) %>% tbl_df
  #xy <- join(x, y) %>% tbl_df
  #xy <- h(mbase)
  eval(parse(
    text = paste(paste0(c('x', 'y', 'wt'), 
                        ' <- h(mbase, family = family, yv = yv, logistic.yv = logistic.yv, wt = wt, wt.control = wt.control, xy.matrix = xy.matrix, setform = setform, .log = .log)[[', 
                        c('\'x\'', '\'y\'', '\'wt\''),']]'), collapse = '; ')))
  
  ## convert the single column y and wt into a numeric vector.
  if(!is.numeric(y)) y <- unlist(y)
  if(!is.numeric(wt)) wt <- unlist(wt)
  
  ## x is a matrix while y is a vector.
  #'@ if(nrow(x) != nrow(y)) stop('number of observation of x must be same with y.')
  
  ## weight parameters.
  #'@ if(weight.date == FALSE) {
  #'@   wt <- data_frame(wt = rep(1, nrow(x))) %>% mutate(wetd = rep(1, nrow(x)))
  #'@ }
  
  #'@ if(weight.volume == FALSE) {
  #'@   wt <- data_frame(wt = rep(1, nrow(x))) %>% mutate(wetv = rep(1, nrow(x)))
  #'@ }
  
  ## applicable to internal or external or both weight values.
  #'@ wt %<>% mutate(wt = wetd * wetv)
  #'@ wt %<>% .['wt'] %>% tbl_df
  
  ## ======================= Cost Function ==================================
  ## "lambda.1se": the largest λλ at which the MSE is within one standard error 
  ##    of the minimal MSE.
  ## 
  ## "lambda.min": the λλ at which the minimal MSE is achieved.
  if(s == 'lambda.1se') {
    s <- s
  } else if(s == 'lambda.min') {
    s <- s
  } else {
    stop('Kindly select s = "lambda.1se" or s = "lambda.min".')
  }
  
  ## http://stats.stackexchange.com/questions/48811/cost-function-for-validating-poisson-regression-models?answertab=votes#tab-top
  ## Assuming nothing special in your particular case, I think there is a good 
  ##   argument for either using the default (Mean Square Error) or use the 
  ##   mean of the error of the logs, or even the chi-squared error.
  ## 
  ## The purpose of the cost function is to express how "upset" you are with 
  ##   wrong predictions, specifically what "wrongness" bothers you most. This 
  ##   is particularly important for binary responses, but can matter in any 
  ##   situation.
  
  ## http://stackoverflow.com/questions/36121171/extract-error-rate-with-cost-function-from-cv-glmnet
  ## 
  if(tmeasure == 'deviance') {    #which uses squared-error for gaussian models
    tmeasure <- tmeasure          #deviance for logistic and poisson regression, and partial-likelihood for the Cox model.
  } else if(tmeasure == 'mse') {  #can be used by all models except the "cox"
    tmeasure <- tmeasure
  } else if(tmeasure == 'mae') {  #can be used by all models except the "cox"
    tmeasure <- tmeasure
  } else if(tmeasure == 'class') {#applies to binomial and multinomial logistic regression only
    tmeasure <- tmeasure
  } else if(tmeasure == 'auc') {  #for two-class logistic regression only, and gives area under the ROC curve.
    tmeasure <- tmeasure
  } else if(tmeasure == 'cox') {
    tmeasure <- tmeasure
  } else {
    stop('Kindly select tmeasure = "deviance", tmeasure = "mse", tmeasure = "mae", tmeasure = "class", tmeasure = "cox".')
  }
  
  if(tmultinomial == 'grouped') {
    type.multinomial <- tmultinomial
  } else if(tmultinomial == 'ungrouped') {
    type.multinomial <- tmultinomial
  } else {
    stop('Kindly set tmultinomial = "grouped" or tmultinomial = "ungrouped" for family = "multinomial".')
  }
  
  ## ==================== Regression Models ===============================
  ## In order to avoid llply() occupy a lot of RAM, here I seperate the model independently.
  ##
  ## glmnet() is a R package which can be used to fit Regression models,lasso model 
  ##   and others. Alpha argument determines what type of model is fit. When alpha=0, 
  ##   Ridge Model is fit and if alpha=1, a lasso model is fit.
  ## cv.glmnet() performs cross-validation, by default 10-fold which can be adjusted 
  ##   using nfolds. A 10-fold CV will randomly divide your observations into 10 
  ##   non-overlapping groups/folds of approx equal size. The first fold will be used 
  ##   for validation set and the model is fit on 9 folds. Bias Variance advantages is 
  ##   usually the motivation behind using such model validation methods. In the case 
  ##   of lasso and ridge models, CV helps choose the value of the tuning parameter 
  ##   lambda.
  ## In your example, you can do plot(reg) OR reg$lambda.min to see the value of lambda 
  ##   which results in the smallest CV error. You can then derive the Test MSE for that 
  ##   value of lambda. By default, glmnet() will perform Ridge or Lasso regression for 
  ##   an automatically selected range of lambda which may not give the lowest test MSE. 
  ## Hope this helps!
  ##
  
  ## http://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection?answertab=votes#tab-top
  ## Please note that glmnet is the preferred package now, it is actively maintained, 
  ##   more so than lars, and that there have been questions about glmnet vs lars answered 
  ##   before (algorithms used differ).
  ##
  
  if(family == 'gaussian') {
    ## -------------------------- gaussian --------------------------------
    ## standardize is a logical flag for x variable standardization, prior to fitting the model sequence. 
    ##   The coefficients are always returned on the original scale. Default is standardize=TRUE.
    ## 
    ## http://stackoverflow.com/questions/17887747/how-does-glmnets-standardize-argument-handle-dummy-variables?answertab=votes#tab-top
    ## If you take a look at the R function, glmnet codes the standardize parameter internally as `isd = as.integer(standardize)`.
    ## 
    if(!is.null(nfolds)) {
      for (i in alpha) {
        ## 3 models : tmeasure = 'deviance' or tmeasure = 'mse' or tmeasure = 'mae'.
        assign(paste('fit', i, sep = ''),
               cv.glmnet(x, y, type.measure = tmeasure, parallel = parallel, 
                         alpha = i/10, family = 'gaussian', 
                         weights = wt, maxit = maxit, nfolds = nfolds))
      }; rm(i)
    }
    
    if(!is.null(foldid)) {
      for (i in alpha) {
        ## 3 models : tmeasure = 'deviance' or tmeasure = 'mse' or tmeasure = 'mae'.
        assign(paste('fit', i, sep = ''),
               cv.glmnet(x, y, type.measure = tmeasure, parallel = parallel, 
                         alpha = i/10, family = 'gaussian', 
                         weights = wt, maxit = maxit, foldid = foldid))
      }; rm(i)
    }
    
  } else if(family == 'binomial') {
    ## -------------------------- binomial --------------------------------
    ## https://rstudio-pubs-static.s3.amazonaws.com/71750_a733595676d3437f940244bc678b4f1f.html
    ## http://ricardoscr.github.io/how-to-use-ridge-and-lasso-in-r.html
    ## https://rpubs.com/chihst01/15922
    ## http://54.225.166.221/arifulmondal/creditscoring
    
    ## http://faculty.chicagobooth.edu/max.farrell/bus41100/week8-Rcode.R
    ## sample data : germancredit.csv
    ## 
    ## slide 21
    ##LASSO selection
    # install.packages("glmnet") #just run this once (on each computer)
    #'@ library(glmnet)
    #
    #'@ X <- model.matrix(formula(full), credit)
    # need to subtract the intercept
    #'@ X <- X[,-1]
    #
    #'@ set.seed(1) #so cross-validation results are the same every time.
    #'@ cvfit <- cv.glmnet(x = X[train,], y = credit$GoodCredit[train], family="binomial", alpha=1, standardize=TRUE)
    # sometimes adding type.measure="auc" is useful for logistic regression, sometimes not
    #'@ betas <- coef(cvfit, s = "lambda.1se")
    #'@ model.1se <- which(betas[2:length(betas)]!=0)
    
    if(!is.null(nfolds)) {
      for (i in alpha) {
        ## 5 models : tmeasure = 'deviance' or tmeasure = 'mse' or 
        ##            tmeasure = 'mae' or tmeasure = 'class' or 
        ##            tmeasure = 'auc'.
        assign(paste('fit', i, sep = ''),
               cv.glmnet(x, y, type.measure = tmeasure, parallel = parallel, 
                         alpha = i/10, family = 'binomial', 
                         weights = wt, maxit = maxit, nfolds = nfolds))
      }; rm(i)
    }
    
    if(!is.null(foldid)) {
      for (i in alpha) {
        ## 5 models : tmeasure = 'deviance' or tmeasure = 'mse' or 
        ##            tmeasure = 'mae' or tmeasure = 'class' or 
        ##            tmeasure = 'auc'.
        assign(paste('fit', i, sep = ''),
               cv.glmnet(x, y, type.measure = tmeasure, parallel = parallel, 
                         alpha = i/10, family = 'binomial', 
                         weights = wt, maxit = maxit, foldid = foldid))
      }; rm(i)
    }
    
  } else if(family == 'poisson') {
    ## -------------------------- poisson ---------------------------------
    
    if(!is.null(nfolds)) {
      for (i in alpha) {
        ## 3 models : tmeasure = 'deviance' or tmeasure = 'mse' or tmeasure = 'mae'.
        assign(paste('fit', i, sep = ''),
               cv.glmnet(x, y, type.measure = tmeasure, parallel = parallel, 
                         alpha = i/10, family = 'poisson', 
                         weights = wt, maxit = maxit, nfolds = nfolds))
      }; rm(i)
    }
    
    if(!is.null(foldid)) {
      for (i in alpha) {
        ## 3 models : tmeasure = 'deviance' or tmeasure = 'mse' or tmeasure = 'mae'.
        assign(paste('fit', i, sep = ''),
               cv.glmnet(x, y, type.measure = tmeasure, parallel = parallel, 
                         alpha = i/10, family = 'poisson', 
                         weights = wt, maxit = maxit, foldid = foldid))
      }; rm(i)
    }
    
  } else if(family == 'multinomial') {
    ## ----------------------- multinomial --------------------------------
    ## http://faculty.chicagobooth.edu/max.farrell/bus41100/week8-Rcode.R
    ## sample data : fueleconomy.csv
    ##
    ## not in slides
    ## Lasso with multinomial logistic regression
    ##take a random sample of the data to select variables. Doing cv.glmnet on the full data set can be very slow.
    ##eventually, try it without this step (and remove all the [indexes.to.keep] and [indexes.to.keep,] from the glmnet command
    #'@ indexes.to.keep <- sample(1:length(mpg.data$hwy.factor), 2000)
    #'@ X <- model.matrix(~(trans + cyl + displ)^2, mpg.data)
    #'@ X <- X[,-1] #need to subtract the intercept
    #'@ cv.fit <- cv.glmnet(X[indexes.to.keep,], mpg.data$hwy.factor[indexes.to.keep], family=c("multinomial"), alpha=1, intercept=TRUE, standardize=FALSE, type.measure="class", type.multinomial="grouped")
    #'@ lambda.index <- which(cv.fit$lambda==cv.fit$lambda.min) #or use lambda.min at the end
    #'@ selected.variables <- unlist(predict(cv.fit, type="nonzero")[lambda.index])
    #
    ## if you want to put more variables back in:
    #'@ refitting.variables <- sort(union(which(colnames(X)=="cyl"), selected.variables))
    # or just set refitting.variables <- selected.variables
    #
    ##refit on the full data
    #'@ mlogit.post.lasso <- multinom(hwy.factor ~ X[,refitting.variables], data=mpg.data)
    ## 
    
    if(!is.null(nfolds)) {
      for (i in alpha) {
        ## 4 models : tmeasure = 'deviance' or tmeasure = 'mse' or 
        ##            tmeasure = 'mae' or tmeasure = 'class'.
        ## 2 models : tmultinomial = 'grouped' or tmultinomial = 'ungrouped'.
        assign(paste('fit', i, sep = ''),
               cv.glmnet(x, y, type.measure = tmeasure, parallel = parallel, 
                         alpha = i/10, family = 'multinomial', 
                         type.multinomial = tmultinomial, 
                         weights = wt, maxit = maxit, nfolds = nfolds))
      }; rm(i)
    }
    
    if(!is.null(foldid)) {
      for (i in alpha) {
        ## 4 models : tmeasure = 'deviance' or tmeasure = 'mse' or 
        ##            tmeasure = 'mae' or tmeasure = 'class'.
        ## 2 models : tmultinomial = 'grouped' or tmultinomial = 'ungrouped'.
        assign(paste('fit', i, sep = ''),
               cv.glmnet(x, y, type.measure = tmeasure, parallel = parallel, 
                         alpha = i/10, family = 'multinomial', 
                         type.multinomial = tmultinomial, 
                         weights = wt, maxit = maxit, foldid = foldid))
      }; rm(i)
    }
    
  } else if(family == 'cox') {
    ## ------------------------------ cox ---------------------------------
    ## Example : 
    #'@ fit = glmnet(x , y ,family = "cox", maxit = 1000)
    #'@ cv.fit = cv.glmnet(x , y ,family = "cox", maxit = 1000)
    #'@ risk.fit <- predict(fit, newdata, s = cv.fit$lambda.1se)
    #'@ risk.cv <- predict(cv.fit, newdata)
    ##
    
    if(!is.null(nfolds)) {
      for (i in alpha) {
        ## 1 model : tmeasure = 'cox'
        assign(paste('fit', i, sep = ''),
               cv.glmnet(x, y, type.measure = tmeasure, parallel = parallel, 
                         alpha = i/10, family = 'cox', 
                         weights = wt, maxit = maxit, nfolds = nfolds))
      }; rm(i)
    }
    
    if(!is.null(foldid)) {
      for (i in alpha) {
        ## 1 model : tmeasure = 'cox'
        assign(paste('fit', i, sep = ''),
               cv.glmnet(x, y, type.measure = tmeasure, parallel = parallel, 
                         alpha = i/10, family = 'cox', 
                         weights = wt, maxit = maxit, foldid = foldid))
      }; rm(i)
    }
    
  }  else if(family == 'mgaussian') {
    ## ------------------------------ mgaussian ---------------------------------
    
    if(!is.null(nfolds)) {
      for (i in alpha) {
        ## 1 model : tmeasure = 'mgaussian'
        assign(paste('fit', i, sep = ''),
               cv.glmnet(x, y, type.measure = tmeasure, parallel = parallel, 
                         alpha = i/10, family = 'mgaussian', 
                         weights = wt, maxit = maxit, nfolds = nfolds))
      }; rm(i)
    }
    
    if(!is.null(foldid)) {
      for (i in alpha) {
        ## 1 model : tmeasure = 'mgaussian'
        assign(paste('fit', i, sep = ''),
               cv.glmnet(x, y, type.measure = tmeasure, parallel = parallel, 
                         alpha = i/10, family = 'mgaussian', 
                         weights = wt, maxit = maxit, foldid = foldid))
      }; rm(i)
    }
    
  }else {
    stop('Kindly select family = "gaussian", family = "binomial", family = "poisson", family = "multinomial", family = "cox" or family = "mgaussian".')
  }
  
  ## ==================== Models Comparison ===============================
  ## http://stackoverflow.com/questions/29311323/difference-between-glmnet-and-cv-glmnet-in-r?answertab=votes#tab-top
  ## Never rely on glmnet's default lambda sequence! Notorious issue. 
  ##   Always provide your own sequence. Then get the optimal lambda value 
  ##   afterwards from fit$lambda.min and use it with the s=lambda.min parameter 
  ##   in all calls to predict(), coef() etc.
  #'@ predict(fit1, s = 'lambda.min')
  ## 
  
  ## http://stats.stackexchange.com/questions/121842/getting-to-predicted-values-using-cv-glmnet
  #'@ cvFit <- cv.glmnet(x = as.matrix(imputedTrainingData[,2:33]), 
  #'@                    y = imputedTrainingData[,1], family = "binomial", 
  #'@                    type.measure = "class" )
  #'@ response<-predict(cvFit, as.matrix(imputedTestData[,2:33]), s= "lambda.min")
  #'@ response <- predict(cvFit, as.matrix(imputedTestData[,2:33]), 
  #'@                     s = "lambda.min",type = "class")
  ## where type = "class" has meaning:
  ## 
  ## Type "class" applies only to
  ##   "binomial" or "multinomial" models, and produces the
  ##   class label corresponding to the maximum probability.
  ## (from ?predict.glmnet)
  ##   What you were seeing was the predicted values on the scale of the linear predictor 
  ##   (link function), i.e. before the inverse of the logit function had been applied to 
  ##   yield probability of class == 1. This is fairly typical in R, and just as typically 
  ##   this behaviour can be controlled via a type argument.
  ##
  
  #'@ yhat0  <- predict(fit0 , s = fit0$lambda.1se , newx = x)
  #'@ yhat1  <- predict(fit1 , s = fit1$lambda.1se , newx = x)
  #'@ yhat2  <- predict(fit2 , s = fit2$lambda.1se , newx = x)
  #'@ yhat3  <- predict(fit3 , s = fit3$lambda.1se , newx = x)
  #'@ yhat4  <- predict(fit4 , s = fit4$lambda.1se , newx = x)
  #'@ yhat5  <- predict(fit5 , s = fit5$lambda.1se , newx = x)
  #'@ yhat6  <- predict(fit6 , s = fit6$lambda.1se , newx = x)
  #'@ yhat7  <- predict(fit7 , s = fit7$lambda.1se , newx = x)
  #'@ yhat8  <- predict(fit8 , s = fit8$lambda.1se , newx = x)
  #'@ yhat9  <- predict(fit9 , s = fit9$lambda.1se , newx = x)
  #'@ yhat10 <- predict(fit10, s = fit10$lambda.1se, newx = x)
  
  ## 
  ## how to get probabilities between 0 and 1 using glmnet logistic regression
  ## http://stackoverflow.com/questions/26806902/how-to-get-probabilities-between-0-and-1-using-glmnet-logistic-regression
  ## 
  ## In your predict call you need the type="response" argument set. As per the 
  ##   documentation it returns the fitted probabilities.
  #'@ pred = predict(fit, s='lambda.min', newx=x_test, type="response")
  ## Also, if you are just wanted the classification labels you can use type="class"
  ## 
  
  if(!is.null(newx)) {
    eval(parse(
      text = paste(paste0(c('x', 'y', 'wt'), 
                          ' <- h(newx, family = family, yv = yv, logistic.yv = logistic.yv, wt = wt, wt.control = wt.control, xy.matrix = xy.matrix, setform = setform, .log = .log)[[', 
                          c('\'x\'', '\'y\'', '\'wt\''),']]'), collapse = '; ')))
    
    ## convert the single column y and wt into a numeric vector.
    if(!is.numeric(y)) y <- unlist(y)
    if(!is.numeric(wt)) wt <- unlist(wt)
  }
  
  ## evaluate all yhat0 to yhat10
  eval(parse(text = paste(
    paste0('yhat', alpha, ' <- predict(fit', alpha, ', s = fit', alpha, '$', s, 
           ', newx = x, type = \'', pred.type, '\')'), collapse ='; ')))
  
  #'@ mse0  <- mean((y - yhat0 )^2)
  #'@ mse1  <- mean((y - yhat1 )^2)
  #'@ mse2  <- mean((y - yhat2 )^2)
  #'@ mse3  <- mean((y - yhat3 )^2)
  #'@ mse4  <- mean((y - yhat4 )^2)
  #'@ mse5  <- mean((y - yhat5 )^2)
  #'@ mse6  <- mean((y - yhat6 )^2)
  #'@ mse7  <- mean((y - yhat7 )^2)
  #'@ mse8  <- mean((y - yhat8 )^2)
  #'@ mse9  <- mean((y - yhat9 )^2)
  #'@ mse10 <- mean((y - yhat10)^2)
  
  ## evaluate all mse0 to mse10
  eval(parse(text = paste(paste0('mse', alpha, ' <- mean((y - yhat', 
                                 alpha, ')^2)'), collapse ='; ')))
  
  ## combine all mse0 to mse10 into a data.frame named mse
  eval(parse(
    text = paste0('mse <- t(data.frame(', paste(paste0('mse', alpha), 
                                                collapse = ', '), '))')))
  ## rm all mse0 to mse10
  eval(parse(text = paste0('rm(', paste0(paste0('mse', alpha), 
                                         collapse = ', '), ')')))
  
  mse %<>% data.frame(model = rownames(.), mse = .) %>% tbl_df
  
  ## ==================== Return Function ===============================
  
  #'@ eval(parse(
  #'@   text = paste0('yhat <- list(', 
  #'@                 paste(ls(envir = .GlobalEnv, pattern = "[yhat]"), '=', 
  #'@                       ls(envir = .GlobalEnv, pattern = "[yhat]"), 
  #'@                       collapse = ', '), ')')))
  
  ## combine all yhat0 to yhat10 into a list named yhat
  eval(parse(text = paste0('yhat <- list(', paste(paste0('yhat', alpha), 
                                                  collapse = ', '), ')')))
  
  ## rm all yhat0 to yhat10
  eval(parse(text = paste0('rm(', paste0(paste0('yhat', alpha), 
                                         collapse = ', '), ')')))
  
  ## combine all fit0 to fit10 into a list named fit
  eval(parse(text = paste0('fit <- list(', paste(paste0('fit', alpha), 
                                                  collapse = ', '), ')')))
  
  ## rm all fit0 to fit10
  eval(parse(text = paste0('rm(', paste0(paste0('fit', alpha), 
                                         collapse = ', '), ')')))
  
  tmp <- list(fit = fit, yhat = yhat, mse = mse)
  
  ### Stop cluster
  stopCluster(cl)
  
  options(warn = 0)
  
  return(tmp)
}