lmStocks <- function(mbase, family = 'binomial', alpha, yv = 'daily mean', tmeasure = 'deviance', 
                     nfolds = 10, s = 'lambda.min', weight.date = NULL, weight.volume = NULL, 
                     parallel = FALSE, .log = FALSE) {
  ## mbase = default AAPL or in data frame format.
  ## family = 'gaussian', 'binomial', 'multinomial'
  
  
  ## ========================= Load Packages ===================================
  # http://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection
  suppressPackageStartupMessages(library("BBmisc"))
  suppressAll(library('lubridate'))
  suppressAll(library('MASS'))
  suppressAll(library('glmnet'))
  suppressAll(library('Matrix'))
  suppressAll(library('broom'))
  suppressAll(library('parallel'))
  suppressAll(library('doParallel'))
  doParallel::registerDoParallel(cores = parallel::detectCores())
  
  ## ======================= Data Validation ===================================
  if(is.xts(mbase)) {
    mbase <- as.matrix(mbase) %>% data.frame %>% data.frame(Date = rownames(.), .) %>% 
      tbl_df %>% mutate(Date = ymd(Date))
  } else if(is.data.frame(mbase)) {
    mbase <- mbase
  } else {
    stop('Kindly apply filterAAPL and fit the dataset into the function.')
  }
  
  dateID <- sort(unique(mbase$Date))
  
  ## ========================= Weight Function =================================
  ## http://www.ats.ucla.edu/stat/r/library/contrast_coding.htm
  ## set the first dat as baseline.
  #
  #hsb2 = read.table('http://www.ats.ucla.edu/stat/data/hsb2.csv', header=T, sep=",")
  #
  #creating the factor variable race.f
  #hsb2$race.f = factor(hsb2$race, labels=c("Hispanic", "Asian", "African-Am", "Caucasian"))
  #Before considering any analyses, let's look at the mean of the dependent variable, write, for each level of race.  This will help in interpreting the output from later analyses.
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
  
  h <- function(ddt) {
    
    #AAPLDT_DF <- AAPLDT[, 1:5] %>% gather(Category, Price, AAPL.Open:AAPL.Close) %>% 
    #  mutate(Date = as.character(Date), Category = factor(
    #    Category, levels = c('AAPL.Open', 'AAPL.High', 'AAPL.Low', 'AAPL.Close')), 
    #    wt = 1, b0 = Price / first(Price)) #set `Date` as a ccategory variable.
    
    ddt_DF <- ddt[, 1:5] %>% gather(Category, Price, AAPL.Open:AAPL.Close) %>% 
      mutate(Category = factor(
        Category, levels = c('AAPL.Open', 'AAPL.High', 'AAPL.Low', 'AAPL.Close')))
         #let `Date` be numeric variable as convert by system.
    
    Y <- ddt_DF %>% mutate(wt = 1, b0 = Price / first(Price))
    Y <- ddply(Y, .(Date), transform, dmean = mean(Price)) %>% tbl_df
    #> mean(y$b0)
    #[1] 1.009086
    
    ## ----------------- start omit below codes ---------------------------------------
    #'@ contrasts(AAPLDT_DF$Category) <- contr.treatment(AAPLDT_DF$Category)
    #'@ attr(AAPLDT_DF$Category, 'levels') <- c('AAPL.Open', 'AAPL.High', 'AAPL.Low', 'AAPL.Close')
    #'@ attr(AAPLDT_DF$Category,'contrasts') <- contrasts(C(factor(AAPLDT_DF$Category), 'contr.treatment'))
    
    #'@ tmp <- model.matrix(Category ~ Date + Price + wt + b0, data = AAPLDT_DF) %>% 
    #'@   tbl_df %>% mutate(Category = AAPLDT_DF$Category)
    ## -------------------------- end omit codes ---------------------------------------
    tmp <- list(x = sparse.model.matrix(~ -1 + ., ddt_DF[, -1]), y = Y)
    
    return(tmp)
  }
  
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
  #ridge.coef.DEF <- drop(solve(crossprod(X) + diag(n.tmp * ridge.fit.lambda, p.tmp), crossprod(X, Y)))
  #
  ## Plot estimates
  #plot(ridge.coef, type = "l", ylim = range(c(ridge.coef, ridge.coef.DEF)),
  #     main = "black: Ridge `glmnet`\nred: Ridge by definition")
  #lines(ridge.coef.DEF, col = "red")
  #x <- sparse.model.matrix(~., mbase[, 2:5])
  #wt <- rep(0, nrow(x))
  #y.true <- x %*% wt
  #x <- h()
  #y <- AAPLDT['AAPL.Volume']
  #y <- ddply(x, .(Category), summarise, 
  #           resp = sum(((Price * wt * b0) - Price)^2) / nrow(x)) %>% tbl_df
  #xy <- join(x, y) %>% tbl_df
  #xy <- h(mbase)
  eval(parse(text = paste(paste0(c('x', 'y'), ' <- h(mbase)[[', c('\'x\'', '\'y\''),']]'), 
                          collapse = '; ')))
  
  ## ======================= Parameter Adjustment ==================================
  if(.log == TRUE) y %<>% mutate(b0 = log(b0), dmean =log(dmean))
  
  yt <- rep(0, nrow(y))
  if(yv == 'daily mean') { 
    yt <- y$dmean
  } else if(yv == 'baseline') {
    yt <- y$dmean
  } else {
    stop('Kindly select "yv = daily mean" or "yv = baseline".')
  }
  
  ## “lambda.1se”: the largest λλ at which the MSE is within one standard error of the minimal MSE.
  ## “lambda.min”: the λλ at which the minimal MSE is achieved.
  if(s == 'lambda.1se') {
    s <- 'lambda.1se'
  } else if(s == 'lambda.min') {
    s <- 'lambda.min'
  } else {
    stop('Kindly select s = "lambda.1se" or s = "lambda.min".')
  }
  
  if(tmeasure == 'deviance') { #which uses squared-error for gaussian models
    tmeasure <- 'deviance'     #deviance for logistic and poisson regression, and partial-likelihood for the Cox model.
  } else if(tmeasure == 'mse') { #can be used by all models except the "cox"
    tmeasure <- 'mse'
  } else if(tmeasure == 'mae') { #can be used by all models except the "cox"
    tmeasure <- 'mae'
  } else if(tmeasure == 'class') {#applies to binomial and multinomial logistic regression only
    tmeasure <- 'class'
  } else if(tmeasure == 'auc') {  #for two-class logistic regression only, and gives area under the ROC curve.
    tmeasure <- 'auc'
  } else if(tmeasure == 'cox') {
    tmeasure <- 'cox'
  } else {
    stop('Kindly select tmeasure = "deviance", tmeasure = "mse", tmeasure = "mae", tmeasure = "class", tmeasure = "cox".')
  }
  
  ## ==================== Regression Models ===============================
  if(family == 'gaussian') {
    ## -------------------------- gaussian --------------------------------
    ## standardize is a logical flag for x variable standardization, prior to fitting the model sequence. 
    ##   The coefficients are always returned on the original scale. Default is standardize=TRUE.
    ## 
    ## http://stackoverflow.com/questions/17887747/how-does-glmnets-standardize-argument-handle-dummy-variables?answertab=votes#tab-top
    ## If you take a look at the R function, glmnet codes the standardize parameter internally as `isd = as.integer(standardize)`.
    ## 
    for (i in 0:10) {
      ## 3 models : tmeasure = 'deviance' or tmeasure = 'mse' or tmeasure = 'mae'
      assign(paste('fit', i, sep = ''),
             cv.glmnet(x, yt, type.measure = tmeasure, parallel = parallel, 
                       alpha = i/10, family = 'gaussian', nfolds = 10))
      }; rm(i)
    
  } else if(family == 'binomial') {
    ## -------------------------- binomial --------------------------------
    ## https://rstudio-pubs-static.s3.amazonaws.com/71750_a733595676d3437f940244bc678b4f1f.html
    ## http://ricardoscr.github.io/how-to-use-ridge-and-lasso-in-r.html
    for (i in 0:10) {
      ## 5 models : tmeasure = 'deviance' or tmeasure = 'mse' or 
      ##            tmeasure = 'mae' or tmeasure = 'class' or 
      ##            tmeasure = 'auc'.
      assign(paste('fit', i, sep = ''),
             cv.glmnet(x, yt, type.measure = tmeasure, parallel = parallel, 
                       alpha = i/10, family = 'binomial', nfolds = 10))
    }; rm(i)
    
  } else if(family == 'poisson') {
    ## -------------------------- poisson ---------------------------------
    for (i in 0:10) {
      ## 3 models : tmeasure = 'deviance' or tmeasure = 'mse' or tmeasure = 'mae'
      assign(paste('fit', i, sep = ''),
             cv.glmnet(x, yt, type.measure = tmeasure, parallel = parallel, 
                       alpha = i/10, family = 'poisson', nfolds = 10))
    }; rm(i)
    
  } else if(family == 'multinomial') {
    ## ----------------------- multinomial --------------------------------
    for (i in 0:10) {
      ## 4 models : tmeasure = 'deviance' or tmeasure = 'mse' or 
      ##            tmeasure = 'mae' or tmeasure = 'class'
      assign(paste('fit', i, sep = ''),
             cv.glmnet(x, yt, type.measure = tmeasure, parallel = parallel, 
                       alpha = i/10, family = 'multinomial', nfolds = 10))
    }; rm(i)
    
  } else if(family == 'cox') {
    ## ------------------------------ cox ---------------------------------
    for (i in 0:10) {
      ## 1 model : tmeasure = 'cox'
      assign(paste('fit', i, sep = ''),
             cv.glmnet(x, yt, type.measure = tmeasure, parallel = parallel, 
                       alpha = i/10, family = 'cox', nfolds = 10))
    }; rm(i)
    
  } else {
    stop('Kindly select family == "gaussian", family == "binomial", family == "poisson", family == "multinomial" or family == "cox".')
  }
  
  ## 
  ## In order to avoid llply() occupy a lot of RAM, here I seperate the model independently.
  ## 
  ## binomial
  ## 
  # 
  #cv.glmnet(x, y, family='binomial', alpha=1, parallel = parallel, standardize=TRUE, type.measure='auc')
  #cv.glmnet(x, y, family='binomial', alpha=1, parallel = parallel, standardize=TRUE, type.measure='auc')
  
  ## multinomial
  ## http://faculty.chicagobooth.edu/max.farrell/bus41100/week8-Rcode.R
  # 
  #cv.glmnet(X, class, standardize=FALSE, family='multinomial', alpha=1, nfolds = 10)
  #for (i in 0:10) {
  #  assign(paste('fit.m', i, sep = ''), cv.glmnet(x, y = yt, family = 'multinomial', 
  #                                                type.measure = 'class', parallel = parallel, 
  #                                                alpha = i/10, nfolds = 10))}
  #rm(i)
  
  yhat.b0  <- predict(fit.b0 , s = fit.b0$lambda.1se , newx = x)
  yhat.b1  <- predict(fit.b1 , s = fit.b1$lambda.1se , newx = x)
  yhat.b2  <- predict(fit.b2 , s = fit.b2$lambda.1se , newx = x)
  yhat.b3  <- predict(fit.b3 , s = fit.b3$lambda.1se , newx = x)
  yhat.b4  <- predict(fit.b4 , s = fit.b4$lambda.1se , newx = x)
  yhat.b5  <- predict(fit.b5 , s = fit.b5$lambda.1se , newx = x)
  yhat.b6  <- predict(fit.b6 , s = fit.b6$lambda.1se , newx = x)
  yhat.b7  <- predict(fit.b7 , s = fit.b7$lambda.1se , newx = x)
  yhat.b8  <- predict(fit.b8 , s = fit.b8$lambda.1se , newx = x)
  yhat.b9  <- predict(fit.b9 , s = fit.b9$lambda.1se , newx = x)
  yhat.b10 <- predict(fit.b10, s = fit.b10$lambda.1se, newx = x)
  
  mse.b0  <- mean((yt - yhat.b0 )^2)
  mse.b1  <- mean((yt - yhat.b1 )^2)
  mse.b2  <- mean((yt - yhat.b2 )^2)
  mse.b3  <- mean((yt - yhat.b3 )^2)
  mse.b4  <- mean((yt - yhat.b4 )^2)
  mse.b5  <- mean((yt - yhat.b5 )^2)
  mse.b6  <- mean((yt - yhat.b6 )^2)
  mse.b7  <- mean((yt - yhat.b7 )^2)
  mse.b8  <- mean((yt - yhat.b8 )^2)
  mse.b9  <- mean((yt - yhat.b9 )^2)
  mse.b10 <- mean((yt - yhat.b10)^2)
  
  #yhat.m0  <- predict(fit.m0 , s = fit.m0$lambda.1se , newx = x)
  #yhat.m1  <- predict(fit.m1 , s = fit.m1$lambda.1se , newx = x)
  #yhat.m2  <- predict(fit.m2 , s = fit.m2$lambda.1se , newx = x)
  #yhat.m3  <- predict(fit.m3 , s = fit.m3$lambda.1se , newx = x)
  #yhat.m4  <- predict(fit.m4 , s = fit.m4$lambda.1se , newx = x)
  #yhat.m5  <- predict(fit.m5 , s = fit.m5$lambda.1se , newx = x)
  #yhat.m6  <- predict(fit.m6 , s = fit.m6$lambda.1se , newx = x)
  #yhat.m7  <- predict(fit.m7 , s = fit.m7$lambda.1se , newx = x)
  #yhat.m8  <- predict(fit.m8 , s = fit.m8$lambda.1se , newx = x)
  #yhat.m9  <- predict(fit.m9 , s = fit.m9$lambda.1se , newx = x)
  #yhat.m10 <- predict(fit.m10, s = fit.m10$lambda.1se, newx = x)
  
  #mse.m0  <- mean((yt - yhat.m0 )^2)
  #mse.m1  <- mean((yt - yhat.m1 )^2)
  #mse.m2  <- mean((yt - yhat.m2 )^2)
  #mse.m3  <- mean((yt - yhat.m3 )^2)
  #mse.m4  <- mean((yt - yhat.m4 )^2)
  #mse.m5  <- mean((yt - yhat.m5 )^2)
  #mse.m6  <- mean((yt - yhat.m6 )^2)
  #mse.m7  <- mean((yt - yhat.m7 )^2)
  #mse.m8  <- mean((yt - yhat.m8 )^2)
  #mse.m9  <- mean((yt - yhat.m9 )^2)
  #mse.m10 <- mean((yt - yhat.m10)^2)
  
  mse1 <- eval(parse(
    text = paste0('t(data.frame(', paste(paste0('mse.b', 1:10), collapse = ', '), '))')))
  paste0('rm(mse.b', 1:10,')')
  
  mse1 %<>% data.frame(model = rownames(.), mse = .) %>% tbl_df
  #                [,1]
  #mse.b1  2.530778e-05
  #mse.b2  2.523219e-05
  #mse.b3  2.534738e-05
  #mse.b4  2.533257e-05
  #mse.b5  2.526375e-05
  #mse.b6  2.527281e-05
  #mse.b7  2.533669e-05
  #mse.b8  2.545001e-05
  #mse.b9  2.561610e-05
  #mse.b10 2.558813e-05
  mse1[mse1$mse == min(mse1$mse), ]
  # A tibble: 1 × 2
  #model          mse
  #<fctr>        <dbl>
  #  1 mse.b2 2.523219e-05
  
  ## ==================== Test Regression Models ===============================
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
  
  #glmmod <- glmnet(dplyr::select(xy, -c(Category, resp)), y = dplyr::select(xy, resp), alpha = 1, family = 'binomial')
  
  ## Test cv.glmnet with binomial and multinomial logistic regression.
  # http://faculty.chicagobooth.edu/max.farrell/bus41100/week8-Rcode.R
  #fit1 <- cv.glmnet(x = as.matrix(AAPL), y = as.matrix(AAPL)[, 1], family = 'multinomial', type.measure = 'class')
  
  ## http://stackoverflow.com/questions/29311323/difference-between-glmnet-and-cv-glmnet-in-r?answertab=votes#tab-top
  ## Never rely on glmnet's default lambda sequence! Notorious issue. Always provide your own sequence. Then get the optimal lambda value afterwards from fit$lambda.min and use it with the s=lambda.min parameter in all calls to predict(), coef() etc.
  predict(fit1, s = 'lambda.min')
  
  ## testing...
  ## http://faculty.chicagobooth.edu/max.farrell/bus41100/week8-Rcode.R
  cv.glmnet(x = mxt[, 1:4], y = rowMeans(mxt[, 1:4]), family = 'multinomial', type.measure = 'class')
  
  ## not in slides
  ## Lasso with multinomial logistic regression
  ##take a random sample of the data to select variables. Doing cv.glmnet on the full data set can be very slow.
  ##eventually, try it without this step (and remove all the [indexes.to.keep] and [indexes.to.keep,] from the glmnet command
  indexes.to.keep <- sample(1:length(mpg.data$hwy.factor), 2000)
  
  X <- model.matrix(~(trans + cyl + displ)^2, mpg.data)
  #need to subtract the intercept
  X <- X[,-1]
  
  cv.fit <- cv.glmnet(X[indexes.to.keep,], mpg.data$hwy.factor[indexes.to.keep], family=c("multinomial"), alpha=1, intercept=TRUE, standardize=FALSE, type.measure="class", type.multinomial="grouped")
  
  lambda.index <- which(cv.fit$lambda==cv.fit$lambda.min) #or use lambda.min at the end
  
  selected.variables <- unlist(predict(cv.fit, type="nonzero")[lambda.index])
  
  ## if you want to put more variables back in:
  refitting.variables <- sort(union(which(colnames(X)=="cyl"), selected.variables))
  # or just set refitting.variables <- selected.variables
  
  ##refit on the full data
  mlogit.post.lasso <- multinom(hwy.factor ~ X[,refitting.variables], data=mpg.data)
  
  
  ## testing 2...
  fit = glmnet(x , y ,family = "cox", maxit = 1000)
  cv.fit = cv.glmnet(x , y ,family = "cox", maxit = 1000)
  
  risk.fit <- predict(fit, newdata, s = cv.fit$lambda.1se)
  risk.cv <- predict(cv.fit, newdata)
  
  ## http://stats.stackexchange.com/questions/121842/getting-to-predicted-values-using-cv-glmnet
  cvFit <- cv.glmnet(x = as.matrix(imputedTrainingData[,2:33]), y = imputedTrainingData[,1], family = "binomial", type.measure = "class" )
  
  response<-predict(cvFit, as.matrix(imputedTestData[,2:33]), s= "lambda.min")
  
  response <- predict(cvFit, as.matrix(imputedTestData[,2:33]),
                      s = "lambda.min",
                      type = "class")
  ## where type = "class" has meaning:
  ## 
  ## Type ‘"class"’ applies only to
  ##   ‘"binomial"’ or ‘"multinomial"’ models, and produces the
  ##   class label corresponding to the maximum probability.
  ## (from ?predict.glmnet)
  ##   What you were seeing was the predicted values on the scale of the linear predictor 
  ##   (link function), i.e. before the inverse of the logit function had been applied to 
  ##   yield probability of class == 1. This is fairly typical in R, and just as typically 
  ##   this behaviour can be controlled via a type argument.
  
  ## http://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection?answertab=votes#tab-top
  
  
  return()
}