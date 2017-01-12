lmStocks <- function(mbase) {
  ## ========================= Load Packages ===================================
  # http://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection
  suppressPackageStartupMessages(library("BBmisc"))
  suppressAll(library('lubridate'))
  suppressAll(library('glmnet'))
  
  ## ======================= Data Validation ===================================
  if(is.xts(mbase)) {
    mxt <- as.matrix(mbase)
  } else {
    stop('Kindly apply filterAAPL and fit the dataset into the function.')
  }
  
  dateID <- rownames(mxt) %>% ymd
  
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
  
  h <- function() {
    
    AAPLDT_DF <- AAPLDT[, 1:5] %>% gather(Category, Price, AAPL.Open:AAPL.Close) %>% 
      mutate(Date = factor(Date), Category = 
               factor(Category, 
                      levels = c('AAPL.Open', 'AAPL.High', 'AAPL.Low', 'AAPL.Close')), 
             wt = 1, b0 = Price / first(Price))
    #'@ contrasts(AAPLDT_DF$Date) <- contr.treatment(AAPLDT_DF$Date)
    #'@ attr(AAPLDT_DF$Category, 'levels') <- c('AAPL.Open', 'AAPL.High', 'AAPL.Low', 'AAPL.Close')
    tmp <- model.matrix(Category ~ Date + Price + wt + b0, data = AAPLDT_DF) %>% 
      tbl_df %>% mutate(Category = AAPLDT_DF$Category)
    return(tmp)
  }
  
  x <- h()
  y <- ddply(x, .(Category), summarise, 
             resp = sum(((Price * wt * b0) - Price)^2) / nrow(x)) %>% tbl_df
  xy <- join(x, y) %>% tbl_df
  
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
  
  glmmod <- glmnet(dplyr::select(xy, -c(Date, resp)), y = dplyr::select(xy, resp), alpha = 1, family = 'binomial')
  
  ## Test cv.glmnet with binomial and multinomial logistic regression.
  # http://faculty.chicagobooth.edu/max.farrell/bus41100/week8-Rcode.R
  fit1 <- cv.glmnet(x = as.matrix(AAPL), y = as.matrix(AAPL)[, 1], family = 'multinomial', type.measure = 'class')
  
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