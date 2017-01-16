h <- function(ddt, family, yv = 'baseline', wt, xy.matrix = 'h1', .log = .log) {
  ## mbase = default LAD or in data frame format.
  ## 
  ## family = gaussian', 'binomial', 'poisson', 'multinomial', 'cox' and 'mgaussian'.
  ## 
  ## xy.matrix = 'h1' or xy.matrix = 'h2'. setting x and y variables.
  ## 
  ## yvs <- c('baseline', 'close1', 'close2', 'daily.mean1', 'daily.mean2', 'daily.mean3', 
  ##   'mixed1', 'mixed2', 'mixed3') #to model the y (respondence variables)
  ##   baseline only use first element of opening price as baseline.
  ##   close1 = X = data.frame(Op(LAD), Hi(LAD), Lo(LAD)) and Y = Cl(LAD), 
  ##   close2 = X = data.frame(Op(LAD), Hi(LAD), Lo(LAD), Cl(LAD)) and Y = Cl(LAD), 
  ##   daily.mean1 = mean(Op(LAD), Cl(LAD)), daily.mean2 = mean(Hi(LAD), Lo(LAD)), 
  ##   daily.mean3 = mean(Op(LAD), Hi(LAD), Lo(LAD), Cl(LAD)).
  ##   mixed1,2,3 are the Y = baseline * daily.mean1,2,3
  
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
  suppressPackageStartupMessages(library("BBmisc"))
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
  
  xy.matries <- c('h1', 'h2')
  if(xy.matrix %in% xy.matries) {
    xy.matrix <- xy.matrix
  } else {
    stop('xy.matrix must be one among c(\'', paste(xy.matries, collapse = '\', \''), '\').')
  }
  
  if(!is.null(wt)) {
    if(is.numeric(wt)) {
      wt <- wt
    } else if(anyNA(suppressWarnings(as.numeric(wt)))) {
      stop('Kindly select a numeric vector as weight parameters.')
    }
  } else {
    wt <- 1
  }
  
  ## ============================= Regression Model ===================================
  if(family %in% families[c(1, 3)]) {
    ## -------------------------- gaussian or poisson --------------------------------
    if(yv %in% c('baseline', 'close1', 'close2')) {
      X <- ddt[, 1:6]
      
    } else if(yv %in%  c('daily.mean1', 'mixed1')) {
      X <- ddt[, 1:6] %>% mutate(dmean1 = (LAD.Open + LAD.Close) / 2)
      
    } else if(yv %in% c('daily.mean2', 'mixed2')) {
      X <- ddt[, 1:6] %>% mutate(dmean2 = (LAD.High + LAD.Low) / 2)
      
    } else {
      X <- ddt[, 1:6] %>% 
        mutate(dmean3 = (LAD.Open + LAD.High + LAD.Low + LAD.Close) / 4)
    }
    
    if(xy.matrix == 'h1') {
      ## ---------------------------------- h1 ---------------------------------------
      #LADDT_DF <- LADDT[, 2:5] %>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
      #  mutate(Date = as.character(Date), Category = factor(
      #    Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')), 
      #    wt = 1, b0 = Price / first(Price)) #set `Date` as a category variable.
      
      if(yv == 'baseline') {
        X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
          mutate(Category = factor(
            Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
          arrange(Date)
        #let `Date` be numeric variable as convert by system.
        
        Y <- X %>% mutate(wt = wt, b0 = Price / first(Price))
        Y %<>% .[c('wt', 'b0')]
        X %<>% .[c('LAD.Volume', 'Category', 'Price')]
        rm(wt)
      }
      
      if(yv == 'close1') {
        X %<>% gather(Category, Price, LAD.Open:LAD.Low) %>% 
          mutate(Category = factor(
            Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low'))) %>% 
          arrange(Date)
        
        Y <- X['LAD.Close'] %>% mutate(wt = wt)
        Y %<>% .[c('wt', 'LAD.Close')]
        X %<>% .[c('LAD.Volume', 'Category', 'Price')]
        rm(wt)
      }
      
      if(yv == 'close2') {
        X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
          mutate(Category = factor(
            Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
          arrange(Date)
        
        Y <- X['LAD.Close'] %>% mutate(wt = wt)
        Y %<>% .[c('wt', 'LAD.Close')]
        X %<>% .[c('LAD.Volume', 'Category', 'Price')]
        rm(wt)
      }
      
      if(yv == 'daily.mean1') {
        X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
          mutate(Category = factor(
            Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
          arrange(Date)
        #let `Date` be numeric variable as convert by system.
        
        Y <- X %>% mutate(wt = wt)
        Y %<>% .[c('wt', 'dmean1')]
        X %<>% .[c('LAD.Volume', 'Category', 'Price')]
        rm(wt)
      }
      
      if(yv == 'daily.mean2') {
        X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
          mutate(Category = factor(
            Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
          arrange(Date)
        #let `Date` be numeric variable as convert by system.
        
        Y <- X %>% mutate(wt = wt)
        Y %<>% .[c('wt', 'dmean2')]
        X %<>% .[c('LAD.Volume', 'Category', 'Price')]
        rm(wt)
      }
      
      if(yv == 'daily.mean3') {
        X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
          mutate(Category = factor(
            Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
          arrange(Date)
        #let `Date` be numeric variable as convert by system.
        
        Y <- X %>% mutate(wt = wt)
        Y %<>% .[c('wt', 'dmean3')]
        X %<>% .[c('LAD.Volume', 'Category', 'Price')]
        rm(wt)
      }
      
      if(yv == 'mixed1') {
        X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
          mutate(Category = factor(
            Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
          arrange(Date)
        #let `Date` be numeric variable as convert by system.
        
        Y <- X %>% mutate(wt = wt, mixed1 = (Price / first(Price)) * dmean1)
        Y %<>% .[c('wt', 'mixed1')]
        X %<>% .[c('LAD.Volume', 'Category', 'Price')]
        rm(wt)
      }
      
      if(yv == 'mixed2') {
        X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
          mutate(Category = factor(
            Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
          arrange(Date)
        #let `Date` be numeric variable as convert by system.
        
        Y <- X %>% mutate(wt = wt, mixed2 = (Price / first(Price)) * dmean2)
        Y %<>% .[c('wt', 'mixed2')]
        X %<>% .[c('LAD.Volume', 'Category', 'Price')]
        rm(wt)
      }
      
      if(yv == 'mixed3') {
        X %<>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
          mutate(Category = factor(
            Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
          arrange(Date)
        #let `Date` be numeric variable as convert by system.
        
        Y <- X %>% mutate(wt = wt, mixed3 = (Price / first(Price)) * dmean3)
        Y %<>% .[c('wt', 'mixed3')]
        X %<>% .[c('LAD.Volume', 'Category', 'Price')]
        rm(wt)
      }
      
      ## ----------------- start omit below codes ---------------------------------------
      #'@ contrasts(LADDT_DF$Category) <- contr.treatment(LADDT_DF$Category)
      #'@ attr(LADDT_DF$Category, 'levels') <- c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close')
      #'@ attr(LADDT_DF$Category,'contrasts') <- contrasts(C(factor(LADDT_DF$Category), 
      #'@                                                     'contr.treatment'))
      
      #'@ tmp <- model.matrix(Category ~ Date + Price + wt + b0, data = LADDT_DF) %>% 
      #'@   tbl_df %>% mutate(Category = LADDT_DF$Category)
      ## -------------------------- end omit codes ---------------------------------------
      
      if(.log == TRUE) {
        X %<>% mutate_each(funs(log))
        Y %<>% mutate_each(funs(log))
      }
      
      #'@ suppressWarnings(build.x(~ -1 + ., ddt_DF, contrasts = TRUE))
      
      #set X_i0 as baseline (intercept).
      tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y)
      
    } else if(xy.matrix == 'h2') {
      ## ---------------------------------- h2 ---------------------------------------
      X <- ddt_DF[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 'LAD.Volume')]
      #'@ data.frame(as.matrix(X[, -5]) / as.numeric(X[1, 1])) %>% tbl_df
      
      Y <- ddt_DF %>% mutate(dmean = rowMeans(.), wt = 1, b0 = dmean / first(dmean))
      Y <- Y[c('wt', 'b0', 'dmean')]
      
      if(yv == 'baseline') {
        Y <- X %>% mutate(wt = wt, b0 = Price / first(Price))
        Y %<>% .[c('wt', 'b0')]
        X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 'LAD.Volume')]
        rm(wt)
      }
      
      if(yv == 'close1') {
        Y <- X['LAD.Close'] %>% mutate(wt = wt)
        Y %<>% .[c('wt', 'LAD.Close')]
        X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Volume')]
        rm(wt)
      }
      
      if(yv == 'close2') {
        Y <- X['LAD.Close'] %>% mutate(wt = wt)
        Y %<>% .[c('wt', 'LAD.Close')]
        X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 'LAD.Volume')]
        rm(wt)
      }
      
      if(yv == 'daily.mean1') {
        Y <- X %>% mutate(wt = wt)
        Y %<>% .[c('wt', 'dmean1')]
        X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 'LAD.Volume')]
        rm(wt)
      }
      
      if(yv == 'daily.mean2') {
        Y <- X %>% mutate(wt = wt)
        Y %<>% .[c('wt', 'dmean2')]
        X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 'LAD.Volume')]
        rm(wt)
      }
      
      if(yv == 'daily.mean3') {
        Y <- X %>% mutate(wt = wt)
        Y %<>% .[c('wt', 'dmean3')]
        X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 'LAD.Volume')]
        rm(wt)
      }
      
      if(yv == 'mixed1') {
        Y <- X %>% mutate(wt = wt, mixed1 = (Price / first(Price)) * dmean1)
        Y %<>% .[c('wt', 'mixed1')]
        X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 'LAD.Volume')]
        rm(wt)
      }
      
      if(yv == 'mixed2') {
        Y <- X %>% mutate(wt = wt, mixed2 = (Price / first(Price)) * dmean2)
        Y %<>% .[c('wt', 'mixed1')]
        X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 'LAD.Volume')]
        rm(wt)
      }
      
      if(yv == 'mixed3') {
        Y <- X %>% mutate(wt = wt, mixed3 = (Price / first(Price)) * dmean3)
        Y %<>% .[c('wt', 'mixed1')]
        X %<>% .[c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close', 'LAD.Volume')]
        rm(wt)
      }
      
      if(.log == TRUE) {
        X %<>% mutate_each(funs(log))
        Y %<>% mutate_each(funs(log))
      }
      
      #'@ suppressWarnings(build.x(~ -1 + ., ddt_DF, contrasts = TRUE))
      
      #set X_i0 as baseline (intercept).
      tmp <- list(x = sparse.model.matrix(~ -1 + ., X), y = Y)
    
    } else {
      stop('Kindly set xy.matrix = "h1" or xy.matrix = "h2".')
    }
    
    
    return(tmp)
    
  } else if(family %in% families[c(2, 4)]) {
    ## -------------------------- binomial or multinomial ----------------------------
    if(yv %in% c('baseline', 'close1', 'close2')) {
      X <- ddt[, 1:6]
      
    } else if(yv %in%  c('daily.mean1', 'mixed1')) {
      X <- ddt[, 1:6] %>% mutate(dmean1 = (LAD.Open + LAD.Close) / 2)
      
    } else if(yv %in% c('daily.mean2', 'mixed2')) {
      X <- ddt[, 1:6] %>% mutate(dmean2 = (LAD.High + LAD.Low) / 2)
      
    } else {
      X <- ddt[, 1:6] %>% 
        mutate(dmean3 = (LAD.Open + LAD.High + LAD.Low + LAD.Close) / 4)
    }
    
    if(xy.matrix == 'h1') {
      ## ---------------------------------- h1 ---------------------------------------
      
      ddt_DF <- ddt[, 1:6] %>% gather(Category, Price, LAD.Open:LAD.Close) %>% 
        mutate(Category = factor(
          Category, levels = c('LAD.Open', 'LAD.High', 'LAD.Low', 'LAD.Close'))) %>% 
        arrange(Date)
      #let `Date` be numeric variable as convert by system.
      
      Y <- ddt_DF %>% mutate(wt = 1, b0 = ifelse(Category == 'LAD.Open', 1, 0))
      Y <- ddply(Y, .(Date), transform, dmean = mean(Price)) %>% tbl_df
      
      ddt_DF <- ddt_DF[, -1]
      Y <- Y[-c(1:4)]
      
      ## 
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
      ## 
      
      #set X_i0 as baseline (intercept).
      tmp <- list(x = build.x(~ -1 + ., X, sparse = TRUE), y = Y)
      
    } else if(xy.matrix == 'h2') {
      ## ---------------------------------- h2 ---------------------------------------
      ddt_DF <- ddt[, 2:5]
      Y <- ddt_DF %>% mutate(dmean = rowMeans(.), wt = 1, b0 = dmean / first(dmean))
      Y <- Y[c('wt', 'b0', 'dmean')]
      
      #set X_i0 as baseline (intercept).
      tmp <- list(x = sparse.model.matrix(~ -1 + ., ddt_DF), y = Y)
      
    } else {
      stop('Kindly set xy.matrix = "h1" or xy.matrix = "h2".')
    }
    
    if(.log == TRUE) {
      ddt_DF %<>% mutate_each(funs(log))
      Y %<>% mutate_each(funs(log))
    }
    
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
    
    return(tmp)
    
  } else {
    stop('The regression model that you choose not yet ready.')
  }
  
}
