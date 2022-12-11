source('./function/h.R')
family = 'gaussian'; logistic.yv = TRUE; wt = NULL; wt.control = TRUE; setform = 'l1'; xy.matrix = 'h1'

h(LADDT, family = family, yv = 'baseline', logistic.yv = logistic.yv, xy.matrix = xy.matrix, wt = wt, wt.control = wt.control, setform = setform)
h(LADDT, family = family, yv = 'close1', logistic.yv = logistic.yv, xy.matrix = xy.matrix, wt = wt, wt.control = wt.control, setform = setform)
h(LADDT, family = family, yv = 'close2', logistic.yv = logistic.yv, xy.matrix = xy.matrix, wt = wt, wt.control = wt.control, setform = setform)
h(LADDT, family = family, yv = 'daily.mean1', logistic.yv = logistic.yv, xy.matrix = xy.matrix, wt = wt, wt.control = wt.control, setform = setform)
h(LADDT, family = family, yv = 'daily.mean2', logistic.yv = logistic.yv, xy.matrix = xy.matrix, wt = wt, wt.control = wt.control, setform = setform)
h(LADDT, family = family, yv = 'daily.mean3', logistic.yv = logistic.yv, xy.matrix = xy.matrix, wt = wt, wt.control = wt.control, setform = setform)
h(LADDT, family = family, yv = 'mixed1', logistic.yv = logistic.yv, xy.matrix = xy.matrix, wt = wt, wt.control = wt.control, setform = setform)
h(LADDT, family = family, yv = 'mixed2', logistic.yv = logistic.yv, xy.matrix = xy.matrix, wt = wt, wt.control = wt.control, setform = setform)
h(LADDT, family = family, yv = 'mixed3', logistic.yv = logistic.yv, xy.matrix = xy.matrix, wt = wt, wt.control = wt.control, setform = setform)


source('./function/glmPrice.R')


mbase = LADDT; family = 'gaussian'; xy.matrix = 'h1'; setform = 'l1'; 
yv = 'baseline'; logistic.yv = TRUE; tmeasure = 'deviance'; 
tmultinomial = 'grouped'; maxit = 1000; pred.type = 'class'; 
alpha = 0:10; nfolds = 10; foldid = NULL; s = 'lambda.min'; 
weight.date = FALSE; weight.volume = FALSE; wt.control = FALSE; 
parallel = TRUE; .log = FALSE

glmPrice(mbase = LADDT, family = 'gaussian', xy.matrix = 'h1', setform = 'l1', 
         yv = 'baseline', logistic.yv = TRUE, tmeasure = 'deviance', 
         tmultinomial = 'grouped', maxit = 1000, pred.type = 'class', 
         alpha = 0:10, nfolds = 10, foldid = NULL, s = 'lambda.min', 
         weight.date = FALSE, weight.volume = FALSE, wt.control = FALSE, 
         parallel = TRUE, .log = FALSE)



