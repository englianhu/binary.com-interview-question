source('./function/h.R')
family = 'multinomial'; logistic.yv = TRUE; wt = NULL; wt.control = TRUE; setform = 'l4'; xy.matrix = 'h2'

is.error(h(LADDT, family = family, yv = 'baseline', logistic.yv = logistic.yv, xy.matrix = xy.matrix, wt = wt, wt.control = wt.control, setform = setform))
is.error(h(LADDT, family = family, yv = 'close1', logistic.yv = logistic.yv, xy.matrix = xy.matrix, wt = wt, wt.control = wt.control, setform = setform))
is.error(h(LADDT, family = family, yv = 'close2', logistic.yv = logistic.yv, xy.matrix = xy.matrix, wt = wt, wt.control = wt.control, setform = setform))
is.error(h(LADDT, family = family, yv = 'daily.mean1', logistic.yv = logistic.yv, xy.matrix = xy.matrix, wt = wt, wt.control = wt.control, setform = setform))
is.error(h(LADDT, family = family, yv = 'daily.mean2', logistic.yv = logistic.yv, xy.matrix = xy.matrix, wt = wt, wt.control = wt.control, setform = setform))
is.error(h(LADDT, family = family, yv = 'daily.mean3', logistic.yv = logistic.yv, xy.matrix = xy.matrix, wt = wt, wt.control = wt.control, setform = setform))
is.error(h(LADDT, family = family, yv = 'mixed1', logistic.yv = logistic.yv, xy.matrix = xy.matrix, wt = wt, wt.control = wt.control, setform = setform))
is.error(h(LADDT, family = family, yv = 'mixed2', logistic.yv = logistic.yv, xy.matrix = xy.matrix, wt = wt, wt.control = wt.control, setform = setform))
is.error(h(LADDT, family = family, yv = 'mixed3', logistic.yv = logistic.yv, xy.matrix = xy.matrix, wt = wt, wt.control = wt.control, setform = setform))
