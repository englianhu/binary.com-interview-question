Mn <- function (x) {
  if (has.Mn(x)) 
    return(x[, grep('Mean', colnames(x), ignore.case = TRUE)])
  stop('subscript out of bounds: no column name containing "Mean"')
}