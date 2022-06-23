## Mn(Fund) to retrieve the Mean column.
has.Mn <- function (x, which = FALSE) {
  colAttr <- attr(x, 'Mn')
  if(!is.null(colAttr)) 
    return(if (which) colAttr else TRUE)
  loc <- grep('Mean', colnames(x), ignore.case = TRUE)
  
  if(!identical(loc, integer(0))) {
    return(if (which) loc else TRUE)
  } else FALSE 
}

Mn <- function(x) {
  if(has.Mn(x)) 
    return(x[, grep("Mean", colnames(x), ignore.case = TRUE)])
  stop("subscript out of bounds: no column name containing \"Mean\"")
}
