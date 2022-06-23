last <- function (x, length.out = 1, na.rm = FALSE) 
{
  if (na.rm) 
    x <- x[!is.na(x)]
  n <- length(x)
  x[sign(length.out) * (n - abs(length.out) + 1):n]
}