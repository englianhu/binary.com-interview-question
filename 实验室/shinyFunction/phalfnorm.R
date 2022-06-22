phalfnorm <- function (q, theta = sqrt(pi/2), lower.tail = TRUE, log.p = FALSE) {
  sd.norm = theta2sd(theta)
  p = ifelse(q < 0, 0, 2 * pnorm(q, mean = 0, sd = sd.norm) - 1)
  if (lower.tail == FALSE) 
    p = 1 - p
  if (log.p) 
    p = log(p)
  
  return(p)
  }


