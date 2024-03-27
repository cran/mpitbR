censored_c.score <- function(cvector, cutoff){
  c.k <- sapply(cvector, function(x) ifelse(x >= cutoff,x,0))
  return(c.k)
}
