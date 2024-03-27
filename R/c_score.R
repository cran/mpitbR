c_score <- function(X){
  # x : numeric matrix
  c <- apply(X, MARGIN = 1, FUN = sum)

  return(c)
}
