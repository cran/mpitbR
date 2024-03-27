weighted.g0.matrix <- function(X, weights) {

  g0.w <- X %*% diag(weights)

  return(g0.w)
}
