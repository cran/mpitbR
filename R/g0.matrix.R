g0.matrix <- function(data, indicators) {

  g0 <- as.matrix(data$variables[, indicators])

  return(g0)
}
