mpitb.actb <- function(X, weights, indicators,...){
  actb <- X[X$measure == "hdk",]
  actb$b <- actb$b*weights[actb$indicator]
  actb$se <- NA
  actb$ll <- NA
  actb$ul <- NA
  actb$measure <- "actb"
  return(rbind(X,actb))
}
