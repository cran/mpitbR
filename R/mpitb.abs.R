mpitb.abs <- function(object, ...){
  dots <- list(...)
  if(length(dots) == 0) dots <- NULL
  change <- lapply(object, absolute.change, dots)
  change
}
