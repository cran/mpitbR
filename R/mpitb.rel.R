mpitb.rel <- function(object, ...){
  dots <- list(...)
  if(length(dots) == 0) dots <- NULL
  change <- lapply(object, relative.change, dots)
  change
}
