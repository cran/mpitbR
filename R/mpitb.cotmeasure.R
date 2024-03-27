mpitb.cotmeasure <- function(change.measure = c("abs","rel"), ...){
  change.measure <- match.arg(change.measure)
  dots <- list(...)
  if(length(dots) == 0) dots <- NULL
  selected.measure <- switch(change.measure,
                             abs = do.call("mpitb.abs", dots),
                             rel = do.call("mpitb.rel", dots))
  selected.measure
}


