mpitb.measure <- function(measure = c("M0","H","A"), prop = TRUE, ...){
  measure <- match.arg(measure)
  dots <- list(...)
  if(length(dots) == 0) dots <- NULL
  if(isTRUE(prop)){
    selected.measure <- switch(measure,
                               M0 = do.call("mpitb.M0_svyciprop", dots),
                               H = do.call("mpitb.H_svyciprop", dots),
                               A = do.call("mpitb.A_svyciprop", dots))
  } else {
    selected.measure <- switch(measure,
                               M0 = do.call("mpitb.M0_svymean", dots),
                               H = do.call("mpitb.H_svymean", dots),
                               A = do.call("mpitb.A_svymean", dots))
  }
  selected.measure
}
