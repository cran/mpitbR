mpitb.indmeasure <- function(indmeasure = c("hd","hdk"), prop = TRUE, ...){
  indmeasure <- match.arg(indmeasure)
  dots <- list(...)
  if(length(dots) == 0) dots <- NULL
  if(isTRUE(prop)){
    selected.measure <- switch(indmeasure,
                               hd = do.call("mpitb.hd_svyciprop", dots),
                               hdk = do.call("mpitb.hdk_svyciprop", dots))
  } else {
    selected.measure <- switch(indmeasure,
                               hd = do.call("mpitb.hd_svymean", dots),
                               hdk = do.call("mpitb.hdk_svymean", dots))
  }
  selected.measure
}
