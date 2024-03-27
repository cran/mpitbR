#' Extract the confidence intervals from the the estimated cross-sectional measures
#'
#' @param object a "lframe"-class object
#' @param parm "coefficient". Confidence intervals are only available for AF measure point estimates.
#' @param level the confidence level required.
#' @param ... additional argument(s) for methods.
#'
#' @return Confidence intervals extracted from the model \code{lframe} object.
#'
#' @export
#'
#' @details
#' The \code{confint} method for "lframe"-class objects find the confidence
#' intervals from the different AF measures estimates data frame. This method work for
#' only one measure \code{c("M0","H","A","hd","hdk")} (Note that contribution measure
#' do no have confidence intervals). Then, user should subset the
#' data frame with the estimates by the chosen measure (including other preferred categories, i.e.,
#' poverty cut-off, subgroup, etc.)
#'
#' @example man/examples/example-mpitb.est.R
#'
#' @seealso \code{coef}, and \code{summary} methods, and \code{mpitb.est} function.
#'
#' @author Ignacio Girela

confint.lframe <-  function(object, parm = "coefficient", level = 0.95, ...){
  stopifnot("`confint` method is only available for one measure" = length(unique(object$measure)) == 1)
  if(isFALSE(level == attr(object,"level"))){
    warning(paste("Confidence level is set to", attr(object,"level") ))
  }
  ### Check arguments
  ## parm == "coefficient"
  stopifnot("`confint` method only available for `parm = 'coefficient'`" = parm == "coefficient")
  parm <- match.arg(parm)
  # Select columns
  x <- as.data.frame(object)
  x <- x[, c("subg","loa","indicator","k","ll","ul")]
  if(any(is.na(x$indicator))){x$indicator <- NULL}
  # Printing format
  format.perc<-function (x, digits) {
    format(x, trim = TRUE,
           scientific = FALSE, digits = digits)
  }
  x$ll <- format.perc(x$ll, 3)
  x$ul <- format.perc(x$ul, 3)
  # Tidy up colnames
  colnames(x)[colnames(x) %in% c("subg","loa","k","ll","ul")] <- c("Subgroup","Level of analysis","Cut-off",
                                                                               paste("Lower Bound (",level*100,"%)", sep = ""), paste("Upper Bound (",level*100,"%)", sep = ""))
  colnames(x)[colnames(x) %in% c("indicator")] <- c("Indicator")
  rownames(x) <- NULL
  return(x)
}

#' Extract the confidence intervals from the the estimated changes over time measures
#'
#' @param object a "cotframe"-class object
#' @param parm "coefficient". Confidence intervals are only available for AF measure point estimates.
#' @param level the confidence level required.
#' @param ... additional argument(s) for methods.
#'
#' @return Confidence intervals extracted from the model \code{cotframe} object.
#'
#' @export
#'
#' @details
#' The \code{confint} method for "cotframe"-class objects find the confidence
#' intervals from the changes over time estimates data frame. This method work for
#' only one measure \code{c("M0","H","A","hd","hdk")}. Then, user should subset the
#' data frame with the estimates by the chosen measure (including other preferred categories, i.e.,
#' poverty cut-off, subgroup, etc.)
#'
#' @example man/examples/example-mpitb.est.R
#'
#' @seealso \code{coef}, and \code{summary} methods, and \code{mpitb.est} function.
#'
#' @author Ignacio Girela

confint.cotframe <-  function(object, parm = "coefficient", level = 0.95, ...){
  stopifnot("`confint` method is only available for one measure" = length(unique(object$measure)) == 1)

  if(isFALSE(level == attr(object,"level"))){
    warning(paste("Confidence level is set to", attr(object,"level") ))
  }
  ### Check arguments
  ## parm == "coefficient"
  stopifnot("`confint` method only available for `parm = 'coefficient'`" = parm == "coefficient")
  parm <- match.arg(parm)
  # Select columns
  x <- as.data.frame(object)
  x <- x[, c("subg","loa","indicator","k","ll","ul")]
  # Printing format
  format.perc<-function (x, digits) {
    format(x, trim = TRUE,
           scientific = FALSE, digits = digits)
  }
  x$ll <- format.perc(x$ll, 3)
  x$ul <- format.perc(x$ul, 3)
  # Tidy up colnames
  colnames(x)[colnames(x) %in% c("subg","loa","k","ll","ul")] <- c("Subgroup","Level of analysis","Cut-off",
                                                                               paste("Lower Bound (",level*100,"%)", sep = ""), paste("Upper Bound (",level*100,"%)", sep = ""))
  colnames(x)[colnames(x) %in% c("indicator")] <- c("Indicator")
  rownames(x) <- NULL
  return(x)
}
