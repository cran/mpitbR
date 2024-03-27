#' Extract the coefficients from the estimated cross-sectional measures
#'
#' @param object a "lframe"-class object
#' @param ... other arguments.
#'
#' @return Coefficients extracted from the model \code{lframe} object.
#'
#' @export
#'
#' @details
#' The \code{coef} method for "lframe"-class objects find the point estimates from the
#' different AF measures. This method work for only one measure \code{c("M0","H","A","hd","hdk")} (Note that contribution measure
#' do no have confidence intervals). Then, user should subset the
#' data frame with the estimates by the chosen measure (including other preferred categories, i.e.,
#' poverty cut-off, subgroup, etc.)
#'
#' @example man/examples/example-mpitb.est.R
#'
#' @seealso \code{confint}, and \code{summary} methods, and \code{mpitb.est} function.
#'
#' @author Ignacio Girela

coef.lframe <- function(object, ...){

  stopifnot("`coef` method is only available for one measure" = length(unique(object$measure)) == 1)
  x <- object

  # Check if indicator is in data.frame
  x <- x[,c("subg","loa","indicator","k","b")]
  if(any(is.na(x$indicator))){x$indicator <- NULL}

  # Printing format
  format.perc<-function (x, digits) {
    format(x, trim = TRUE,
           scientific = FALSE, digits = digits)
  }

  x$b <- format.perc(x$b, 3)

  # Tidy up colnames
  colnames(x)[colnames(x) %in% c("subg","loa","k","b")] <- c("Subgroup","Level of analysis","Cut-off","Coefficient")
  colnames(x)[colnames(x) %in% c("indicator")] <- c("Indicator")
  rownames(x) <- NULL
  return(x)
}


#' Extract the coefficients from the estimated changes over time measures
#'
#' @param object a "cotframe"-class object
#' @param ... other arguments.
#'
#' @return Coefficients extracted from the model \code{cotframe} object.
#'
#' @export
#'
#' @details
#' The \code{coef} method for "cotframe"-class objects find the point estimates
#' from the changes over time data frame. This method work for
#' only one measure \code{c("M0","H","A","hd","hdk")}. Then, user should subset the
#' data frame with the estimates by the chosen measure (including other preferred categories, i.e.,
#' poverty cut-off, subgroup, etc.)
#'
#' @example man/examples/example-mpitb.est.R
#'
#' @seealso \code{confint}, and \code{summary} methods, and \code{mpitb.est} function.
#'
#' @author Ignacio Girela

coef.cotframe <- function(object, ...){

  stopifnot("`coef` method is only available for one measure" = length(unique(object$measure)) == 1)
  x <- object

  # Check if indicator is in data.frame
  x <- x[,c("subg","loa","indicator","k","ctype","b")]

  # Printing format
  format.perc<-function (x, digits) {
    format(x, trim = TRUE,
           scientific = FALSE, digits = digits)
  }

  x$b <- format.perc(x$b, 3)

  # Tidy up colnames
  colnames(x)[colnames(x) %in% c("subg","loa","k","b")] <- c("Subgroup","Level of analysis","Cut-off","Coefficient")
  colnames(x)[colnames(x) %in% c("indicator")] <- c("Indicator")
  rownames(x) <- NULL
  return(x)
}
