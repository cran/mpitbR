
# summary.lframe ----------------------------------------------------------

#' Summary function of the estimates of the cross-sectional AF measures
#'
#' @param object the "lframe"-class object
#' @param ... other arguments
#'
#' @return summary.lframe a list of summary information for the estimated AF measure
#'
#' @export
#'
#' @details
#' The \code{summary} method for "lframe"-class objects with coefficients, standard
#' errors and the corresponding t-values and the p-value for a two sided test,
#' \eqn{H_0: \beta = 0} being \eqn{\beta} any measure, with the 'significance stars'.
#' This method work for only one measure \code{c("M0","H","A","hd","hdk")}.
#' Then, user should subset the data frame to obtain the desired summary.
#'
#' @example man/examples/example-mpitb.est.R
#'
#' @seealso \code{coef}, and \code{confint} methods, and \code{mpitb.est} function.
#'
#' @author Ignacio Girela

summary.lframe <- function(object, ...){

  stopifnot("`summary` method is only available for one measure" = length(unique(object$measure)) == 1)
  x <- object
  k <- unique(x$k)
  measure <- unique(x$measure)


  x <- x[,c("subg","loa","indicator","b","se")]
  if(any(is.na(x$indicator))){x$indicator <- NULL}


  matrices <- lapply(k, function(y){
    mat <- subset(x, k == y)
    # Convert the object into a matrix conveniently for subsequent printing
    rownames_mat <- paste(mat$subg,mat$loa,sep = ".")
    mat$subg <- NULL; mat$loa <- NULL
    if("indicator"%in%colnames(mat)){
      rownames(mat) <- paste(rownames_mat,mat$indicator,sep = ":")
      mat$indicator <- NULL} else {rownames(mat) <- rownames_mat}

    # t-statistic
    mat$t <- mat$b/mat$se
    # p-value
    degfs <- Inf
    mat$p <- 2*stats::pt(-abs(mat$t), degfs, lower.tail = TRUE)
    colnames(mat) <- c("Estimate", "Std.Err", "t-value", "Pr(>|t|)")
    mat <- as.matrix(mat)
    attr(mat,"k") <- y

    mat
  })

  attr(matrices,"measure") <- measure
  class(matrices) <- "summary.lframe"

  return(matrices)
}

#' Print Summary function of summary.lframe object
#'
#' @param x the "summary.lframe"-class object
#' @param digits controls number of digits printed in output.
#' @param signif.stars should significance stars be printed alongside output.
#' @param ... optional arguments
#'
#' @return summarized object with nice format
#'
#' @export

print.summary.lframe <- function(x, digits = max(4, getOption("digits") - 3),
                                        signif.stars = getOption("show.signif.stars"), ...){

  cat("\nMeasure:",attr(x,"measure"),"\n")
  cat("Coefficients:")
  for (k in 1:length(x)){
    cat("\n\tPoverty cut-off:", attr(x[[k]],"k"),"% \n")
    stats::printCoefmat(x[[k]], digits = digits, signif.stars = signif.stars, na.print = "NA", ...)
  }
  invisible(x)
}

# summary.cotframe --------------------------------------------------------

#' Summary function of the estimates of the changes over time measures
#'
#' @param object the "cotframe"-class object
#' @param ... other arguments
#'
#' @return summary.cotframe a list of summary information for the estimated AF measure
#'
#' @export
#'
#' @details
#' The \code{summary} method for "cotframe"-class objects with coefficients, standard
#' errors and the corresponding t-values and the p-value for a two sided test,
#' \eqn{H_0: \beta = 0} being \eqn{\beta} any measure, with the 'significance stars'.
#' This method work for only one measure \code{c("M0","H","A","hd","hdk")} and either
#' relative or absolute measure, either non-annualized and annualized measure and only one
#' poverty cut-off.
#' Then, user should subset the data frame to obtain the desired summary.
#'
#' @example man/examples/example-mpitb.est.R
#'
#' @seealso \code{coef}, and \code{confint} methods, and \code{mpitb.est} function.
#'
#' @author Ignacio Girela

summary.cotframe <- function(object, ...){
  stopifnot("`summary` method is only available for one measure" = length(unique(object$measure)) == 1)
  stopifnot("`summary` method is only available for annualized or non-annualized measures exclusively" = length(unique(object$ann)) == 1)
  stopifnot("`summary` method is only available for absolute or relative measures exclusively" = length(unique(object$ctype)) == 1)
  stopifnot("`summary` method is only available for one poverty cut-off" = length(unique(object$k)) == 1)
  x <- object
  k <- unique(x$k)
  measure <- unique(x$measure)
  ctype <-  unique(x$ctype)
  ann <- unique(x$ann)

  x <- x[,c("subg","loa","indicator","b","se")]
  if(any(is.na(x$indicator))){x$indicator <- NULL}


    # Convert the object into a matrix conveniently for subsequent printing
    rownames_mat <- paste(x$subg,x$loa,sep = ".")
    x$subg <- NULL; x$loa <- NULL
    if("indicator"%in%colnames(x)){
      rownames(x) <- paste(rownames_mat,x$indicator,sep = ":")
      x$indicator <- NULL} else {rownames(x) <- rownames_mat}

    # t-statistic
    x$t <- x$b/x$se
    # p-value
    degfs <- Inf
    x$p <- 2*stats::pt(-abs(x$t), degfs, lower.tail = TRUE)
    colnames(x) <- c("Estimate", "Std.Err", "t-value", "Pr(>|t|)")
    mat <- as.matrix(x)
    attr(mat,"k") <- k

  attr(mat,"measure") <- measure
  attr(mat,"ctype") <- ctype
  attr(mat,"ann") <- ann
  class(mat) <- "summary.cotframe"
  return(mat)
}

#' Print Summary function of summary.cotframe object
#'
#' @param x the "summary.cotframe"-class object
#' @param digits controls number of digits printed in output.
#' @param signif.stars should significance stars be printed alongside output.
#' @param ... optional arguments
#'
#' @return summarized object with nice format
#'
#' @export

print.summary.cotframe <- function(x, digits = max(4, getOption("digits") - 3),
                                 signif.stars = getOption("show.signif.stars"), ...){
  cat("\nMeasure:",attr(x,"measure"),"\n")
  if(attr(x,"ctype") == "abs"){ctype <- "Absolute"}else{ctype <- "Relative"}
  if(attr(x,"ann") == 0){ann <- "Non-annualized "}else{ann <- "Annualized"}
  cat("Coefficients:")
    cat("\n\t",paste(ann,ctype,"change over time measures:"))
    cat("\n\tPoverty cut-off:", attr(x,"k"),"% \n")
    stats::printCoefmat(x, digits = digits, signif.stars = signif.stars, na.print = "NA", ...)

  invisible(x)
}
