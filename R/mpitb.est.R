#' Estimate multidimensional poverty indices based on the Alkire-Foster method
#'
#' @description
#' Estimate multidimensional poverty indices (MPI) based on the Alkire-Foster (AF) method including
#' disaggregated cross-sectional and changes over time estimates as well as quantities such as standard
#' errors and confidences intervals (accounting for the household survey design).
#'
#'
#' @param set a "mpitb_set"-class object in which data, indicators, names and description have been specified.
#' @param klist a numeric vector representing the poverty cut-offs for calculating the MPI. Should be values between 1 and 100.
#' @param weights either a character value or a numeric vector. If "equal", it automatically calculated equal nested weights.
#' @param measures a character vector with the MPI and partial measures. Default include all the measures \code{c("M0","A","H")}. For more information, see Details section below.
#' @param indmeasures a character vector with the indicator-specific measures. Default include all the measures \code{c("hd", "hdk", "actb", "pctb")}. For more information, see Details section below.
#' @param indklist a numeric vector representing the poverty cut-offs for calculating indicator-specific measures. Should be values between 1 and 100. If \code{NULL}, it will be equal to \code{klist}.
#' @param over a character vector with columns names of the population subgroups in data.
#' @param ... other arguments
#' @param tvar a character value containing the column name of the time ID variable in the data.  This argument determines if changes over time are calculated.
#' @param cotyear a character value containing the column name of the years variable in the data. This argument is required if annualized changes over time measure are desired.
#' @param cotmeasures a character vector with the changes over time measures. Default include all the measures \code{c("M0","A","H","hd", "hdk")}. For more information, see Details section below.
#' @param ann logical. If \code{TRUE}, annualized changes over time measure are estimated. If \code{FALSE}, only  non-annualized changes over time are calculated. Default value is \code{FALSE}.
#' @param cotklist a numeric vector representing the poverty cut-offs for calculating changes over time measures. Should be values between 1 and 100. If \code{NULL}, it will be equal to \code{klist}.
#' @param cotoptions a character vector. If "total", estimates change over the total period of observation, i.e. from the first year of observation to the last year of observation. If "insequence", then  estimates all consecutive (i.e. year-to-year) changes. The default is "total"
#' @param noraw logical. If \code{TRUE}, non-annualized changes over time measure are not estimated. Default is \code{FALSE}.
#' @param nooverall logical. If \code{TRUE}, estimations over all the observations are omitted, e.g., national level calculations, and only measure for the specified subgroups are estimated. Default is \code{FALSE}.
#' @param level numeric value with the desired confidence level for the confidence interval calculations in decimal format. Default value is 0.95.
#' @param multicore logical. Use \code{multicore} package for parallel estimation by measure and poverty cut-off over multiple processors? It uses forking approach. See Details below.
#' @param verbose logical. If TRUE, print information messages to the console so the user can control the dimension and indicators weights and which measures are being estimated in the process. If FALSE, these messages are ommited.
#'
#' @return An object with S3 class "mpitb_est" containing two data frames with the estimates of the cross-sectional measures ("lframe"-class) and changes over time ("cotframe"-class).
#'
#' @export
#'
#' @details
#'
#' This functions is a S3 method for "mpitb_set" class. Hence, the project
#' has to be correctly specified with \code{mpitb.set()} function previously.
#'
#' The vector of poverty cut-offs (\eqn{k}) in percentage point, i.e., numbers
#' between 1 and 100. Although the deprivation score (\eqn{c_i = \sum_{i=1}^n w_j g_{ij}^0}) is a real-valued
#' function, given the weights, it will assume a limited number of values. The same
#' occurs with the censored deprivation score. Therefore, despite accepting infinite
#' number of values, results may not vary with close values of \eqn{k}. For this reason,
#' it is recommended to use a very limited number of poverty cut-offs for the analysis.
#'
#' If nothing is passed to \code{weights} argument, equal nested weights are calculated
#' by dimension and indicator. In this case, it is preferred to pass indicators as a list
#' in \code{mpitb.set()}. If the user wants to pass another weighting
#' scheme, she should first pass the indicators as a character vector in \code{mpitb.set()}
#' and then pass a numeric vector in \code{weights} such that the elements of this
#' vector match with the vector of indicators and all the weights sum up to 1.
#'
#' To specify the population subgroups (e.g., living area, sex, etc.) and estimate the
#' disaggregated measures by each level of the subgroup, the user should pass the column
#' names of the population subgroups in the data using \code{over} argument. If \code{over}
#' is \code{NULL}, the measure are estimate using all the observations (e.g., national-level).
#' If population subgroups are specified and \code{nooverall} is set to \code{TRUE},
#'  aggregate (or national-level) estimates will not be produced.
#'
#' Details on the AF measures estimation:
#'
#' Available measures include the Adjusted Headcount Ratio (\eqn{M_0}), the
#' Incidence (\eqn{H}) and the Intensity of poverty (\eqn{A}), as well as other
#' indicator-specific measures
#' such as the uncensored headcount ratio (\eqn{h_j}), the censored headcount ratio (\eqn{h_j(k)})
#' and the absolute and percentage contribution.
#'
#' The three first partial measures are pass in \code{measures} argument. By default,
#' \code{mpitb.est} calculates every measure \code{c("M0","H","A")}. The poverty
#' cut-off (\eqn{k}) for these measures estimation is specified in \code{klist} argument.
#'
#' The indicator-specific measure are passed in \code{indmeasures} argument. By default,
#' \code{mpitb.est} calculates every measure \code{c("hd","hdk","actb","pctb")}. The poverty
#' cut-off (\eqn{k}) for these measures estimation is specified in \code{indklist} argument.
#' If \code{indklist} is \code{NULL}, poverty cut-offs in \code{klist} is used.
#' The absolute contribution \code{c("actb")} cannot be estimated without also
#' passing the censored headcount
#' ratios of each indicator  \code{c("hdk")} and the percentage contribution cannot be
#' calculated without \code{c("hdk")} and \code{c("M0")} passed in \code{measures} argument.
#'
#' If any of these arguments is \code{NULL}, \code{mpitb.est()} skips these
#' measures. So it is useful for avoid calculating unnecessary estimations. For example,
#' if \code{measures = c("H","A")} and \code{indmeasures = NULL}, only the Incidence and
#' the Intensity will be estimated.
#'
#' Details on changes over time measures:
#'
#' The user can decide which AF measure changes over time she want to study. This is
#' set in \code{cotmeasures}. By default it calculates all the measure, except contributions, i.e.,
#' \code{cotmeasure = c("M0","A","H","hd","hdk")}. It would be important to check this argument in order to save time.
#' The poverty
#' cut-off (\eqn{k}) for these measures estimation is specified in \code{cotklist} argument.
#' If \code{cotklist} is \code{NULL}, poverty cut-offs in \code{klist} is used.
#' The standard errors of the changes over time measures is estimated using Delta method.
#'
#' For calculating any point estimate for each time period and any change over time
#' measure, \code{tvar} should not be \code{NULL}. This argument should be a character with the
#' column name that references the time period \eqn{t = 1, \ldots,T}.
#'
#' Changes over time measure can also be annualized. For such measure, information about
#' the years is needed. \code{cotyear}  should be a character with the column name
#' that have information about the years. Decimal digits are permitted. Argument \code{ann} is a logical
#' value. If \code{TRUE}, annualized measures are calculated. If \code{cotyear} is passed, \code{ann}
#' is automatically set to \code{TRUE}. If the former is not \code{NULL} and
#' \code{ann} is \code{FALSE}, only non-annualized
#' measures are estimated. If only annualized measure are
#' under study, the user can switch \code{noraw} to \code{TRUE} to avoid estimating non-annualized changes.
#'
#' Finally, if there are more than two years survey rounds, the user can decide if estimate the
#' change over the total period of observation, i.e. from the first year of observation to the
#' last year of observation or year-to-year changes. To do the former, \code{cotoptions = "total"}
#' whereas for the latter case, \code{cotoptions = "insequence"}. By default, \code{cotoptions = "total"}
#' to avoid unnecessary estimations.
#'
#' Some details on other arguments and estimations:
#'
#' The package includes the possibility to do parallel calculations over all the measures and poverty cut-offs.
#' If \code{multicore} is \code{TRUE}, the package proceeds with parallel estimations. Caveat: this package uses
#' Forking method for parallelization which is only available on Unix-like systems (including Linux), i.e., Windows
#' users cannot benefit from parallelization.
#'
#' For every measure the standard errors and confidence intervals are estimated. The former are estimated taking into
#' account the survey structure whereas the latter are estimated considering measures as proportions using \code{svyciprop()}
#' function from "survey" R package (it uses the "logit" method which consists of fitting a logistic regression model
#' and computes a Wald-type interval on the log-odds scale, which is then transformed to the probability scale).
#'
#'
#' @rdname mpitb.est
#'
#' @references \emph{Alkire, S., Foster, J. E., Seth, S., Santos, M. E., Roche, J., & Ballon, P. (2015). Multidimensional poverty measurement and analysis. Oxford University Press.}
#'
#'              \emph{Alkire, S., Roche, J. M., & Vaz, A. (2017). Changes over time in multidimensional poverty: Methodology and results for 34 countries. World Development, 94, 232-249}. \doi{10.1016/j.worlddev.2017.01.011}
#'
#'              \emph{Suppa, N. (2023). mpitb: A toolbox for multidimensional poverty indices. The Stata Journal, 23(3), 625-657}. \doi{10.1177/1536867X231195286}
#'
#' @example man/examples/example-mpitb.est.R
#'
#' @seealso \code{coef}, \code{confint}, and \code{summary} methods, and \code{mpitb.set} function.
#'
#' @author Ignacio Girela

mpitb.est <- function(set, ...) {UseMethod("mpitb.est", set)}

#' @rdname mpitb.est
#' @export

mpitb.est.mpitb_set <- function(set, klist = NULL, weights = "equal",
                        measures = c("M0","H","A"),
                        indmeasures = c("hd","hdk","actb","pctb"), indklist = NULL,
                        over = NULL, ...,
                        cotyear = NULL, tvar = NULL,
                        cotmeasures = c("M0","H","A","hd","hdk"), ann = FALSE,
                        cotklist = NULL, cotoptions = "total", noraw = FALSE,
                        nooverall = FALSE, level = 0.95,
                        multicore = getOption("mpitb.multicore"), verbose = TRUE){

# Catch call --------------------------------------------------------------

  # Get the formal arguments with default values
  formalArgs <- formals(mpitb.est)
  # Get the arguments passed to the function
  myArgs <- as.list(match.call(expand.dots = FALSE))[-1]
  # Add any missing arguments from formals
  for (v in names(formalArgs)) {
    if (!(v %in% names(myArgs))) {
      myArgs <- append(myArgs, formalArgs[v])
    }
  }
  myArgs$... <- NULL


  this.call <- match.call(expand.dots = FALSE)
  # Print this call so that the user can check if arguments are correctly assigned
  #print(this.call)

# 1) Check arguments ------------------------------------------------------

  Args <- do.call(".checkArgs_est", myArgs)
  ## Get the checked arguments
  set = Args$set #
  klist = Args$klist #

  weights = Args$weights #
  measures = Args$measures #

  indmeasures = Args$indmeasures #
  indklist = Args$indklist

  over = Args$over

  cotyear = Args$cotyear
  tvar = Args$tvar

  cotmeasures = Args$cotmeasures
  ann = Args$ann

  cotklist = Args$cotklist
  cotoptions = Args$cotoptions
  noraw = Args$noraw

  nooverall = Args$nooverall
  level = Args$level
  multicore = Args$multicore
  verbose = Args$verbose

  cot = Args$cot
  nomeasures = Args$nomeasures
  noindmeasure = Args$noindmeasures



# 2) Some arguments treatment ---------------------------------------------

  # In this part, some argument of the mpitb.est() functions are treated. More
  # specifically, weights and indicators. If nothing is specified for the
  # weighting scheme, by default, nested equal eights are calculated. All these
  # treatments are printed, so the user can control if everything is ok.
  # Finally, the deprivations score is calculated and added to the variables of
  # the survey design.

  ### 2.1) WEIGHTS, INDICATORS AND DIMENSIONS ####
  {# `indicators` is a list and it is in `set`
  indicators <- set$indicators
  # if `weights` == "equal", create nested weights
  if ((is.character(weights) && grepl("\\<equal\\>",weights))) {
    weights.scheme <- "equal"
    # equal weights for each dimension
    weight_dim <- 1/length(indicators)
    weights_dim <- rep(weight_dim, length(indicators))
    # equal weights for each indicator
    weights_ind <- sapply(indicators, function(x) rep(1/length(x) * weight_dim,length(x)))
    # weights and indicators as ordered vectors such that each indicator match its corresponding weights
    weights <- unlist(weights_ind, use.names = F)
    dimensions <- names(indicators)
    indicatorsList <- indicators
    indicators <- unlist(indicators, use.names = F)
    names(weights) <- indicators
  } else { # if `weights` is numeric and sum up to 1 (preferred weights specification)
    # indicators as a vector
    weights.scheme <- "pref.spec"
    dimensions <- NULL
    indicators <- unlist(indicators, use.names = F)
    names(weights) <- indicators
  }
  if (verbose){
    cat("\t\t   ****** SPECIFICATION ******\n")
    cat("Call:\n")
    print(this.call)
    cat("Name: ",attr(set,"name"),"\n")
    cat("Weighting scheme: ", weights.scheme,"\n")
    cat("Description: ",attr(set,"desc"),"\n")
    cat("___________________\n")
    if(!is.null(dimensions)){
      catDimensions <- data.frame(dimensions,weights_dim,
                              row.names = paste(paste("Dimension ", 1:length(dimensions),": ",sep="")))
      catDimensions[,3]<-sapply(catDimensions[,1], function(x) paste0("(",toString(indicatorsList[[x]]),")"))
      colnames(catDimensions) <- NULL
      print(catDimensions, digits = 3)
      cat("___________________\n")
      }
    catIndicators <- data.frame(indicators,weights,
                              row.names = paste(paste("Indicator ", 1:length(indicators),": ",sep="")))
    colnames(catIndicators) <- NULL
    print(catIndicators, digits = 3)
    cat("\n")
  }
  # RESULTS
  # indicators <- character: Names of the indicators
  # weights <- numeric: Relative weights of the indicators in the same order as the vector
# End of '2.1) WEIGHTS, INDICATORS AND DIMENSIONS'
  }



  ### 2.2) CALCULATE THE DEPRIVATION SCORE ####
  {# `data` is in`set`
  data <- set$data
  g0 <- g0.matrix(data, indicators)
  ## G0_w <- matrix: Weighted Deprivation Matrix
  g0_w <- weighted.g0.matrix(g0, weights)
  # calculate the deprivations score
  deprivations.score <- c_score(g0_w)
  ### Add deprivations score to survey variables data frame (see utils.R)
  data <- update_svy(data, c.score = deprivations.score)
  # RESULTS:
  # data <- survey.design: Survey design with the deprivations score
# End of '2.2) CALCULATE THE DEPRIVATION SCORE'
  }



  if(verbose){cat("\t\t   ****** ESTIMATION ******\n")}




# 3) AF measures ----------------------------------------------------------

  # In this section, we estimate the MPI and the two main partial AF measures
  # (A and H). Confidence intervals are calculated using svyciprop() function
  # from the survey R package. This function does not provide the covariance
  # matrix of the estimates when we calculate measures by year. This prevents
  # from using delta method when calculating the standard errors of functions
  # of the estimates over time (changes over time measures).

 lframe <- NULL

  if(isFALSE(nomeasures)){
    if(verbose){
      cat("___________________\n")
      cat("Partial AF measures: '",measures,"' under estimation... ")
    }
    # arguments to vectorize over
    VecArgs <- expand.grid(list(k = klist, measure = measures), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    # MoreArgs (a list of other arguments to the mpitb measures FUN)
    OtherArgs <- list(data = data, over = over,
                     cot = cot, tvar = tvar,
                     level = level)


    # estimate each measure by poverty cutoff k
    # include parallel processing
    AFmeasuresList <- (if (multicore) parallel::mcmapply else mapply)(mpitb.measure, prop = TRUE, k = VecArgs$k, measure = VecArgs$measure, MoreArgs = OtherArgs, SIMPLIFY = FALSE, USE.NAMES = FALSE)



    lframe <- do.call("rbind",AFmeasuresList)
    class(lframe) <- c("lframe","data.frame")
    attr(lframe,"level") <- level
    if(verbose){cat("DONE\n\n")}
    }

# 4) Indicators-related measures ------------------------------------------

  # In this section, we estimate the indicator-specific measures such as
  # hd, hdk, actb and pctb. Confidence intervals are calculated using
  # svyciprop() function from the survey R package (except for the contribution
  # measures). This function does not provide the covariance matrix of the
  # estimates when we calculate measures by year. This prevents from using delta
  # method when calculating the standard errors of functions of the estimates
  # over time (changes over time measures).

    if (isFALSE(noindmeasure)){
      if(verbose){
        cat("___________________\n")
        cat("Indicator-specific measures: '", indmeasures,"' under estimation... ")
      }
      # if indklist is NULL, use the same klist as in cross-sectional AF measures
      if(is.null(indklist)){indklist <- klist}

      # separate measures
      contmeasures <- c("actb","pctb")[c("actb","pctb") %in% indmeasures]
      indmeasures <- c("hd","hdk")[c("hd","hdk") %in% indmeasures]

      # arguments to vectorize over
      VecArgs <- expand.grid(list(k = klist, indmeasure = indmeasures, indicator = indicators), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
      VecArgs$k[VecArgs$indmeasure=="hd"] <- NA
      VecArgs <- VecArgs[!duplicated(VecArgs),]
      # MoreArgs (a list of other arguments to the mpitb measures FUN)
      OtherArgs <- list(data = data, over = over,
                      cot = cot, tvar = tvar,
                      level = level)

      # estimate each measure by poverty cutoff k
      # include parallel processing
      indmeasuresList <- (if (multicore) parallel::mcmapply else mapply)(mpitb.indmeasure, prop = TRUE, k = VecArgs$k, indmeasure = VecArgs$indmeasure, indicator = VecArgs$indicator, MoreArgs = OtherArgs, SIMPLIFY = FALSE, USE.NAMES = FALSE)

      # convert to dataframe
      measuresind <- do.call("rbind", indmeasuresList)


      # estimate contributions of each indicator
        # actb
      if("actb"%in%contmeasures){measuresind <- mpitb.actb(measuresind, weights, indicators)}
        # pctb
      if("pctb"%in%contmeasures){
        pctb <- mpitb.pctb(measuresind,lframe)
        measuresind <- rbind(measuresind, pctb[pctb$measure %in% "pctb", ])
      }



      #check if lframe already exists (if nomeasures is FALSE) before binding the indmeasures estimations
      if(!is.null(lframe)){
         lframe <- rbind(lframe,measuresind)
         class(lframe) <- c("lframe","data.frame")
         attr(lframe,"level") <- level
       }else{
           lframe <- measuresind
         }
      if(verbose){cat("DONE\n\n")}
    }






# 5) Changes over time measures -------------------------------------------

  # In this part of the code, we estimate the changes-over-time measures such as
  # (annualized) absolute and relative changes. The measures are estimated using
  # svymean() function from the survey R package. This function provides the
  # covariance matrix of the estimates when we calculate measures by year.
  # This useful because standard errors are estimated using delta method.
  # Confidence intervals are estimated assuming normal distribution of the
  # changes-over-time measures.


  cotframe <- NULL

  if(isTRUE(cot)){
    if(verbose){
      cat("___________________\n")
      cat("Estimate changes over time over '", cotmeasures,"' measures... ")
    }
    # if cotklist is NULL, use the same klist as in cross-sectional AF measures
    if(is.null(cotklist)){cotklist <- klist}
    # some argument check
    # match `tvar` and `cotyear`
    if(isTRUE(ann)){
      # create a vector with the years id numbers with `tvar`
      years.list <- sort(unique(data$variables[,cotyear]))
      names(years.list) <- sort(as.character(unique(data$variable[,tvar])))
      # non-annualized measures finally included?
      if(isTRUE(noraw)){annualized <- c(TRUE)}else{annualized <- c(FALSE,TRUE)}
    } else {
        if(isTRUE(noraw)){warning("`ann` is FALSE but `noraw` is TRUE. No change-over-time measure can be estimated. By coercion, non-annualized measure are estimated")}
        # create a vector with the years id names with `tvar`
        years.list <- sort(as.character(unique(data$variables[,tvar])))
        names(years.list) <- sort(as.character(unique(data$variables[,tvar])))
        # annualized is coerced to FALSE under this setting
        annualized <- c(FALSE)
    }
    # subset data according to the cotoptions ("total" or "insequence")
    if(cotoptions == "total"){
      #data <- subset(data, t == which.min(years.list) | t == which.max(years.list))
      data$variables <- data$variables[data$variables[, tvar]==which.min(years.list)|data$variables[, tvar]==which.max(years.list), ]
    } # if "insequence", keep it and calculate year-to-year changes

    # separate indicators-related measure of the typical AF measures. This is necessary for efficient arguments vectorization below
    indmeasures <- c("hd","hdk")[c("hd","hdk") %in% cotmeasures]
    cotmeasures <- c("M0","A","H")[c("M0","A","H") %in% cotmeasures]

    # MoreArgs (a list of other arguments to the mpitb cotmeasures FUN)
    OtherArgs <- list(data = data, over = over,
                      tvar = tvar, cotyear = cotyear,
                      level = level)

    # estimate measures by poverty cut-off k
      # before set arguments to vectorize
      # over the AF measures
    if(length(cotmeasures)>0){
      VecArgs <- expand.grid(list(k = cotklist, measure = cotmeasures), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

      AFmeasuresList <- (if (multicore) parallel::mcmapply else mapply)(mpitb.measure, prop = FALSE, k = VecArgs$k, measure = VecArgs$measure, MoreArgs = OtherArgs, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    } else {AFmeasuresList <- list()}
      # now over the indicators-related measures
    if(length(indmeasures)>0){
      VecArgs <- expand.grid(list(k = cotklist, indmeasure = indmeasures, indicator = indicators), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
      VecArgs$k[VecArgs$indmeasure=="hd"] <- NA
      VecArgs <- VecArgs[!duplicated(VecArgs),]
      indmeasuresList <- (if (multicore) parallel::mcmapply else mapply)(mpitb.indmeasure, prop = FALSE, k = VecArgs$k, indmeasure = VecArgs$indmeasure, indicator = VecArgs$indicator, MoreArgs = OtherArgs, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    } else {indmeasuresList <- list()}

      # merge everything in one list with all the elements flattening all sublists with purrr::flatten
    measuresList <- purrr::flatten(do.call("c",list(AFmeasuresList,indmeasuresList)))

    # Calculate (annualized) absolute and relative changes

    # arguments to vectorize over
    VecArgs <- expand.grid(list(change.measure = c("abs","rel"), annualized = annualized), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

    # MoreArgs (a list of other arguments to the mpitb cotmeasures FUN)
    OtherArgs <- list(years.list = years.list, object = measuresList,
                      level = level, degfs = survey::degf(data))
    cotmeasuresList <- (if (multicore) parallel::mcmapply else mapply)(mpitb.cotmeasure, change.measure = VecArgs$change.measure, annualized = VecArgs$annualized, MoreArgs = OtherArgs, SIMPLIFY = FALSE, USE.NAMES = FALSE)

    flattened <- purrr::flatten(cotmeasuresList)
    cotframe <- do.call("rbind",flattened)
    class(cotframe) <- c("cotframe", "data.frame")
    attr(cotframe,"level") <- level
    if(verbose){cat("DONE\n\n")}
  }


  if(verbose){
    cat("\t\t   ****** RESULTS ******\n")
    cat("___________________\n")
    cat("Parameters\n")
    if(cot)cat("Number of time periods: ", length(years.list),"\n")
    cat("Subgroups: ",length(over),"\n")
    cat("Poverty cut-offs (k): ", length(klist),"\n\n")

    cat("*Notes: \n\t Confidence level:", 100*level,
      "%\n\t Parallel estimations: ", multicore,"\n")
  }




# 6) Prepare output -------------------------------------------------------

  outputList <- list(
    lframe = lframe,
    cotframe = cotframe)

  attr(outputList,"name") <- attr(set,"name")

  attr(outputList,"desc") <- attr(set,"desc")

  class(outputList) <- "mpitb_est"

  # End of the code :)
  return(outputList)
}








