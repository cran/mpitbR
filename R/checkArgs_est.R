.checkArgs_est <- function(set, klist = NULL, weights = "equal",
                            measures = c("M0","H","A"),
                            indmeasures = c("hd","hdk","actb","pctb"), indklist = NULL,
                            over = NULL, ...,
                            cotyear = NULL, tvar = NULL,
                            cotmeasures = c("M0","H","A","hd","hdk"), ann = FALSE,
                            cotklist = NULL, cotoptions = "total", noraw = FALSE,
                            nooverall = FALSE, level = 0.95,
                            multicore = getOption("mpitb.multicore"), verbose = TRUE){

# Check arguments  --------------------------------------------------------

  ### `klist` argument  ####
  ## check if `klist` is not NULL
  stopifnot("Poverty cutoffs arguments (`klist`) not found"= (!is.null(klist)))
  ## check if `klist` is numeric
  stopifnot("`klist` should be a `numeric`" = is.numeric(klist))
  ## check if `klist` is between 1 and 100
  stopifnot("`klist` out of range. Values greater than 100 found" = klist <= 100)
  stopifnot("`klist` out of range. Values lower than 1 found" = klist >= 1)

  ### `weights` argument  ####
  if(is.numeric(weights)){
    ## check if `weights` sum up to 1
    stopifnot("`weights` must sum up to 1" = sum(weights) == 1)
    ## check if `weights` has the same length as `indicators`
    # In this case, indicators must be coerced to a vector
    indicators <- set$indicators
    indicators <- unlist(indicators)
    stopifnot("`weights` and indicators do no have the same length" = length(weights) == length(indicators))
  } else {
    stopifnot("`weights` argument is not a character or a numeric vector" = is.character(weights))
    stopifnot("`weights` argument is not a numeric vector or a character == 'equal' " = grepl("\\<equal\\>",weights))
  }

  ### `measures` argument ####
  stopifnot("`measures` must be a character vector" = (is.character(measures)| is.null(measures)))
  if(is.character(measures)){
    stopifnot("Incorrect `measures` specification" = all(measures %in% c("M0","H","A")))
    nomeasures <- FALSE
  } else if (is.null(measures)) {
    nomeasures <- TRUE
  }

  ### `indmeasures` argument ####
  stopifnot("`indmeasures` must be a character vector" = (is.character(indmeasures) | is.null(indmeasures)))
  if(is.character(indmeasures)){
    stopifnot("Incorrect `indmeasures` specification" = all(indmeasures %in% c("hd","hdk","actb","pctb")))
    if( ("actb" %in% indmeasures) & !("hdk" %in% indmeasures) ){stop("'actb' requires 'hdk' in `indmeasures`")}
    if( (("pctb" %in% indmeasures) & !("hdk" %in% indmeasures)) | (("pctb" %in% indmeasures) & !("M0" %in% measures)) ){stop("'pctb' requires both 'hdk' in `indmeasures` and 'M0' in `measures`")}
    noindmeasures <- FALSE
  } else if (is.null(indmeasures)){
    noindmeasures <- TRUE
  }

  ### `indklist` argument ####
  ## check if `indklist` is not NULL, then it have to suffice:
  if (!is.null(indklist)) {
    ## check if `cotklist` is numeric
    stopifnot("`indklist` should be a `numeric`" = is.numeric(indklist))
    ## check if `cotklist` is between 1 and 100
    stopifnot("`indklist` out of range. Values greater than 100 found" = indklist <= 100)
    stopifnot("`cotklist` out of range. Values lower than 1 found" = indklist >= 1)
  }

  ### `over` argument ####
  if (!is.null(over)) {
    ## check if `over` is `character`
    stopifnot("`over` should be a `character`" = is.character(over))
    ## check if `over` are in colnames
    stopifnot("At least one subgroup not found in `data`" = over %in% colnames(set$data))
    # the total observations in the data are interpreted as a subgroup.
    # if `nooverall` is FALSE, include in over. Otherwise, excluded and only compute measure over subgroups
    # check if logical first
    stopifnot("`nooverall` argument must be a logical value" = is.logical(nooverall))
    if(!nooverall){over <- c("nat", over)}
    set$data[,"nat"] <- "nat"
  } else {
    over <- c("nat")
    set$data[,"nat"] <- "nat"
    }

  ### `level` argument
  ## check if `level` is numeric
  stopifnot("`level` should be `numeric`" = is.numeric(level))
  ## check if `level` is between 0 and 1
  stopifnot("`level` argument out of bounds" = level > 0 & level < 1)
  if (level < 0.90) stop("`level` is below 0.90. Check `level` argument or confidence intervals will be estimated with a level lower than 90%!")

  ### `multicore` argument
  ## check if parallel processing
  if(is.null(multicore)){multicore <- FALSE}
  if(multicore && !requireNamespace("parallel",quietly=TRUE)) {
    multicore <- FALSE
  }

  ### `verbose` argument
  ## check if verbose is logical
  stopifnot("`verbose` should be a logical" = is.logical(verbose))

# Changes over time arguments ---------------------------------------------

  ### `tvar` argument ####
  if (!is.null(tvar)) {
    ## check if `tvar` is `character`
    stopifnot("`tvar` should be a `character`" = is.character(tvar))
    ## check if `tvar` is of length 1
    stopifnot("`tvar` should be one element (the column name of the data that contains information about the year)" = length(tvar) == 1)
    ## check if `tvar` is in `data` colnames()
    stopifnot("`tvar` not found in `data`" = tvar %in% colnames(set$data))
    ## check if `tvar` is equal to `cotyear`
    stopifnot("`tvar` is specified as the same column as `cotyear`" = tvar != cotyear)
    ## check if `tvar` has numeric arguments
    tvars <- unique(set$data$variables[,tvar])
    stopifnot("years of `tvar` column are not numeric" = is.numeric(tvars))
    # create a logical variable `cot`  if `tvar` is not null
    cot <- TRUE
  } else {cot <- FALSE}

  ### `cotyear` argument
  if (!is.null(cotyear)) {
    ## check if `year` is `character`
    stopifnot("`cotyear` should be a `character`" = is.character(cotyear))
    ## check if `cotyear` is of length 1
    stopifnot("`cotyear` should be one element (the column of name of the data that contains information about the year)" = length(cotyear) == 1)
    ## check if `year` is in `data` colnames()
    stopifnot("`cotyear` not found in `data`" = cotyear %in% colnames(set$data))
    ## check if `cotyear` has numeric arguments
    years <- unique(set$data$variables[,cotyear])
    tvars <- unique(set$data$variables[,tvar])
    stopifnot("years of `cotyear` column are not numeric" = is.numeric(years))
    ann <- TRUE
    stopifnot("Elements of `tvar` and `cotyear` have different length" = length(years) == length(tvars))
  }
    # if the years are missing, annualized measures cannot be calculated
  if (is.null(cotyear) & isTRUE(ann)){
    # if `cotyear` is null `ann` cannot be TRUE
    ann <- FALSE
    warning("Years for changes over time measures (`cotyear`) are not specified but `ann` is TRUE.
            Hence, `ann` is coerced to FALSE and non-annualized measures are only calculated.")
  }

  ### `cotmeasures` argument ####
  stopifnot("`cotmeasures` must be a character vector" = is.character(cotmeasures))
  stopifnot("Incorrect `cotmeasures` specification" = all(cotmeasures %in% c("M0","H","A","hd","hdk")))

  ### `cotklist` argument ####
  ## check if `cotklist` is not NULL, then it have to suffice:
  if (!is.null(cotklist)) {
    ## check if `cotklist` is numeric
    stopifnot("`cotklist` should be a `numeric`" = is.numeric(cotklist))
    ## check if `cotklist` is between 1 and 100
    stopifnot("`cotklist` out of range. Values greater than 100 found" = cotklist <= 100)
    stopifnot("`cotklist` out of range. Values lower than 1 found" = cotklist >= 1)
  }

  ### `cotoptions` argument ####
  ## if `cotoptions` is null, total changes over time is estimated
  ## "insequence" estimates year-to-year changes
  if(cotoptions != "total"){
    if ( cotoptions != "insequence" ) {cotoptions <- "total"}
    }

  ### `noraw` argument ####
  ## if it is not logical, instead of stopping, coerce to FALSE
  if(!is.logical(noraw)){noraw <- FALSE}


  return(list(
    set=set, klist = klist, weights = weights,
    measures = measures,
    indmeasures = indmeasures, indklist = indklist,
    over = over,
    cotyear = cotyear, tvar = tvar,
    cotmeasures = cotmeasures, ann = ann,
    cotklist = cotklist, cotoptions = cotoptions, noraw = noraw,
    nooverall = nooverall, level = level, multicore = multicore, verbose = verbose,
    cot = cot, nomeasures = nomeasures, noindmeasures = noindmeasures)
    )
}
