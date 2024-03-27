relative.change <- function(X, ...){
  dots <- unlist(list(...), recursive = FALSE)
  stopifnot("annualized not logical" = is.logical(dots$annualized))
  # get the measure
  measure <- attr(X, "measure")
  # get the indicator
  indicator <- attr(X, "indicator")
  # get the poverty cutoff
  k <- attr(X,"k")
  # get the subgroup name
  loa <- colnames(X)[1]
  # get the tvar name
  time <-colnames(X)[2]
  # include year() and sub() for a later construction of the data.frame results
  years <- unlist(sapply(X[,time], function(x) paste0("year(",dots$years.list[x],")",sep="")))
  subgs <- unlist(sapply(X[,loa], function(x) paste0("subg(",x,")",sep="")))
  # now modify the rownames with this modifications
  X$id <- paste(subgs,years,sep=".")

  X$year <- unlist(sapply(X[,time], function(x) dots$years.list[x]))

  # get the levels from rownames(X)
  getLevels <- sub('\\..*', '', rownames(X));
  getLevels_id <- sub('\\..*', '', X$id)
  # split the vector by the levels
  splittedbyLevels <- split(rownames(X), getLevels);
  splittedbyLevels_id <- split(X$id, getLevels_id);


  # Define a function that set the expressions for svycontrast.
  # RELATIVE CHANGE
  getExpressions <- function(vector){
    if(dots$annualized){
      dy <- as.character(X[vector[length(vector)],"year"] - X[vector[length(vector)-1],"year"])
      result <- paste0("((`", vector[length(vector)], "` / `", vector[length(vector)-1], "`)^(1/",dy,") - 1 ) * 100")
      if (length(vector) > 2){
        for (i in (length(vector)-2):1) {
          dy <- as.character(X[vector[i+1],"year"] - X[vector[i],"year"])
          result <- c(result, paste0("(`", vector[i+1], "` / `", vector[i], "`)^(1/",dy,") - 1 ) * 100"))
        }
      }
    } else {
      result <- paste0("(`", vector[length(vector)], "` / `", vector[length(vector)-1], "` - 1 ) * 100")
      if (length(vector) > 2){
        for (i in (length(vector)-2):1) {
          result <- c(result, paste0("(`", vector[i+1], "` / `", vector[i], "` - 1 ) * 100"))
        }
      }
    }
    return(result)
  }
  # getExpressions(relative change)
  EXPR <- lapply(splittedbyLevels, getExpressions)
  EXPR <- as.list(unlist(EXPR))
  names(EXPR) <- as.list(unlist(lapply(splittedbyLevels_id, getExpressions)))

  # do svycontrast
  contrasts <- lapply(EXPR, function(x) parse(text=x))
  change <- as.data.frame(survey::svycontrast(X, contrasts = contrasts))

  # calculate confidence intervals
  #change$ll <- change$nlcon - stats::qt(p=(1-dots$level)/2, df=dots$degfs,lower.tail=FALSE) * change$SE
  #change$ul <- change$nlcon + stats::qt(p=(1-dots$level)/2, df=dots$degfs,lower.tail=FALSE) * change$SE
  change$ll <- change$nlcon - stats::qt(p=(1-dots$level)/2, df=Inf, lower.tail=FALSE) * change$SE
  change$ul <- change$nlcon + stats::qt(p=(1-dots$level)/2, df=Inf, lower.tail=FALSE) * change$SE


  change$measure <- measure

  change$indicator <- indicator

  change$k <- k

  change$loa <- loa

  change$subg <- sapply(rownames(change), function(x) {
    # Extract the pattern "subg(xxx)"
    subg <- unlist(regmatches(x, gregexpr("subg\\(.*?\\)", x)))
    # Extract the pattern "xxx"
    subg <- gsub(".*?\\((.*?)\\).*?", "\\1", subg)
    stopifnot("Upss"=length(unique(subg))==1)
    # Extract the unique value
    unique(subg)}, USE.NAMES = FALSE)

  change$ctype <- "rel"

  change$yt0 <- sapply(rownames(change), function(x) {
    # Extract the pattern "year(xxx)"
    years <- unlist(regmatches(x, gregexpr("year\\(.*?\\)", x)))
    # Extract the pattern "xxx"
    years <- gsub(".*?\\((.*?)\\).*?", "\\1", years)
    min(years)})

  change$yt1 <- sapply(rownames(change), function(x) {
    # Extract the pattern "year(xxx)"
    years <- unlist(regmatches(x, gregexpr("year\\(.*?\\)", x)))
    # Extract the pattern "xxx"
    years <- gsub(".*?\\((.*?)\\).*?", "\\1", years)
    max(years)})

  rownames(change) <- NULL

  if (isTRUE(dots$annualized)){change$ann <- 1}else{change$ann <- 0}

  # change colnames
  colnames(change)[colnames(change) %in% c("nlcon", "SE")] <- c("b", "se")


  return(change)
}
