.checkArgs_set <- function(data, indicators, ..., name = "unnamed", desc = "desc."){

# Check arguments ---------------------------------------------------------

  ### `data` argument
  ## check if `data` missing argument
  if (missing(data)){stop("Error: `data` not found")}
  ## check if `data` has non-NULL dim()
  stopifnot("`data` should have non-NULL dimension" = !is.null(dim(data)))
  #if (any(dim(data)) == 0){stop("`data` rows or columns are equal to zero")}
  stopifnot("`data` rows or columns are equal to zero" = all(dim(data) != 0))
  ## check if `data` is a `survey.design` class object. If not, it is coerced to a `survey.design` class assuming simple random sampling
  if (!inherits(data, "survey.design")) {
    warning("`data` is not `survey.design` class. Coerced to survey.design class.")
    data <- as.data.frame(data)
    data <- survey::svydesign(id=~rownames(data), data = data)
  }

  ### `indicators` argument
  ## check if `data` missing argument
  if (missing(indicators)) {stop("Error: `indicators` not found")}
  ## check if is a list
  if (is.list(indicators)){
    ## check if it has less than 10 dimensions
    stopifnot("A total of 10 dimensions is permitted." = length(indicators) <= 10)
    ## check if the names of the dimensions have less than 10 characters
    stopifnot(" It is recommended to use short variable o dimension names (at most 10 characters are permitted)" = all(nchar(names(indicators)) <= 10))
    ## check if `indicators` are `character` class
    stopifnot("`indicators` should be a `character`" = is.character(unlist(indicators)))
    ## check if `indicators` are in `data` colnames()
    stopifnot("At least one indicator is not found in `data`" = unlist(indicators) %in% colnames(data))
  } else {
    ## check if `indicators` is a vector of `character` class
    stopifnot("`indicators` should be a `character`" = is.character(indicators))
    ## check if `indicators` are in `data` colnames()
    stopifnot("At least one indicator is not found in `data`" = indicators %in% colnames(data))
    # coerce to list
    indicators <- as.list(indicators)
  }

  ### `name` argument
  ## check if `name` is `character`
  stopifnot("`name` should be a `character`" = is.character(name))
  ## allow at most 10 characters
  stopifnot("`name` should be have at most 10 characters" = nchar(name) <= 10)

  ### `desc` argument
  ## check if `desc` is `character`
  stopifnot("`description` should be a `character`" = is.character(desc))


# check NA's --------------------------------------------------------------

  overall.nas <- 0
  for (i in unlist(indicators, use.names = F)) {
    nas <- sum(is.na(data$variables[,i]));
    if(nas > 0){warning(nas," missing values found in ",i," indicator")}
    overall.nas <- overall.nas + nas
  }
  if(overall.nas>0){warning("Missing values found in some indicators. As a result, standard errors will not be calculated")}

  return(list(
    data = data,
    indicators = indicators,
    name = name,
    desc = desc)
  )
}
