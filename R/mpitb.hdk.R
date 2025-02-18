mpitb.hdk_svyciprop <- function(k, indicator, data, over,
                               cot, tvar,
                               level){
  # poverty cutoff as decimal
  k <- k/100
  # calculate censored deprivation score and add to the dataset
  censored_indicator <- ifelse(data$variables$c.score >= k,data$variables[,indicator],0)
  data <- update_svy(data, y = censored_indicator)

  # define the vector of the subgroups (check if should be calculated over time)
  # define the expression for the formula
  if (cot == TRUE){bys <- paste(over, tvar, sep= "+")}else{bys <- over}

  # calculate the adjusted headcount ratio
  # if it is used Linux OS, it uses fork to parallelize calculations by loa
  # (if (multicore) parallel::mclapply else lapply)
  hdk_j <- lapply(bys, function(by)
    survey::svyby(survey::make.formula("y"),
                  by=survey::make.formula(by),
                  design=data, survey::svyciprop,
                  vartype = c("se","ci"), level = level, df = survey::degf(data), multicore = TRUE))
  # transform the output in a dataframe according to the desired output
  # transform.svyciprop function is in utils.R
  # if it is used Linux OS, it uses fork to parallelize calculations by loa
  # (if (multicore) parallel::mclapply else lapply)
  ctype_lev <- lapply(hdk_j, transform_svyciprop)
  # rowbind of the subgroups dataframes
  ctype_lev <- do.call("rbind", ctype_lev)
  # create variables
  ctype_lev$measure <- "hdk"
  ctype_lev$ctype <- "lev"
  ctype_lev$k <- k*100
  ctype_lev$indicator <- indicator

  # order dataframe by the column names
  ordered_columns <- c("b", "se", "ll", "ul")
  ctype_lev <- ctype_lev[, c(ordered_columns, setdiff(names(ctype_lev), ordered_columns))]

  return(ctype_lev)
}

mpitb.hdk_svymean <- function(k, indicator, data, over,
                             tvar, cotyear,
                             level){
  # poverty cutoff as decimal
  k <- k/100
  # calculate censored deprivation score and add to the dataset
  censored_indicator <- ifelse(data$variables$c.score >= k,data$variables[,indicator],0)
  data <- update_svy(data, y = censored_indicator)

  # define the vector of the subgroups
  # define the expression for the formula
  bys <- paste(over, tvar, sep= "+")

  # calculate the adjusted headcount ratio
  # if it is used Linux OS, it uses fork to parallelize calculations by loa
  # (if (multicore) parallel::mclapply else lapply)
  # calculate the estimated measures with svymean because svyciprop do not yield
  # the covariance for delta method!
  hdk_j <- lapply(bys, function(by) survey::svyby(survey::make.formula("y"),
                                               by=survey::make.formula(by),
                                               design=data, survey::svymean, covmat = TRUE, na.rm=FALSE, multicore = TRUE))
  hdk_j <- lapply(hdk_j, function(x) {attr(x,"indicator") <- indicator; x})
  hdk_j <- lapply(hdk_j, function(x) {attr(x,"measure") <- "hdk"; x})
  hdk_j <- lapply(hdk_j, function(x) {attr(x,"k") <- k*100; x})
  return(hdk_j)
}
