mpitb.hd_svyciprop <- function(k, indicator, data, over,
                                cot, tvar,
                                level){


  data <- update_svy(data, y = data$variables[,indicator])

  # define the vector of the subgroups (check if should be calculated over time)
  # define the expression for the formula
  if (cot == TRUE){bys <- paste(over, tvar, sep= "+")}else{bys <- over}

  # calculate the adjusted headcount ratio
  # if it is used Linux OS, it uses fork to parallelize calculations by loa
  # (if (multicore) parallel::mclapply else lapply)
  hd_j <- lapply(bys, function(by)
    survey::svyby(survey::make.formula("y"),
                  by=survey::make.formula(by),
                  design=data, survey::svyciprop,
                  vartype = c("se","ci"), level = level, df = survey::degf(data), multicore = TRUE))
  # transform the output in a dataframe according to the desired output
  # transform.svyciprop function is in utils.R
  # if it is used Linux OS, it uses fork to parallelize calculations by loa
  # (if (multicore) parallel::mclapply else lapply)
  ctype_lev <- lapply(hd_j, transform_svyciprop)
  # rowbind of the subgroups dataframes
  ctype_lev <- do.call("rbind", ctype_lev)
  # create variables
  ctype_lev$measure <- "hd"
  ctype_lev$ctype <- "lev"
  ctype_lev$k <- NA
  ctype_lev$indicator <- indicator

  # order dataframe by the column names
  ordered_columns <- c("b", "se", "ll", "ul")
  ctype_lev <- ctype_lev[, c(ordered_columns, setdiff(names(ctype_lev), ordered_columns))]

  return(ctype_lev)
}

mpitb.hd_svymean <- function(k, indicator, data, over,
                              tvar, cotyear,
                              level){

  data <- update_svy(data, y = data$variables[,indicator])

  # define the vector of the subgroups
  # define the expression for the formula
  bys <- paste(over, tvar, sep= "+")

  # calculate the adjusted headcount ratio
  # if it is used Linux OS, it uses fork to parallelize calculations by loa
  # (if (multicore) parallel::mclapply else lapply)
  # calculate the estimated measures with svymean because svyciprop do not yield
  # the covariance for delta method!
  hd_j <- lapply(bys, function(by) survey::svyby(survey::make.formula("y"),
                                                  by=survey::make.formula(by),
                                                  design=data, survey::svymean, covmat = TRUE, na.rm=FALSE, multicore = TRUE))
  hd_j <- lapply(hd_j, function(x) {attr(x,"indicator") <- indicator; x})
  hd_j <- lapply(hd_j, function(x) {attr(x,"measure") <- "hd"; x})
  hd_j <- lapply(hd_j, function(x) {attr(x,"k") <- NA; x})
  return(hd_j)
}
