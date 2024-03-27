
# Update survey design ----------------------------------------------------

update_svy <- function(object, ...) {
  dots <- substitute(list(...))[-1]
  newnames <- names(dots)

  for(j in seq(along=dots)){
    object$variables[, newnames[j]] <- eval(dots[[j]], object$variables, parent.frame())
  }
  object$call<-sys.call(-1)

  object
}


# Transform svyby class ---------------------------------------------------

transform_svyciprop <- function(X){
  # add subgroup column
  X$loa <- colnames(X)[1]
  # change column name with the level for "level"
  colnames(X)[1] <- "subg" # for "level of analysis"
  # preserve colnames
  cols <- colnames(X)
  # rule out rownames
  rownames(X) <- NULL
  # rule out attributes to reduce memory usage
  attributes(X) <- NULL

  X <- as.data.frame(X)
  colnames(X) <- cols
  # change colnames according to names (slower but tidier!)
  colnames(X)[colnames(X) %in% c("y", "se.as.numeric(y)","ci_l","ci_u")] <- c("b", "se","ll","ul")
  #X[X$subg == "nat","loa"] <- NA_character_
  return(X)
}

