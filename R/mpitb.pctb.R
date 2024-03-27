mpitb.pctb <- function(x,y){
  by_cols <- c("loa","subg","k","t")[c("loa","subg","k","t") %in% colnames(y)]
  pctb <- merge(x[x$measure == "actb",], y[y$measure == "M0", c("b",by_cols)], by = by_cols,
                sort = F, no.dups = FALSE, all = F)
  pctb$b <- pctb$b.x/pctb$b.y
  pctb$measure <- "pctb"
  y <- merge(y, pctb, all=T)
  y$b.x <- NULL; y$b.y <- NULL
  return(y)
}
