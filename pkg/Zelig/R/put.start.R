put.start <- function(start.val, value, terms, eqn) {
  if (!any(class(terms) == "multiple"))
    stop("'put.start()' works with 'parse.formula()'.  Use that first!")
  idx <- names(start.val)
  const <- attr(terms, "constraints")
  if (!is.logical(const)) {
    for (var in colnames(const)) {
      eqns <- paste(names(na.omit(const[,var])), collapse = ":")
      idx[idx == var] <- paste(idx[idx == var], eqns, collapse = ":")
    }
  }
  par.id <- NULL
  for (vars in eqn) 
    par.id <- c(par.id, grep(vars, idx))
  start.val[par.id] <- value
  start.val
}
