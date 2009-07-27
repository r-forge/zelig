set.start <- function(start.val = NULL, terms) {
  if (any(class(terms) == "multiple")) 
    labs <- make.parameters(terms = terms, shape = "vector", ancillary = TRUE)
  else
    labs <- attr(terms, "term.labels")
  if (is.null(start.val))
    start.val <- rep(0, length(labs))
  else {
    if (length(start.val) != length(labs))
      stop(paste("length of 'start.val' does not equal number of model parameters = ",
                 length(labs), ".", sep = ""))
  }
  names(start.val) <- labs
  start.val
}
