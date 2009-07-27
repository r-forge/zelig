summary.strata <- function(object, subset = NULL, ...) {
  res <- list()
  if(is.null(subset))
    M <- 1:length(object)
  else
    M <- c(subset)
  for (i in 1:length(M))
    res[[i]] <- summary(object[[i]])
  if (any(class(object[[1]]) == "MI")) {
    by <- object[[1]][[1]]$call$by
    call <- object[[1]][[1]]$call
  }
  else {
    by <- object[[1]]$call$by
    call <- object[[1]]$call
  }
  names(res) <- paste("summary.", by, names(object)[M], sep = "")
  res$call <- call
  res$by <- by
  res$lev <- names(object)[M]
  res$M <- M
  class(res) <- "summary.strata"
  res
}



