repl.default <- function(object, data=NULL, ...) {
  if (!is.null(data))
    object$call$data <- data
  eval(object$call, sys.parent())
}
