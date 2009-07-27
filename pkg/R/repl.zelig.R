repl.zelig <- function(object, data=NULL, prev = NULL, x=NULL,
  x1=NULL, bootfn=NULL, ...) {
  rep.zelig <- eval(object$zelig.call, sys.parent())
  object$call$object <- rep.zelig
  if (is.null(x))
    object$call$x <- object$x
  else
    object$call$x <- x
  if (is.null(x1))
    object$call$x1 <- object$x1
  else
    object$call$x1
  if (!is.null(prev))
    object$call$prev <- prev
  if (!is.null(object$seed)) set.seed(object$seed)
  eval(object$call, sys.parent())
}




