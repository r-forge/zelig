sim <- function(object, x = NULL, ...) {
  if (is.null(x))
    UseMethod("sim")
  else
    UseMethod("sim", x)
}
