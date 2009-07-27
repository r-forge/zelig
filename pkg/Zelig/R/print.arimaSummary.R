print.arimaSummary <- function(x, digits = min(getOption("digits"), 3), ...) {
  cat("\nModel:", x$zelig.call$model, "\n", sep = " ")
  cat("Number of simulations: ", x$number.sim, "\n\n")
  dimnames(x$test.array)[[3]] <- dimnames(x$test.array)[[3]]
  cat("Available Quantities of Interest: \n")
  for (i in 1:length(dimnames(x$test.array)[[3]])) {
    cat("\n", dimnames(x$test.array)[[3]][i], "\n")
    print(x$test.array[,,i], digits = digits, ...)
  }
  return(invisible())
}
