print.eiRxC <- function(x, digits = max(getOption("digits"), 4), ...) {
  cat("\nCall: \n")
  print.formula(x$call)
  cat("\nCoefficients: \n")
  print(x$coefficients, digits = digits, ...)
}
