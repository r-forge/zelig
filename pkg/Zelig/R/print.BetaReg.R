print.BetaReg <- function(x, digits = getOption("digits"), ...) {
  cat("\nCall: ", deparse(x$call), "\n", fill = TRUE)
  cat("Coefficients:\n")
  if(is.matrix(x$coef)) {
    print(x$coef[1:nrow(x$coef) - 1,], digits = digits, ...)
    cat("\n")
    phi <- c(x$coef[nrow(x$coef),])
    names(phi) <- colnames(x$coef)
    cat("Dispersion parameter (phi): \n")
    print(phi, digits = digits, ...)
  }
  else {
    print(x$coef[1:length(x$coef) - 1], digits = digits, ...)
    cat("\n")
    cat("Dispersion parameter (phi) = ", x$coef[length(x$coef)], "\n")
  }
  invisible(x)
}
