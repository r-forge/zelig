summary.eiRxC <- function(object, ...) {
  out <- list(call = object$call)
  coef <- matrix(NA, nrow = length(object$coefficients), ncol = 3)
  rownames(coef) <- names(object$coefficients)
  coef[,1] <- object$coefficients
  coef[,2] <- sqrt(diag(object$vcov))
  coef[,3] <- coef[,1] / coef[,2]
  #coef[,4] <- dchisq(coef[,3])  # Fix
  colnames(coef) <- c("Estimate", "Std. Error", "t value")
  out$coefficients <- coef
  class(out) <- "eiRxC"
  out
}
