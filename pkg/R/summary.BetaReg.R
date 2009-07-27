summary.BetaReg <- function(object, digits = getOption("digits"), ...) {
  summ <- matrix(NA, nrow = length(object$coef), ncol = 3)
  colnames(summ) <- c("Estimate", "SD", "t-stat")
  rownames(summ) <- names(object$coef)
  summ[,1] <- object$coef
  summ[,2] <- sqrt(diag(object$variance))
  summ[,3] <- summ[,1] / summ[,2]
  object$coefficients <- summ
  object
}
