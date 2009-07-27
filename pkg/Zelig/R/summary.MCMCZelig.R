
summary.MCMCZelig <- function(object, quantiles = c(0.025, 0.5, 0.975), ...) {
  require(coda)
  out <- list()
  out$summary <- cbind(summary(object$coefficients)$statistics[,1:2],
                          summary(object$coefficients,
  quantiles=quantiles)$quantiles)
                       
  colnames(out$summary) <- c("Mean", "SD", paste(quantiles*100, "%",sep=""))
  stuff <- attributes(object$coefficients)
  out$call <- object$call
  out$start <- stuff$mcpar[1]
  out$end <- stuff$mcpar[2]
  out$thin <- stuff$mcpar[3]
  out$nchain <- 1
  class(out) <- "summary.MCMCZelig"
  out
}

