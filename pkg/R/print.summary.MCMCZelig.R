print.summary.MCMCZelig <- function(x, digits=max(3, getOption("digits") - 
3), ...) {
  cat("\nCall: ") 
  print(x$call) 
  cat("\n", "Iterations = ", x$start, ":", x$end, "\n", sep = "")
  cat("Thinning interval =", x$thin, "\n")
  cat("Number of chains =", x$nchain, "\n")
  cat("Sample size per chain =", (x$end -
  x$start)/x$thin + 1, "\n")
  cat("\n", "Mean, standard deviation, and quantiles for marginal posterior distributions.", "\n")
  print(round(x$summary, digits=digits))
  cat("\n")
}
