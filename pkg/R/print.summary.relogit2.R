print.summary.relogit2 <- function(x, digits = max(3, getOption("digits") - 3),
                                  ...){
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
  print(x$lower.estimate)
  print(x$upper.estimate)
}
