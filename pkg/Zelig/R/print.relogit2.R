print.relogit2 <- function(x, digits = max(3, getOption("digits") - 3),
                          ...) {
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
  print.relogit(x$lower.estimate)
  print.relogit(x$upper.estimate)
}
             
