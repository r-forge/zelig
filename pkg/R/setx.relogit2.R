setx.relogit2 <- function(object, fn = list(numeric = mean,
                                    ordered = median, other = mode), data =
                          NULL, cond = FALSE,
                          counter = NULL, ...) {
  return(setx.default(object$lower.estimate))
}
                          
