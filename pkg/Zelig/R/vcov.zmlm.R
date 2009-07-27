vcov.zmlm <- function(object,...){
   so <- summary.zmlm(object, corr = FALSE)[[1]]
    kronecker(estVar(object), so$cov.unscaled, make.dimnames = TRUE)
 }
