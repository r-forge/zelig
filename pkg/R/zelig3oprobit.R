zelig3oprobit <- function(res, fcall = NULL, zcall = NULL) {
  inv.link <- function(eta, zeta) {
    tmp1 <- matrix(1, nrow = length(eta), ncol = length(zeta) + 1)
    iprobit <- function(z, e)
      pnorm(z - e)
    tmp1[, 1:length(zeta)] <- sapply(zeta, iprobit, e = eta)
    tmp1
  }
  res$inv.link <- as.function(inv.link)
  res
}

