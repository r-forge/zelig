zelig3ologit <- function(res, fcall = NULL, zcall = NULL) { 
  inv.link <- function(eta, zeta) {
    tmp1 <- matrix(1, nrow = length(eta), ncol = length(zeta) + 1)
    ilogit <- function(e, z) {
      exp(z - e) / (1 + exp(z - e))
    }
    tmp1[, 1:length(zeta)] <- sapply(zeta, ilogit, e = eta)
    tmp1
  }
  res$inv.link <- as.function(inv.link)
  res
}
