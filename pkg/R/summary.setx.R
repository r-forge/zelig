summary.setx <- function(object, ...) {
  res <- matrix(NA, nrow(object), ncol(object))
  for (i in 1:ncol(object))
    res[,i] <- object[,i]
  colnames(res) <- colnames(object)
  res <- as.data.frame(res)
  summary(res, ...)
}
