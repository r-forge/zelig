mcmcei <- function(formula, data, ...) {
  #if (is.null(rownames(data))) {
  #  rownames(data) <-1:nrow(data)
  # assign(data, as.character(mc$data), env = .GlobalEnv)
  #}
  res <- NULL
  vars <- model.frame(formula, data)
  vars <- as.matrix(vars)
  res$c0 <- vars[,1]
  res$c1 <- vars[,2]
  res$r0 <- vars[,3]
  res$r1 <- vars[,4]
  res<-as.data.frame(res)
 res
}
