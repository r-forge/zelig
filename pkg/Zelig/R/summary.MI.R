summary.MI <- function(object, subset = NULL, ...){
  res <- list()
  if (is.null(subset))
    M <- 1:length(object)
  else
    M <- c(subset)
  for (i in M)  res[[i]] <- summary(object[[i]])
  ans <- list()
  ans$zelig <- getzelig(object[[1]])
  ans$call <- getcall(object[[1]])
  if (length(M) > 1) {
    ans$all <- res
    coef1 <- se1 <- NULL
    for (i in M){
      tmp <-  getcoef(res[[i]])
      coef1 <- cbind(coef1, tmp[,1])
      se1 <- cbind(se1, tmp[,2])
    }
    rows <- nrow(coef1)
    Q <- apply(coef1, 1, mean)
    U <- apply(se1^2, 1, mean)
    B <- apply((coef1-Q)^2, 1, sum)/(length(M)-1)
    var <- U+(1+1/length(M))*B
    nu <- (length(M)-1)*(1+U/((1+1/length(M))*B))^2
    coef.table <- matrix(NA, nrow = rows, ncol = 4)
    dimnames(coef.table) <- list(rownames(coef1),
                                 c("Value", "Std. Error", "t-stat", "p-value"))
    coef.table[,1] <- Q
    coef.table[,2] <- sqrt(var)
    coef.table[,3] <- Q/sqrt(var)
    coef.table[,4] <- pt(abs(Q/sqrt(var)), df=nu, lower.tail=F)*2
    ans$coefficients <- coef.table
    ans$cov.scaled <- ans$cov.unscaled <- NULL
    for (i in 1:length(ans)) {
      if (is.numeric(ans[[i]]) && !names(ans)[i] %in% c("coefficients")){
        tmp <- NULL
        for (j in M)
          tmp <- cbind(tmp, res[[j]][[pmatch(names(ans)[i], names(res[[j]]))]])
        ans[[i]] <- apply(tmp, 1, mean)
      }
    }
    class(ans) <- "summary.MI"
  } else if (length(M) == 1) {
    ans <- summary(object[[M]])
  } else {
    stop("invalid input for `subset'")
  }
  return(ans)
}
