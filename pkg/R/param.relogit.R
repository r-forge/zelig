param.relogit <- function(object, num, x, bootstrap = FALSE, bootfn = NULL) {
  if ("relogit2" %in% class(object)) {
    pping <- function(tmp0, tmp1, num, bootstrap, x) {
      par0 <- param.relogit(tmp0, num=num, x=x, bootstrap=bootstrap)
      par1 <- param.relogit(tmp1, num=num, x=x, bootstrap=bootstrap)
      P00 <- as.matrix(qi.relogit(tmp0, par0, x=x)$qi$ev)
      P10 <- as.matrix(qi.relogit(tmp1, par1, x=x)$qi$ev)
      test <- P00[,1] < P10[,1]
      par0 <- as.matrix(par0[test,])
      par1 <- as.matrix(par1[test,])
      list(par0 = par0, par1 = par1)
    }
    tmp0 <- object$lower.estimate
    tmp1 <- object$upper.estimate
    tmp <- pping(tmp0, tmp1, num = num, bootstrap=bootstrap, x=x)
    par0 <- tmp$par0
    par1 <- tmp$par1
    while (nrow(par0) < num) {
      tmp <- pping(tmp0, tmp1, num=num, bootstrap=bootstrap, x=x)
      par0 <- rbind(par0, tmp$par0)
      par1 <- rbind(par1, tmp$par1)
    }
    if (nrow(par0) > num) {
      par0 <- par0[1:num,]
      par1 <- par1[1:num,]
    }
    par0 <- as.matrix(par0)
    par1 <- as.matrix(par1)
    rownames(par0) <- 1:nrow(par0)
    rownames(par1) <- 1:nrow(par1)
    return(list(par0 = par0, par1 = par1))    
  } else {
    if (!bootstrap) 
      return(mvrnorm(num, mu = coef(object), Sigma = vcov(object)))
    else
      return(coef(object))
  }
}
