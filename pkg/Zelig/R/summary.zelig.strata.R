summary.zelig.strata <-function(object, subset = NULL, CI=95, 
                        stats=c("mean", "sd", "min", "max"), ...){
  m <- length(object)
  if(is.null(subset)) { # summary for all strata together
    qi <- object[[1]]$qi
    if(length(dim(qi[[1]]))==3) {
      w <- NULL
      for (i in 1:length(object))
        w <- c(w, dim(object[[i]]$qi[[1]])[3])
    }
    for (i in 1:length(qi)){
      qi1i <- object[[1]]$qi[[i]]
      if(any(class(object[[1]]$x)=="cond")) {# conditional prediction
        if(length(dim(qi1i))==3){
          tmp <- array(NA, dim=c(dim(qi1i)[1:2], sum(w)))
          tmp[,1:dim(qi1i)[2],1:dim(qi1i)[3]] <- qi1i
        } else {
          tmp <- as.matrix(qi1i)
        }
      } else { # unconditional prediction
        if(length(dim(qi1i))==3){
          tmp <- array(NA, dim=c(dim(qi1i)[1:2], sum(w)))
          tmp[,,1:w[1]] <- qi1i[,,1:w[1]] 
        } else {
          tmp <- as.matrix(qi1i)
        }
      }
      for (j in 2:m) {
        qiji <- object[[j]]$qi[[i]]
        if(any(class(object[[j]]$x)=="cond")) {# conditional prediction
          if(length(dim(qi1i))==3) 
            tmp[,,(sum(w[1:(m-1)])+1):sum(w[1:m])] <- qiji
          else
            tmp <- cbind(tmp, qiji)
        } else{ # unconditional prediction
          if(length(dim(qi1i))==3)
            tmp[(sum(w[1:(j-1)])+1):sum(w[1:j]),,] <- qiji[1:w[j],,]
          else
            tmp <- cbind(tmp, as.matrix(qiji))
        }
      }
      qi[[i]] <- tmp
    }
    c <- match.call()
    c$num <- object[[1]]$call$num
    res <- list(qi=qi, qi.name=object[[1]]$qi.name,
                x=object[[1]]$x, x1=NULL, call=c,
                zelig.call=object[[1]]$zelig.call)
    return(summary.zelig(res, ...))  
  } else { # summary for each strata
    res <- list()
    if(is.function(subset)){
      m <- length(object)
      subset <- 1:m
    }
    else
      m <- length(subset)
    for (i in 1:m) {
      res[[i]] <- summary.zelig(object[[subset[i]]], subset=NULL, ...)
      names(res)[i] <- names(object)[i]
    }
    class(res) <- "summary.zelig.strata"
    return(res)
  }
}



















