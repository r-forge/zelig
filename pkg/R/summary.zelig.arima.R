summary.zelig.arima <- function(object, CI=95, stats=c("mean", "sd"),  ...){
  if (is.null(object$qi$fd)){
    if (is.null(object$qi$t.eff)){
      test.array <- array(NA, c(ncol(object$qi[[1]]), length(stats)+2, 2))
      for (i in 1:(length(object$qi)-1)){
        for (j in 1:length(stats)){
          test.array[,j, i]<- t(as.matrix(apply(object$qi[[i]], 2, stats[j])))
        }
        test.array[, c(length(stats) +1 , length(stats) + 2), i] <-
          t(as.matrix(apply(object$qi[[i]], 2, "quantile",
                            c((100-CI)/200, 1-(100-CI) / 200))))
      }
      colnames(test.array) <- c(stats, paste(as.character((100-CI)/200), "%", sep=""),
                               paste(as.character(1-(100-CI)/200), "%", sep=""))
      dimnames(test.array)[[3]] <- object$qi.name[1:2] 
      rownames(test.array) <- (object$min.time+1):(object$min.time + nrow(test.array))  
      number.sim <- nrow(object$qi$ev)
    }
    
    if (!is.null(object$qi$t.eff)){
      test.array <- array(NA, c(ncol(object$qi[[1]]), length(stats)+2, 3))
      for (i in 1:length(object$qi)){
        for (j in 1:length(stats)){
          test.array[,j, i]<- t(as.matrix(apply(object$qi[[i]], 2, stats[j])))
        }
        test.array[, c(length(stats) +1 , length(stats) + 2), i] <-
          t(as.matrix(apply(object$qi[[i]], 2, "quantile",
                            c((100-CI)/200, 1-(100-CI)/200))))
      }
      colnames(test.array) <- c(stats, paste(as.character((100-CI)/200), "%", sep=""),
                               paste(as.character(1-(100-CI)/200), "%", sep=""))
      dimnames(test.array)[[3]] <- object$qi.name 
      rownames(test.array) <- (object$min.time+1):(object$min.time + nrow(test.array))  
      number.sim <- nrow(object$qi$ev)
    }
  }
  if (!is.null(object$qi$fd)){
    test.array <- array(NA, c(ncol(object$qi[[1]]), length(stats)+2, 3))
    for (i in 1:(length(object$qi)-1)){
      for (j in 1:length(stats)){
        test.array[,j, i] <- t(as.matrix(apply(object$qi[[i]], 2, stats[j])))
      }
      test.array[, c(length(stats) +1 , length(stats) + 2), i] <-
        t(as.matrix(apply(object$qi[[i]], 2, "quantile",
                          c((100-CI)/200, 1-(100-CI)/200))))
    }
    colnames(test.array) <- c(stats, paste(as.character((100-CI)/200), "%", sep=""),
                              paste(as.character(1-(100-CI)/200), "%", sep=""))
    dimnames(test.array)[[3]] <- object$qi.name[1:3]
    rownames(test.array) <- (object$min.time+1):(object$min.time + nrow(test.array)) 
    number.sim <- nrow(object$qi$ev)   
  }
  out <- list(number.sim = number.sim, test.array = test.array,
              zelig.call = object$zelig.call)
  class(out) <- "arimaSummary"
  out
}
