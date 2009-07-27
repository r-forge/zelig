summary.setx.cond <- function(object, ...) {
  if(any(class(object)=="setx.MI")){
	res <- list()
      for (j in 1:length(object)){
	  res[[j]] <- matrix(NA, nrow(object[[j]]), ncol(object[[j]]))
	  for (i in 1:ncol(object[[j]])){
          res[[j]][,i] <- object[[j]][,i]
	  }
  	    colnames(res[[j]]) <- colnames(object[[j]])
  	    res[[j]] <- as.data.frame(res[[j]])
  	    res[[j]] <- summary(res[[j]],...)
	}
  }	
  else{
    res <- matrix(NA, nrow(object), ncol(object))
    for (i in 1:ncol(object))
      res[,i] <- object[,i]
    colnames(res) <- colnames(object)
    res <- as.data.frame(res)
    res <- summary(res, ...)
  }
  return(res)
}