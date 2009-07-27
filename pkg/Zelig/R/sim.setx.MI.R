sim.setx.MI <- function(object, x, x1 = NULL, num = c(1000, 100), prev = NULL, 
                          bootstrap = FALSE, bootfn = NULL, ...) {
  if (any(class(object) == "MCMCZelig"))
    num <- nrow(object[[1]]$coefficients) * length(object)
  else if (length(num) == 2) {
    if (!bootstrap)
      num <- num[1]
    else 
      num <- num[2]
  }
  ca <- match.call()
  if (!any(class(x) == "cond")) {
    simpar <- MIsimulation(object, num, prev, bootstrap, bootfn=bootfn, x=x, x1=x1, ...)
    if(any(class(object[[1]]) == "coxph"))
      simqi <- qi.coxph(object, simpar = simpar, x = x, x1 = x1)
    else
      simqi <- qi(object[[1]], simpar = simpar, x = as.matrix(x), 
                x1 = if (!is.null(x1)) as.matrix(x1))
    ca$num <- num
    res <- list(x = x, x1 = x1, call = ca, zelig.call = getcall(object[[1]]), 
                par = simpar, qi = simqi$qi, qi.name = simqi$qi.name)
  }
  else {
    simpar <- MIsimulation(object, num, prev, bootstrap, bootfn=bootfn, x=x, x1=NULL, ...)
    tmp.qi <- list()
    for (i in 1:length(x)) {
      if (!is.null(x1)) {
        warning("First Differences are not calculated in conditional prediction models.")
        x1 <- NULL
      }
      if (object[[i]]$call$model %in% c("bprobit", "blogit")) {
        yvar <- x[[i]][,1:2]
        x[[i]] <- x[[i]][,3:ncol(x[[i]])]
	  x[[i]] <- cbind(1,x[[i]])
      }
      else {
        yvar <- x[[i]][,1]
        x[[i]] <- x[[i]][,2:ncol(x[[i]])]
	  x[[i]] <- cbind(1,x[[i]])
      }
      tmp.qi[[i]] <- qi(object[[1]], simpar = simpar, x = x[[i]], x1 =
                        x1[[i]], y = yvar)
    }
    simqi <- tmp.qi[[1]]
    for (i in 2:length(tmp.qi)) {
      for (j in 1:length(simqi)) {
        if (length(dim(simqi$qi[[j]])) == 2)
          simqi$qi[[j]] <- cbind(simqi$qi[[j]], tmp.qi[[i]]$qi[[j]])
        else {
          tmp <- array(NA, dim = c(dim(simqi$qi[[j]])[1],
                             dim(simqi$qi[[j]])[2],
                             (dim(simqi$qi[[j]])[3] +
                              dim(tmp.qi[[i]]$qi[[j]])[3])))
          tmp[,, 1:dim(simqi$qi[[j]])[3]] <- simqi$qi[[j]]
          tmp[,, (dim(simqi$qi[[j]])[3]+1):dim(tmp)[3]] <-
            tmp.qi[[i]]$qi[[j]]
          simqi$qi[[j]] <- tmp
        }
      }
      print(object[[1]]$call)
      ca$num <- num
      res <- list(x = x, x1 = x1, call = ca,
                  zelig.call = getcall(object[[1]]), 
                  par = simpar, qi = simqi$qi, qi.name =
                  simqi$qi.name)
    }
  }
  class(res) <- "zelig"
  res
}







