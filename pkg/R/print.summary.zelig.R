print.summary.zelig <- function(x, digits=getOption("digits"),
                              print.x=FALSE, ...){
  cat("\n  Model:", x$model, "\n")
  if (!is.null(x$num))
      cat("  Number of simulations:", x$num, "\n")
  if (!is.null(x$x)) {
    if (print.x || nrow(x$x) == 1 || is.null(dim(x$x))) {
      if (any(class(x$x) == "cond"))
        cat("\nObserved Data \n")
      else
        cat("\nValues of X \n")
      print(x$x, digits=digits, ...)
      if(!is.null(x$x1)){
        cat("\nValues of X1 \n")
        print(x$x1, digits=digits, ...)
      }
    }
    else {
      if (any(class(x$x) == "cond"))
        cat("\nMean Values of Observed Data (n = ", nrow(x$x), ") \n", sep = "")
      else
        cat("\nMean Values of X (n = ", nrow(x$x), ") \n", sep = "")
      print(apply(x$x, 2, mean), digits=digits, ...)
      if (!is.null(x$x1)) {
        cat("\nMean Values of X1 (n = ", nrow(x$x1), ") \n", sep = "")
        print(apply(x$x1, 2, mean), digits=digits, ...) 
      }
    }
  }
  for (i in 1:length(x$qi.name)){
    indx <- pmatch(names(x$qi.name[i]), names(x$qi.stats))
    tmp <- x$qi.stats[[indx]]
#    if (names(x$qi.name)[indx] == "pr" && colnames(tmp)[1] != "mean")
#      lab <- paste(x$qi.name[[i]], "(percentage of simulations)", sep = " ")
#    else
      lab <- x$qi.name[[i]]
    cat("\n", lab, "\n", sep = "")
    if (length(dim(tmp)) == 3) {
        for (j in 1:dim(tmp)[3]){
          cat("\n  Observation", dimnames(tmp)[[3]][j], "\n")
          if (is.null(rownames(tmp[,,j])))
            rownames(tmp[,,j]) <- 1:nrow(tmp[,,j])
          if (!is.null(names(tmp[,,j])))
            names(tmp[,,j]) <- NULL
          print(tmp[,,j], digits=digits, ...)
        }
      }
    else {
      if (is.matrix(tmp) & is.null(rownames(tmp)))
        rownames(tmp) <- 1:nrow(tmp)
#      if (!is.null(names(tmp)))
#        names(tmp) <- NULL
      print(tmp, digits=digits, ...)
    }
  }
}












