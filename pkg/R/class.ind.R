#borrow from library(nnet)
class.ind<-function (cl, levels.data=NULL) 
{
    n <- length(cl)
    cl <- as.factor(cl)
    levels(cl)<-levels.data
    x <- matrix(0, n, length(levels(cl)))
    x[(1:n) + n * (unclass(cl) - 1)] <- 1
    dimnames(x) <- list(names(cl), levels(cl))
    x
}
