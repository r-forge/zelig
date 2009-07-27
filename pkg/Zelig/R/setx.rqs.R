setx.rqs <- function(object, ...){
    x <- vector("list", length(object$tau))
    object <- stratify.rqs(object)
    mc <- match.call(expand.dots=T)
    mc[[1]] <- setx.rq
    for(i in 1:length(object)){
        x[[i]] <- eval(mc,env = parent.frame()) 
    }
    names(x) <- names(object) 
    class(x) <- c("setx.rqs", "data.frame")
    return(x)
}
