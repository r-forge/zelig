summary.zeliglist<-function(object, subset = NULL, CI=95, 
                        stats=c("mean", "sd"), ...){
        nm <- names(object)
        lst <- list()
        for(obj in object){
                class(obj) <- c("zelig", class(obj))
                res <- summary.zelig(obj, subset = subset, CI=CI,stats=stats, ...)
                
                lst <- c(lst, list(res))
        }
        names(lst) <- nm
        return(lst)
}




















