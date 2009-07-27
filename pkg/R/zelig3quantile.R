zelig3quantile <- function(res, fcall = NULL, zcall = NULL) {
    se <- eval(zcall$se)
    if(!is.null(se) && se=="rank"){
        warning("Rank test inversion is incompatible with estimation of the covariance matrix. Switching se method to \"nid\".")
        se <- "nid"
    }
    else if(is.null(se))
        se <- "nid" 
    res$se <- se
    res
}

stratify.rqs <- function(object){
    x <- vector("list", length(object$tau))
    for(i in 1:length(object$tau)){
        xi <- object
        xi$coefficients <- xi$coefficients[,i]
        xi$residuals <- xi$residuals[,i]
        xi$tau <- xi$tau[i]
        class(xi) <- "rq"
        x[[i]] <- xi 
    }
    names(x) <- object$tau
    x
}

