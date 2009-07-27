summary.zaovlist <- function (object, ...) {
        object$call <- NULL
        stats:::summary.aovlist(object,...)
}


summary.zmaov <- function(object, ...){
        summary.aov(object)
}


summary.zmlm <- function (object, ...) {
        frm <- object$call$formula
        frm <- eval(frm)
        ## solving a bug in the stats::summary.mlm, it does not work if
        ## you pass the formula as a variable
        object$call$formula <- frm
        ## coef.aov gives the wrong formatting and I want
        ## to use coef.default instead for class aov
        if("aov" %in% class(object)){
                ix <- grep("aov", class(object))
                class(object) <- class(object)[-ix]
        }
        
        summary.mlm(object,...)
}
