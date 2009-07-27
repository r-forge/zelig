param.rq <- function(object, num, bootstrap=FALSE){
    if (!bootstrap){
        rq.sum <- summary.rq(object, cov=TRUE, se=object$se)
        coef <- mvrnorm(num, mu=object$coef, Sigma=rq.sum$cov)
    }

    else {
        coef <- object$coef
    }
    res <- coef
    res
}
