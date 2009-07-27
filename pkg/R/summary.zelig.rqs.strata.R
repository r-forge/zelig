summary.zelig.rqs.strata <- function(object, subset=NULL, CI=95,
                                     stats=c("mean", "sd", "min", "max"),...)
{
    class(object) <- "zelig.strata"
    if(is.null(subset))
        subset <- rq
    summary(object, subset, CI, stats)
}
