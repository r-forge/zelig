zelig2tobit <- function(formula, model, data, M, ...) {
        mf <- match.call(expand.dots = TRUE)
        mf$model <- mf$M <- NULL
        
        mf[[1]] <- survival::survreg
        mf$dist <- "gaussian"
        if (is.null(mf$above)) 
          above <- Inf
        else {
                above <- mf$above
                mf$above <- NULL
        }
        if (is.null(mf$below))
          below <- 0
        else {
                below <- mf$below
                mf$below <- NULL
        }
        
        ## Fixing the call for robust SEs
        if (is.null(mf$robust))
          mf$robust <- FALSE
        if (!is.null(mf$cluster) & !mf$robust) 
          stop("\nIf cluster is specified, robust must be TRUE.")

  
        if (!is.null(mf$cluster)) {
                mf$formula <- update(mf$formula, paste(". ~ . + ", paste("cluster(",mf$cluster,")")))
                mf$cluster <- NULL
        } else if (mf$robust)
          mf$formula <- update(formula, paste(". ~ . + ", paste("cluster(1:nrow(",deparse(formula[[2]]),"))")))
    
        ## Fixing the call for left censoring
        if (length(grep("Surv", as.character(formula[[2]]))) == 0) { 
                if (above == Inf & is.numeric(below) & below != -Inf) {
                        tt <- "left"
                }
                else if (below == -Inf & above == Inf) {
                        stop("No censoring, use zelig(..., model = \"normal\") instead!")
                }
                else if (below == -Inf & is.numeric(above) & above != Inf) {
                        stop("Right censored data not supported in a tobit model.")
                }
                else if (is.numeric(above) & is.numeric(below) & above != Inf & below != -Inf) {
                        stop("Interval censored data not suppored in a tobit model.")
                }
                mf$formula[[2]] <- call("Surv", formula[[2]],
                                        call("<", below, formula[[2]]),
                                        type = "left")
        }
        as.call(mf)
}
