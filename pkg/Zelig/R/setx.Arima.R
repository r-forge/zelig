setx.Arima <- function(object, cond=FALSE, data=NULL, counter=NULL,
                      pred.ahead=0, ...){
  if (!is.null(counter)) 
    warning("counter ignored in ARIMA models.")
  t.effect <-cond 
  mc <- match.call()
  env <- attr(object$terms, ".Environment")
  if (is.null(data)){
    data <- eval(getcall(object)$data, envir=env)
  }
  if(!is.null(data)){
   data <- as.data.frame(data)
 }
  trew <- na.omit(pmatch(names(mc), colnames(data), duplicates.ok=TRUE))
  wert <- na.omit(pmatch(colnames(data), names(mc), duplicates.ok=TRUE))
  wert <- sort(wert)
  if (length(trew>0)){
    if (pred.ahead!=0){
      warning("'pred.ahead' available only for prediction models\nwith no external regressors\npred.ahead being set to zero\n")
      pred.ahead <- 0 
    }
    test.time <- unlist(lapply(1:length(wert),
                               function(i, mc) eval(mc[[wert[i]]]$time), mc=mc))
    max.time <- max(test.time)
    min.time <- min(test.time)
    if (max.time > length(object$residuals)){
      data.new <- matrix(as.matrix(data[nrow(data),]), ncol=ncol(data),
                         nrow=(max.time - length(object$residuals)), byrow=TRUE)
      colnames(data.new) <- colnames(data)
      dta <- as.matrix(rbind(data, data.new))
      rownames(dta) <- 1:nrow(dta)
    } 
    else { dta  <- data[1:(max.time),] }
    for (i in 1:length(trew)){
      dta[eval(mc[[wert[i]]]$time, envir=env), trew[i]] <-
        eval(mc[[wert[i]]]$value, envir=env)
    }
    dta <- as.matrix(dta[,na.omit(pmatch(names(object$coef), colnames(data)))])
  }
  if (length(trew)==0) {
    data.new <- matrix(as.matrix(data[nrow(data),]), ncol=ncol(data),
                       nrow=pred.ahead, byrow=TRUE)
    colnames(data.new) <- colnames(data)
    dta <- as.matrix(rbind(data, data.new))
    dta <- as.matrix(dta[, na.omit(pmatch(names(object$coef), colnames(data)))])
    rownames(dta) <- 1:nrow(dta)
    min.time <- length(object$residuals)
    max.time <- length(object$residuals)
  }
  out <- list(dta=dta, min.time=min.time, max.time=max.time,
              pred.ahead=pred.ahead, t.effect = cond)
  class(out) <- "setxArima"
  out
}
