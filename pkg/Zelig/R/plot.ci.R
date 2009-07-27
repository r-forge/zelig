plot.ci <- function(x, CI=95, qi = "ev", main = "",
                    ylab = NULL, xlab = NULL, xlim = NULL,
                    ylim = NULL, col = c("red", "blue"), ...) {
  "%w/o%" <- function(x,y) x[!x %in% y] #--  x without y
  if (class(x) != "zelig")
    stop(" plot.ci() works only for sim() output.")
  if (!(x$zelig.call$model) %in% 
        c("ls", "logit", "probit", "exp", "gamma", "lognorm",
          "weibull", "normal", "poisson", "tobit", "relogit",
          "negbin", "logit.bayes", "probit.bayes",
          "poisson.bayes", "normal.bayes", "tobit.bayes",
          "ls.mixed", "logit.mixed", "probit.mixed",
          "gamma.mixed", "poisson.mixed",
          "logit.gam", "gamma.gee", "normal.gam", "poisson.gam",
          "probit.gam", "logit.gee", "normal.gee",
          "poisson.gee", "probit.gee"))
    stop("\n  plot.ci() is valid only for non-categorical, univariate response models.")
  cip <- c((100-CI)/200, 1-(100-CI)/200)
  summarize <- function(z, cip){
    res <- NULL
    res <- cbind(res, apply(z, 2, quantile, prob=cip[1]))
    res <- cbind(res, apply(z, 2, quantile, prob=cip[2]))
    res
  }
  vv <- apply(x$x, 2, unique)
  idx <- sapply(vv, length)
  cidx <- which(idx > 1)
  if (!is.null(x$x1)) { 
    vv1 <- apply(x$x1, 2, unique)
    idx1 <- sapply(vv1, length)
    cidx1 <- which(idx1 > 1)
    if (!identical(names(idx), names(idx1)))
      stop("variables in x and x1 do not match.")
    ## Checking for one dimension of variation, including interaction terms
    if (length(cidx) > length(cidx1)) { 
      tmp <- names(idx)[cidx %w/o% cidx1]
      tmp1 <- names(idx)[cidx[cidx %in% cidx1]]
    }
    else { 
      tmp <- names(idx1)[cidx1 %w/o% cidx]
      tmp1 <- names(idx1)[cidx1[cidx1 %in% cidx]]
    }
  check <- grep(tmp1, tmp)
  if (length(check) != length(tmp)) 
    stop("x and x1 vary on more than one dimension.")
  }
  var <- vv[[cidx[1]]]
  q <- pmatch(qi, names(x$qi))
  qofi <- x$qi[[q]]
  sum.qi <- summarize(qofi, cip)
  if (!is.null(x$x1) && qi == "ev") {
    fd <- x$qi$fd
    ev1 <- fd + qofi
    sum.qi1 <- summarize(ev1, cip)
  }
  else sum.qi1 <- NULL
  if (is.null(ylab))  ylab <- x$qi.name[[q]]
  if (is.null(xlab))  xlab <- paste("Range of", colnames(x$x)[cidx[1]])
  if (is.null(ylim)) {
    if (is.null(sum.qi1))  ylim <- c(min(sum.qi), max(sum.qi))
    else  ylim <- c(min(sum.qi, sum.qi1), max(sum.qi, sum.qi1))
  }
  if (is.null(xlim))  xlim <- c(min(var), max(var))
  plot.default(var, type = "n", ylab = ylab, main = main, xlab = xlab, 
               xlim = xlim, ylim = ylim)
  for (i in 1:length(var)) {
    lines(c(var[i], var[i]), c(sum.qi[i,1], sum.qi[i,2]), col = col[1], ...)
    if (!is.null(x$x1) && qi == "ev")
      lines(c(var[i], var[i]), c(sum.qi1[i,1], sum.qi1[i,2]), col = col[2], ...)
  }
}










