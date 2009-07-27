setx.default <- function(object, fn = list(numeric = mean, ordered =
                                   median, other = mode), data = NULL,
                         cond = FALSE, counter = NULL, ...){
  mc <- match.call()
  if (class(object)[1]=="MI")
    object <- object[[1]]

  mode <- function(x){
    tb <- tapply(x, x, length)
    if(is.factor(x))
      value <- factor(unlist(labels(tb[seq(along=tb)[tb==max(tb)]])),
                      levels=levels(x))
    else if (is.logical(x))
      value <- as.logical(unlist(labels(tb[seq(along=tb)[tb==max(tb)]])))
    else if (is.character(x))
      value <- as.character(unlist(labels(tb[seq(along=tb)[tb==max(tb)]])))
    else
      stop(paste(vars[i], "is not a supported variable type."))
    if (length(value)>1) {
      warning("There is more than one mode. The first level is selected.")
      value <- sort(value)[1]
    }
    return(value)
  }
  
  median.default <- median
  median <- function(x) {
    if(is.numeric(x))
      value <- median.default(x)
    else if (is.ordered(x)) {
      value <- factor(levels(x)[quantile(as.integer(x), type = 1, prob = 0.5)],
                      levels=levels(x)) 
    } else
      stop("median cannot be calculated for this data type")
    return(value)
  }
  
 
  max.default <- max
  max <- function(x, na.rm=FALSE) {
    if(is.numeric(x))
      value <- max.default(x, na.rm=na.rm)
    else if (is.ordered(x)) 
      value <- factor(levels(x)[length(levels(x))], levels=levels(x))
    else
      stop("max cannot be calculated for this data type")
    return(value)
  }
  
  min.default <- min
  min <- function(x, na.rm=FALSE) {
    if(is.numeric(x))
      value <- min.default(x, na.rm = na.rm)
    else if (is.ordered(x))
      value <- factor(levels(x)[1], levels=levels(x))
    else
      stop("min cannot be calculated for this data type")
    return(value)
  }
 
  
  # Testing From Here
  if(length(fn))
    fn <- updatefn(fn, operVec=c("mode", "median","min", "max"),
                   ev=environment(), global=parent.frame())
  
  tt <- terms(object)
  tt.attr <- attributes(tt)
  env <- tt.attr$.Environment
  if (is.null(env))
    env <- parent.frame()
  ## original data
  if (is.null(data)) {
    if (nrow(as.data.frame(getdata(object))) > 0)
      dta <- getdata(object)
    else
      dta <- eval(getcall(object)$data, envir = env)
  }
  else
    dta <- as.data.frame(data)
  ## extract variables we need
  mf <- model.frame(tt, data = dta, na.action = na.pass)
  if(any(class(tt)=="multiple"))
    vars<-unlist(c(attr(tt,"depVars"),attr(tt,"indVars")),use.names=FALSE)
  else
  vars <- all.vars(tt)
  if (!is.null(tt.attr$response) && tt.attr$response)
    resvars <- all.vars(tt.attr$variables[[1+tt.attr$response]])
  else
    resvars <- NULL
  opt <- vars[na.omit(pmatch(names(mc), vars))]
  data <- dta[complete.cases(mf), names(dta)%in%vars, drop=FALSE]
  if (!is.null(counter)) {
    if (!any(counter == vars))
      stop("the variable specified for counter is not used in the model")
    treat <- data[, names(data)==counter]
    if(is.numeric(treat)) {
      data[treat==1, names(data)==counter] <- 0
      data[treat==0, names(data)==counter] <- 1
    } else if(is.factor(treat)) {
      lev <- levels(treat)
      if(length(lev)==2) {
        treat <- as.numeric(treat) - 1 
        data[treat==1, names(data)==counter] <- lev[1]
        data[treat==0, names(data)==counter] <- lev[2]
      } else {
        stop("counter only takes a binary variable")
      }
    } else if(is.logical(treat)) {
      treat <- as.numeric(treat)
      data[treat==1, names(data)==counter] <- FALSE
      data[treat==0, names(data)==counter] <- TRUE
    } else {
      stop("not supported variable type for counter")
    }
    if(!cond)
      stop("if counter is specified, cond must be TRUE")
  }
  if (cond) {
    if (is.null(data)) 
      stop("if cond = TRUE, you must specify the data frame.")
    if (is.null(mc$fn))
      fn <- NULL
    if (!is.null(fn)) {
      warning("when cond = TRUE, fn is coerced to NULL")
      fn <- NULL
    }
    maxl <- nrow(data)
  } else if (!is.null(fn)) {
    if (is.null(fn$numeric) || !is.function(fn$numeric)) {
      warning("fn$numeric coerced to mean().")
      fn$numeric <- mean
    }
    if (is.null(fn$ordered) || !is.function(fn$ordered) || 
        identical(mean, fn$ordered)) {
      warning("fn$ordered coreced to median().")
      fn$ordered <- median
    } else if (identical(min.default, fn$ordered)) {
      fn$ordered <- min
    } else if (identical(max.default, fn$ordered)) {
      fn$ordered <- max
    } else if (identical(median.default, fn$ordered)) {
      fn$ordered <- median
    }
    if (is.null(fn$other) || !is.function(fn$other)) { 
      warning("the only available fn for other is mode.")
      fn$other <- mode
    }
    for (i in 1:ncol(data)) {
      if (!(colnames(data)[i] %in% opt)) {
        if (!(colnames(data)[i] %in% resvars)) {
          if (is.numeric(data[,i]))
            value <- lapply(list(data[,i]), fn$numeric)[[1]]
          else if (is.ordered(data[,i])) 
            value <- lapply(list(data[,i]), fn$ordered)[[1]]
          else 
            value <- lapply(list(data[,i]), fn$other)[[1]]
          data[,i] <- value
        }
      }
    }
    maxl <- 1
  } else {
    maxl <- nrow(data)
  }
  if (length(opt) > 0)
    for (i in 1:length(opt)) {
      value <- eval(mc[[opt[i]]], envir = parent.frame())
      lv <- length(value)
      if (lv>1)
        if (maxl==1 || maxl==lv) {
          maxl <- lv
          data <- data[1:lv,,drop = FALSE]
        }
        else
          stop("vector inputs should have the same length.")
      if (is.factor(data[,opt[i]]))
        data[,opt[i]] <- list(factor(value, levels=levels(data[,opt[i]])))
      else if (is.numeric(data[,opt[i]]))
        data[,opt[i]] <- list(as.numeric(value))
      else if (is.logical(data[,opt[i]]))
        data[,opt[i]] <- list(as.logical(value))
      else
        data[,opt[i]] <- list(value)
    }
  data <- data[1:maxl,,drop = FALSE]
  
  if (cond) {
    X <- model.frame(tt, data = dta)
    if (!is.null(counter)) {
      X <- list(treat=X[treat==1,,drop=FALSE],
                control=X[treat==0,,drop=FALSE])
      class(X$treat) <- class(X$control) <- c("data.frame", "cond")
      class(X) <- "setx.counter"
    }
    else
      class(X) <- c("data.frame", "cond")
  }
  else {
    X <- as.data.frame(model.matrix(tt, data = data))
  }
  return(X)
}
### DESCRIPTION: Takes the operations in vector operVec and updates list fn
###              so that list elements "numeric", "ordered", and "other" in fn
###              are as defined in setx rather
###              than those taken from .GlobalEnv or namespace:base
###
### INPUTS: fn a list with default operations for numeric, ordered, other
###         operVec a vector of chars with operations, e.g max, min, median, mode
###         ev, parent environment; global, granparent environment
###
updatefn <- function(fn, operVec=c("mode", "median","min", "max"), ev=parent.frame(), global=.GlobalEnv)
   {
     mode   <- get("mode", env=ev)
     median <- get("median", env=ev)
     max   <- get("max", env=ev)
     min   <- get("min", env=ev)
      
     modeG   <- get("mode", env=global)
     medianG <- get("median.default", env=global)
     minG <- get("min", env=global)
     maxG <-  get("max", env=global)
    if(!identical(sort(c("max", "median", "min", "mode")), sort(operVec)))
      stop("updatefn missing some operations from setx")
     
    for(oper in operVec){     
      operGlob <- switch(EXPR=oper,"mode"=, "mode.default"=modeG,"median"=, "median.default"=medianG,
                         "min"=,"min.default"= minG,"max"=, "max.default"=maxG)
      operSetx <- switch(EXPR=oper,"mode"=, "mode.default"=mode,"median"=, "median.default"=median,
                         "min"=,"min.default"= min,"max"=, "max.default"=max)
      if(identical(fn$other, operGlob))
        fn$other <-  operSetx
   
      if(identical(fn$numeric, operGlob))
        fn$numeric <-  operSetx
      
      if(identical(fn$ordered, operGlob))
        fn$ordered <- operSetx
    }
     fn
   }
