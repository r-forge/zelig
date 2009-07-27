setx.coxph <- function(object, fn = list(numeric = mean, ordered =
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

################################ create new terms without strata or cluster
  if(!is.null(tt.attr$specials$strata) | !is.null(tt.attr$specials$cluster)){
    no.st.cl <- colnames(tt.attr$factors)[-c(tt.attr$specials$strata - 1,
                                       tt.attr$specials$cluster - 1)]
    rhs <- paste(no.st.cl, collapse="+")
    lhs <- rownames(tt.attr$factors)[1]
    nf <- as.formula(paste(paste(lhs), paste("~"), rhs)) #new formula for terms
#####extract strata
    mf1 <- model.frame(tt, data = dta, na.action = na.pass)

    if(!is.null(tt.attr$specials$strata)){
      stratas <- mf1[complete.cases(mf1),tt.attr$specials$strata]
      st <- na.omit(pmatch(names(mc), as.character("strata")))
      if (length(st>0))
	strata <- mc[["strata"]]
      else
	strata <- mode(stratas)
    }
    else
      strata <- NULL
    
    tt <- terms(nf)
    tt.attr <- attributes(tt)
  }
  else
    strata <- NULL
  #################################################

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
  data <- dta[complete.cases(mf), names(dta)%in%vars, drop=FALSE]
  if (cond) {
      stop("conditional prediction not supported for coxph models")
  }
  else if (!is.null(fn)) {
    if (is.null(fn$numeric) || !is.function(fn$numeric)) {
      warning("fn$numeric coerced to mean().")
      fn$numeric <- mean
    }
    if (is.null(fn$ordered) || !is.function(fn$ordered) || 
        identical(mean, fn$ordered)) {
      warning("fn$ordered coreced to median().")
      fn$ordered <- median
    }
    else if (identical(min.default, fn$ordered)) 
      fn$ordered <- min
    else if (identical(max.default, fn$ordered)) 
      fn$ordered <- max
    else if (identical(median.default, fn$ordered)) 
      fn$ordered <- median			
    if (is.null(fn$other) || !is.function(fn$other)) { 
      warning("the only available fn for other is mode.")
      fn$other <- mode
    }
    for (i in 1:ncol(data)) {
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
    maxl <- 1
  } else {
    maxl <- nrow(data)
  }
  opt <- vars[na.omit(pmatch(names(mc), vars))]
  if (length(opt) > 0)
    for (i in 1:length(opt)) {
      value <- eval(mc[[opt[i]]], envir = env)
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
  
  X <- as.data.frame(model.matrix(tt, data = data))[-1] #delete cluster
  if(!is.null(strata))
      X <- as.data.frame(cbind(X, strata))
  class(X) <- c("data.frame", "coxph")
  
  return(X)
}

