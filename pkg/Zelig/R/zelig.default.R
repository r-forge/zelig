zelig.default <- function(formula, model, data, by = NULL, save.data =
                          FALSE, cite = TRUE, ...) {

  fn1 <- paste("zelig2", model, sep = "")
  fn2 <- paste("zelig3", model, sep = "")

  if (!exists(fn1))
    stop(model,
         " not supported. Type help.zelig(\"models\") to list supported models.")

  ## load model dependeny pakcages
  loadModelDeps(model)
  
  mf <- zelig.call <- match.call(expand.dots = TRUE)
  zelig.call[[1]] <- as.name("zelig")
  if (missing(by))
    by <- NULL
  N <- M <- 1
  object <- list()
  if ("mi" %in% class(data))
    M <- length(data)
  if (M > 1)
    dat <- data[[1]]
  else
    dat <- data
  if (!is.null(by)) {
          if (any(as.character(by) %in% c(formula[[2]], formula[[3]])))
            stop("the variable selected for subsetting cannot be called in the formula.")
          idx <- dat[,by]
          mf$by <- NULL
          lev <- sort(unique(idx))
          N <- length(lev)
  }
  
  ## call zelig2* function
  mf <- do.call(fn1, list(formula, model, dat, N, ...))
    
  for (i in 1:N) {
          if (N > 1) {
                  dat <- list()
                  if (M > 1) {
                          for (j in 1:M)
                            dat[[j]] <- data[[j]][idx == lev[i],]
                  } else {
                          dat <- data[idx == lev[i],]
                  }
          } else {
                  dat <- data
          }
          obj <- list()
          for (j in 1:M) {
            if (M > 1)
              d <- dat[[j]]
            else
              d <- dat
            if (is.data.frame(d)) {
              ## List-wise deletion on d performed on the
              ## rows which have NAs only for the variables which appear in
              ## formula.
              d <- d[complete.cases(d[,all.vars(as.expression(formula))]),]
              mf$data <- d
            }
            ## evaluate the call returned by zelig2* function
            ## (which is basically a call to the foreign model
            res <- eval(as.call(mf))
            
            ## if zelig3* exists (i.e. we want to manipulate the output coming
            ## from the foreign model) then call it
            if (exists(fn2)) 
              res <- do.call(fn2, list(res = res, fcall = mf,
                                       zcall = as.list(zelig.call)))
            
            ## check the class of the object (S3/S4)
            ##check <- length(slotNames(res)) > 0
            check <- isS4(res)
            if (check) {                             #S4
              if ("call" %in% slotNames(res))
                res@call <- as.call(zelig.call)
              else
                stop("no slot \"call\" in the result")
              
              ## if is S4 and "model" slot found, save data in that slot
              ## otherwise, we cannot save the data
              if (save.data){
                if ("model" %in% slotNames(res))
                  res@model <- as.data.frame(d)
                else
                  stop("\"save.data\" option is not supported for this model")
              }
            } else {                                 #S3
              res$call <- as.call(zelig.call)
              if (save.data) res$zelig.data <- as.data.frame(d)
            }
            ##        res$zelig <- model
            if (M > 1) 
              obj[[j]] <- res
            else
              obj <- res
          }
          if (M > 1) 
            class(obj) <- "MI"
          if (N > 1) 
            object[[i]] <- obj
          else
            object <- obj
        }
  if (N > 1) {
    class(object) <- "strata"
    names(object) <- lev
  }
  if (cite){
    cat(getModelCitation(model))
  }
  return(object)
}

###
## loadModelDeps
##
## For a given model, tries to load the package it depends on
## model : model name
##

loadModelDeps <- function(model){
        zdpd <- Zelig:::zeligModelDependency(model)
        
        ## e.g. describe does not exist 
        if(!is.null(zdpd)){
                pkg <- zdpd[1,1]   ## what if more then 1 dependency?!
                if (!is.na(pkg)) {
                        if (!suppressWarnings(require(pkg,quietly=TRUE,character.only=TRUE)))
                          stop("This model depends on package \"",pkg,"\". Please install this package and try again")
                }
        }
}
