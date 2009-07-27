qi.zmlm <- function(object, simpar, x, x1 = NULL, y = NULL) {
  
  k <- length(getcoef(object))
  coef <- simpar[,1:k]
  alpha <- simpar[,(k+1):ncol(simpar)]
  if(length(dim(coef(object))))
    rw <- rownames(coef(object))
  else
    rw <- names(coef(object))
  
  
  if (k < ncol(x))
    x <- as.data.frame(x[,rw,drop = FALSE])
  dx <- dim(x)[[2]]
  ##return a list to obtain expected values for each element
  ##that stands for each depVar in mlm or maov
  
  coeflist <- split.simpar(coef, dx)     
  ev <- matrix(,nrow(simpar), length(coeflist))
  colnames(ev) <- names(coeflist)
  for(n in 1:ncol(ev))
    ev[,n] <- coeflist[[n]] %*% t(x)
  qi <- list(ev=ev)
  qi.name <- list(ev="Expected Values: E(Y|X)")
  if(!is.null(x1)){
    if (k < ncol(x1))
      x1 <- as.data.frame(x1[,rw,drop=FALSE])
    dx1 <- dim(x1)[[2]]
    coeflist <- split.simpar(coef, dx1)     
    ev1 <- matrix(,nrow(simpar), length(coeflist))
    colnames(ev1) <- names(coeflist)
    for(n in 1:ncol(ev1))
      ev1[,n] <- coeflist[[n]] %*% t(x1)
    
    qi$fd <- ev1-ev
    qi.name$fd <-
      "First Differences in Expected Values: E(Y|X1)-E(Y|X)"
  }
  if (!is.null(y)) {
    yvar <- matrix(rep(y, nrow(simpar)), nrow = nrow(simpar), byrow = TRUE)
    #tmp.ev <- qi$tt.ev <- apply(as.matrix(qi$ev), 2, function(m)yvar - m)
    tmp.ev <- apply(as.matrix(qi$ev), 2, function(m)yvar - m)
    qi$att.ev <- matrix(apply(tmp.ev, 1, mean), nrow = nrow(simpar))
    #qi.name$tt.ev <- "Unit Treatment Effect for the Treated: Y - EV"
    qi.name$att.ev <- "Average Treatment Effect for the Treated: Y - EV"
  }
  list(qi=qi, qi.name=qi.name)
}



split.simpar <- function(coef, dx){
  ix <- grep("Intercept", colnames(coef))
  nmall <- colnames(coef)[ix]
  vecnm <- NULL
  for(nm in nmall)
    vecnm <- c(vecnm, sub("(.*):(.*)", "\\1",nm))
  
  dc <- dim(coef)[[2]]
  nt <- dc/dx
  lst <- list()
  ptf <- 0
  for(n in 1:nt){
    pti <- ptf + 1 
    ptf <- ifelse(n<= 1, dx, dx +ptf) 
    pc <- coef[,pti:ptf]
    lst <- c(lst, list(pc))
  }
  names(lst) <- vecnm
  return(lst)
}
