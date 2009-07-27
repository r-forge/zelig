qi.multiple <- function(object, simpar, x, x1 = NULL, y = NULL) {
 
  check <- FALSE
  model <- object$zelig
  coef<-list()
  om<-attr(object$terms,"omit")
  nreq<-nrow(om)

  start<-1
  for(i in 1:nreq){
    eqni<-paste("eqn",i,sep="")
    coef[[i]]<-simpar[,start:(start+length(attr(x,eqni))-1)]
    start<-start+length(attr(x,eqni))
  }

  fillmatrix<-function(simpar,x,nreq){
    r<-list()
    eta<-list()
    if(nrow(x)==1)
      q<-array(NA,c(nrow(simpar),nreq))
    else
      q<- array(NA,c(nrow(simpar),nreq,nrow(x)))

    for(i in 1:nreq){
      eqn=paste("eqn",i,sep="")
      r[[i]]= as.matrix(x[,attr(x,eqn)])
      if(ncol(r[[i]])==1){
        eta[[i]] <- coef[[i]] %*% r[[i]]  
        q[,i] <- eta[[i]]
      }
      else
        {
          eta[[i]] <- coef[[i]] %*% t(r[[i]])
          q[,i,] <- eta[[i]]
        }
    }
    return (q)

  }
  pr<-ev<-fillmatrix(simpar,x,nreq)

  qi <- list(ev = ev,pr=pr)
  qi.name <- list(ev = "Expected Values: E(Y|X)",pr = "Predicted Values: Y|X")

  
  if (!is.null(x1)){
      theta1<-fillmatrix(simpar,x1,nreq)
      ev1 <- theta1
      qi$fd <- ev1-ev
      qi.name$fd <- "First Differences in Expected Values: E(Y|X1)-E(Y|X)"

  }
  list(qi=qi, qi.name=qi.name)
}






