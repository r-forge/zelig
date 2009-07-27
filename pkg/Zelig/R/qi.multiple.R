qi.multiple <- function(object, simpar, x, x1 = NULL, y = NULL) {
 
  check <- FALSE
  model <- getzelig(object)
  coef<-list()
  tt<-terms(object)
  nreq<-length(tt)
  nms<-names(tt)
#print(colnames(simpar))
  start<-1
  for(i in 1:nreq){
    eqni<-nms[[i]]
    coef[[i]]<-simpar[,start:(start+length(attr(tt,"term.labels")[[eqni]])-1)]
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
      eqn<-nms[[i]]
      #print("from qi.multiple")
      #print(eqn)
      r[[i]]= x[,attr(tt,"term.labels")[[eqn]],drop=FALSE]
      #print(r[[i]])
  
      #  #print("yes")
        eta[[i]] <- coef[[i]] %*% t(r[[i]])
      #print("etai is calculated")
      if(nrow(r[[i]])==1){
        q[,i] <- eta[[i]]
      }
      else
        {
        #  print("coefi\n")
        #  print(coef[[i]])
       #   eta[[i]] <- coef[[i]] %*% (r[[i]])
       #   print("etai\n")
       #   print(eta[[i]])
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






