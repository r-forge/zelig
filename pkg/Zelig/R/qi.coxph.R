qi.coxph <- function(object, simpar, x, x1 = NULL, y = NULL) {
  #model <- getzelig(object)  	
  num <- nrow(simpar)
  if(!any(class(object) == "MI"))
    k <- length(getcoef(object))
  else
    k <- length(getcoef(object[[1]]))
  #xnames <- colnames(x)[1:k]
  
## Strata
  clean.strata <-function(aStr){        #fix strata string
     aStr <- gsub('[[:space:]]', '', aStr)
     ge <- unlist(strsplit(aStr, split=">="))
     le <- unlist(strsplit(aStr, split="<="))
     g <- unlist(strsplit(aStr, split=">"))
     l <- unlist(strsplit(aStr, split="<"))
     e <- unlist(strsplit(aStr, split="="))
     split <- equal <- NULL
     if(length(ge)>1)
       split <- ">="
     if(length(le)>1)
       split <- "<="
     if(length(g)>1 & length(ge)==1)
       split <- ">"
     if(length(l)>1 & length(le)==1)
       split <- "<"
     if(is.null(split)){
       split <- "="
       equal <- 1
     }
     res <- unlist(strsplit(aStr, split="=|>|<|>=|<="))
     if(!is.null(equal)){
       string <- paste(res[[1]],split,res[[2]], sep="")
     }
     else{
       string <- paste(res[[1]], " ", split, " ", res[[2]], "=",
                       res[[3]], sep="")
     }
     return(string)
  }

  if(length(x)>k){
	strata <- as.matrix(x[k+1])
        strata <- clean.strata(strata)
	x <- x[1:k]
  }
  else
	strata <- NULL
  if(!is.null(x1) & length(x1)>k){
	strata1 <- as.matrix(x1[k+1])
        strata1 <- clean.strata(strata1)
	x1 <- x1[1:k]
  }
  else
	strata1 <- NULL
  if(!is.null(x1) & !identical(strata, strata1))
	stop("Strata must be identical for x and x1")    

  #if(!is.null(x)){
   # x <- matrix(as.numeric(x), nrow=1)
   # colnames(x) <- xnames
  #}
  #if(!is.null(x1)){
   # x1 <- matrix(as.numeric(x1), nrow=1)
   # colnames(x1) <- xnames
  #}
  
  if (!is.null(x))
      x.mat <- as.matrix(x)
  if (!is.null(x1))
      x1.mat <- as.matrix(x1)

  
  coef <- as.matrix(simpar[,1:k])
  eta <- coef%*%t(x.mat)
  risk <- exp(eta)
  qi <- qi.name <- list()


## Rate Ratio
  if(!is.null(x1)){
	eta1 <- coef%*%t(x1.mat)
	risk1 <- exp(eta1)
	qi$hr <- risk1/risk
	qi.name$hr <- "Hazard Ratios: h(t|X1)/h(t|X)"
  }

## Survival Function
## Not MI data
  if(!any(class(object) == "MI")){ 
    surv.fit <- survfit(object, newdata=x)
  
    if(!is.null(strata)){			
      index <- which(match(summary(surv.fit)$strata, strata) == 1)
      surv <- surv.fit$surv[index]
      time <- surv.fit$time[index]
      surv.se <- summary(surv.fit)$std.err[index]
      log.surv <- log(surv)
      log.surv.se <- surv.fit$std.err[index]
    }
    else{
      surv <- surv.fit$surv
      time <- surv.fit$time
      surv.se <- summary(surv.fit)$std.err
      log.surv <- log(surv)
      log.surv.se <- surv.fit$std.err
    }
  }

## MI data

  else{
    m <- length(object)

    surv.fit <- survfit(object[[1]], newdata=x)
    
    if(!is.null(strata)){			
      index <- which(match(summary(surv.fit)$strata, strata) == 1)
      surv <- surv.fit$surv[index]
      time <- surv.fit$time[index]
    }
    else{
      surv <- surv.fit$surv
      time <- surv.fit$time
    }
    means <- cbind(time, "1"=surv)

    #Merge data by survival time
    for(i in 2:m){
      surv.fit <- survfit(object[[i]], newdata=x)

      if(!is.null(strata)){			
        index <- which(match(summary(surv.fit)$strata, strata) == 1)
        surv <- surv.fit$surv[index]
        time <- surv.fit$time[index]
      }
      else{
        surv <- surv.fit$surv
        time <- surv.fit$time
      }
      
      new.means <- cbind(time,surv)
      colnames(new.means) <- c("time",paste(i))
      means <- merge(means, new.means, by="time", all = T)
    }
    means <- means[,-1]
    surv <- rowMeans(means, na.rm = T) #survival means
    na <- apply(means, 2, is.na) 
    na <- 1 - apply(na, 2, as.numeric)
    n <- apply(na, 1, sum) #number of non-NA per survival time
    if(any(n==1)){
      warning("Some imputed survival times appear in only one
dataset.  Suggest increasing number of imputed datasets and/or specify
duration variable as ordinal")
      n[which(n==1)] <- n[which(n==1)]+1
    }

    surv.fit <- survfit(object[[1]], newdata=x)

    if(!is.null(strata)){			
      index <- which(match(summary(surv.fit)$strata, strata) == 1)
      surv.se <- summary(surv.fit)$std.err[index]
      time <- surv.fit$time[index]
    }
    else{
      surv.se <- summary(surv.fit)$std.err
      time <- surv.fit$time
    }
    se <- cbind(time, "1"=surv.se)
    for(i in 2:m){
      surv.fit <- survfit(object[[i]], newdata=x)

      if(!is.null(strata)){			
        index <- which(match(summary(surv.fit)$strata, strata) == 1)
        surv.se <- summary(surv.fit)$std.err[index]
        time <- surv.fit$time[index]
      }
      else{
        surv.se <- summary(surv.fit)$std.err
        time <- surv.fit$time
      }
      new.se <- cbind(time,surv.se)
      colnames(new.se) <- c("time",paste(i))
      se <- merge(se, new.se, by="time", all = T)
    }
    
    time <- se[,1]
    t <- length(t)
    se <- se[,-1]
    var <- se^2
    surv.se <- c()
    mean.var <- rowMeans(var, na.rm = T)

    #Rubin's rule
    
    B <- (means-surv)^2
    B[is.na(B)] <- 0
    B <- apply(B,1,sum)/(n-1)
    surv.se <- sqrt(mean.var + B*(1+1/n))

    log.surv <- log(surv)
    log.surv.se <- sqrt(1/(surv^2) * surv.se^2) #delta method
    
  }

  surv.sims <- matrix(NA, nrow=num, ncol=length(surv))
      for (i in 1:length(surv)){
	surv.sims[,i] <- exp(rnorm(num, mean=log.surv[i], sd=log.surv.se[i]))
      }
  colnames(surv.sims) <- time 
  
  qi$survival <- surv.sims
  qi.name$survival <- "Estimated Survival Function Over Time: S(t|X)"

## Cumulative Hazard
  qi$cumhaz <- cumhaz <- -log(surv.sims)
  qi.name$cumhaz <- "Estimated Cumulative Hazard Over Time: H(t|X)"

## Hazard
  cumhaz.means <- colMeans(cumhaz)
  hazard <- matrix(NA, ncol=ncol(cumhaz), nrow=1)
  colnames(hazard) <- colnames(cumhaz)
  hazard[,1] <- cumhaz.means[1]
  for (i in 2:length(time)){
	hazard[,i] <- cumhaz.means[i] - cumhaz.means[i-1]
  }
  qi$hazard <- hazard
  qi.name$hazard <- "Estimated Hazard Rate Over Time: h(t|X)"
  class(qi$hazard) <- "coxhazard"
  

  list(qi=qi, qi.name=qi.name)
}  








