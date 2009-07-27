plot.surv <- function(x,duration,censor,type="line",
plotcensor=TRUE,plottimes=FALSE,int=c(0.025,0.975),...) {	
	
		s.out <- x
		nobj <- length(s.out)
		for (s in 1:nobj) {
		if (s==1) {
			survival <- s.out[[s]]$qi$survival
			survest <- c(1,apply(survival,2,mean))
	
			survest.lb <- apply(survival,2,function(X){quantile(X,int[1])})
	
			survest.ub <- apply(survival,2,function(X){quantile(X,int[2])})
	
			times <- as.numeric(colnames(survival))
	
			#plot(stepfun(times,survest),xlim=c(0,max(duration)),xlab=xlab,ylab=ylab,main=title)
			plot(times,survest[-1],type="n",xlim=c(0,max(duration)),
					...)
			ntimes <- length(times)-1
			for (t in 1:ntimes){
				u <- survest.ub[t]
				l <- survest.lb[t]
			if (type=="poly") {
				x <- c(times[t],times[t+1],times[t+1],times[t])
				y <- c(survest.ub[t],survest.ub[t+1],survest.lb[t+1],survest.lb[t])
				polygon(x,y,density=100,col="grey")
				}
			if (type=="line") {
					segments(times[t],u,times[t],l,col="grey",lwd=.5)
				}
			}
			if (type=="line"){
					t <- length(times)
					u <- survest.ub[t]
					l <- survest.lb[t]
					segments(times[t],u,times[t],l,col="grey",lwd=.5)
			}	

		}
		if (s>1){
			survival <- s.out[[s]]$qi$survival
			survest <- c(1,apply(survival,2,mean))
	
			survest.lb <- apply(survival,2,function(X){quantile(X,int[1])})
	
			survest.ub <- apply(survival,2,function(X){quantile(X,int[2])})
	
			times <- as.numeric(colnames(survival))
	
			#lines(times,survest[-1],type="s")
	
			ntimes <- length(times)-1
			for (t in 1:ntimes){
				u <- survest.ub[t]
				l <- survest.lb[t]
				
				if (type=="poly") {
				x <- c(times[t],times[t+1],times[t+1],times[t])
				y <- c(survest.ub[t],survest.ub[t+1],survest.lb[t+1],survest.lb[t])
				polygon(x,y,density=100,col="grey")
				}
				if (type=="line") {
					segments(times[t],u,times[t],l,col="grey",lwd=.5)
				}

			}
			if (type=="line"){
					t <- length(times)
					u <- survest.ub[t]
					l <- survest.lb[t]
					segments(times[t],u,times[t],l,col="grey",lwd=.5)
			}	

			
			}

				
		
	}
	
	for (s in 1:nobj) {
			survival <- s.out[[s]]$qi$survival
			survest <- c(1,apply(survival,2,mean))
	
			survest.lb <- apply(survival,2,function(X){quantile(X,0.025)})
	
			survest.ub <- apply(survival,2,function(X){quantile(X,0.975)})
	
			times <- as.numeric(colnames(survival))			
			if (plottimes){lines(stepfun(times,survest),lty=s)}
			else {lines(times,survest[-1],type="s",lty=s)}
			}
		if(plotcensor){
				rug(duration[censor==0])
			}
	}



