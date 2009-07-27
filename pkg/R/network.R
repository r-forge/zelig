network <- function(...){
	cl <- match.call()
	datanames <- list()
	for(i in 2: length(cl)){ 
		datanames[[i-1]] <- cl[[i]]
		} 
	newdata <- list(...)
	names(newdata) <- datanames
	for(i in 1: length(newdata)){
			newdata[[i]] <- as.matrix(newdata[[i]])
			if(any(dimnames(newdata[[i]])[[2]] == "V1")){
				dimnames(newdata[[i]]) <- list(NULL, NULL)
				}
			}	
	class(newdata) <- "data.frame"
	return(newdata)	
	}
