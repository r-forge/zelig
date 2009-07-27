describe.ei.dynamic <- function(){
category <- "ei"
description  <- "Quinn's Dynamic Ecological Inference Model"
authors <- c("Ben Goodrich", "Ying Lu")
year <- 2007
package <- list (
	name="MCMCpack",
	version="0.8-2"
	)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE,
			specialFunction="cbind",
			varInSpecialFunction=c(2,Inf)
		)
			
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
