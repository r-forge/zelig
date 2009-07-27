describe.factor.mix<-function(){
category <- "mixed"
description  <- "Mixed Data Factor Analysis"

authors <- c("Ben Goodrich", "Ying Lu")
year <- 2007

package <-list(	name 	="MCMCpack",
		version	="0.6"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=FALSE,
			specialFunction="cbind",
			varInSpecialFunction=c(2,Inf)
		)
			
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
