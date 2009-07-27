describe.poisson.bayes<-function(){
category <- "count"
description  <- "Bayesian Poisson Regression"
authors <- c("Ben Goodrich", "Ying Lu")
year <- 2007
package <-list(	name 	="MCMCpack",
		version	="0.6"
		)
parameters<-list(lambda="lambda")
parameters$lambda<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
