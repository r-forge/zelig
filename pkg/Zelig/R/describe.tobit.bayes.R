describe.tobit.bayes<-function(){
category <- "continuous"
description  <- "Bayesian Linear Regression for a Censored Dependent Variable"
authors <- c("Ben Goodrich","Ying Lu")
year <- 2007
package <-list(	name 	="MCMCpack",
		version	="0.6"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
