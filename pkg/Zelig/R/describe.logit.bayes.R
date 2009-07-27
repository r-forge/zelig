describe.logit.bayes<-function(){
category <- "dichotomous"
description  <- "Bayesian Logistic Regression for Dichotomous Dependent Variables"
authors <- c("Ben Goodrich", "Ying Lu")
year <- 2007
package <-list(	name 	="MCMCpack",
		version	="0.6"
		)
parameters<-list(pi="pi")
parameters$pi<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
