describe.mlogit.bayes<-function(){
category <- "multinomial"
description  <- "Bayesian Multinomial Logistic Regression for Dependent Variables with Unordered Categorical Values"
authors <- c("Ben Goodrich", "Ying Lu")
year <- 2007
package <-list(	name 	="MCMCpack",
		version	="0.6"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE,
			specialFunction="as.factor",
			varInSpecialFunction=c(1,1)
		)
			
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
