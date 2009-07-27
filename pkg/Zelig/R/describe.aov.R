describe.aov<-function(){
category <- "continuous"
description  <- "Fit an Analysis of Variance Model"
authors <- c()
year <- 2007
package <-list(	name 	="stats",
		version	="2.5.0"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
                    tagsAllowed=FALSE,
                    depVar=TRUE,
                    expVar=TRUE
                    ##specialFunction="cbind",
                    ##varInSpecialFunction= c(2,Inf)
                    )
			
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
