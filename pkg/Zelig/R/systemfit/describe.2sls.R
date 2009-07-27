describe.2sls<-function(){
category <- "continuous"
description  <- "Two Stage Least Squares"
package <-list(	name 	="systemfit",
		version	="0.8",
		)
parameters<-list(mu="mu", inst="inst")
parameters$mu<-list(equations=c(2,Inf),
			tagsAllowed=TRUE,
			depVar=TRUE,
			expVar=TRUE)
			
parameters$inst<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=FALSE,
			expVar=TRUE)
list(category=category,description=description,package=package,parameters=parameters)
}
