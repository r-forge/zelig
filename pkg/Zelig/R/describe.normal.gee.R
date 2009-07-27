describe.normal.gee<-function(){
  category <- "continuous"
  description  <- "General Estimating Equation for Normal Regression"
  authors <- c("Patrick Lam")
  year <- 2007
  package <- list(name ="gee",
		version	="4.13-12"
		)
  mu <- list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
  list(category = category, authors = authors, year = year,description=description,package=package,parameters=list(mu=mu))
}

