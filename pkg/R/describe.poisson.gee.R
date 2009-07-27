describe.poisson.gee<-function(){
  category <- "count"
  description  <- "General Estimating Equation for Poisson Regression"
  authors <- c("Patrick Lam")
  year <- 2007
  package <- list(name ="gee",
		version	="4.13-12"
		)
  lambda <- list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
  list(category = category, authors = authors, year = year,description=description,package=package,parameters=list(lambda=lambda))
}

