describe.gamma.mixed <- function(){
  category <- "bounded"
  description  <- "Mixed effects gamma model"
  authors <- c("Delia Bailey","Ferdinand Alimadhi")
  year <- 2007
  
  parameters <- list(mu="mu", delta="delta", sigma2="sigma2")

  parameters$mu <- list(equations=c(1,1),
                        tagsAllowed=TRUE,
                        depVar=TRUE,
                        expVar=TRUE)

  parameters$delta <- list(equations=c(1,2),
                           tagsAllowed=TRUE,
                           depVar=FALSE,
                           expVar=TRUE)

  parameters$sigma2 <- list(equations=c(1,1),
                            tagsAllowed=FALSE,
                            depVar=FALSE,
                            expVar=FALSE)

  # Does Zelig need all dependencies here?
  # e.g., lme4 depends on Matrix and lattice
  package <- list(name="lme4", version="0.99875-9")
  

  list(category = category, authors = authors, year = year, description = description, package=package, parameters=parameters)
}

  
