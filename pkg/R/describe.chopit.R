describe.chopit <- function(){
  category <- "ordinal"
  description  <- "Compound Hierarchical Ordinal Probit Regression for Survey Vignettes"
  authors <- c()
  year <- 2007
  
  package <-list(name    = "anchors",
                 version = "2.0",
                 CRAN    = "http://wand.stanford.edu/R/CRAN")

  parameters <- list()
  parameters$self <- list(equations = c(1,1),
                          tagsAllowed = FALSE,
                          depVar = TRUE,
                          expVar = TRUE)

  parameters$vign <- list(equations = c(1,1),
                          tagsAllowed = FALSE,
                          depVar = TRUE,
                          expVar = FALSE)

  parameters$tau <- list(equations = c(1,1),
                         tagsAllowed = FALSE,
                         depVar = FALSE,
                         expVar = TRUE)

  list(category = category, authors = authors, year = year,
       description = description,
       package = package,
       parameters = parameters)
}
