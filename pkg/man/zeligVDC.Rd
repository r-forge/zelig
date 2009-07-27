\name{zeligDescribeModelXML}
\alias{zeligDescribeModelXML}
\alias{zeligInstalledModels}
\alias{zeligListModels}
\alias{zeligModelDependency}
\alias{zeligGetSpecial}
\title{ Zelig interface functions}
\description{
	Zelig interface functions. Used by VDC DSB to  communicate with Zelig.
}
\usage{
	zeligDescribeModelXML(modelName,force=FALSE,schemaVersion="1.1")
	zeligInstalledModels(inZeligOnly=TRUE,schemaVersion="1.1")
	zeligListModels(inZeligOnly=TRUE) 
	zeligModelDependency(modelName,repos) 
	zeligGetSpecial(modelName)
}

\arguments{
  \item{modelName}{Name of model as returned by zeligInstalledModels or zeligListModels.}
  \item{inZeligOnly}{Flag, include only models in official Zelig distribution}
  \item{repos}{URL of default repository to use}
  \item{schemaVersion}{version of Zelig schema}
  \item{force}{generate a description even if no custom description supplied}
}

\value{
Use zeligInstalledModels and zeligListModels to determine what models are available in zelig
for a particular schema level. Use zmodel2string(zeligDescribeModel()) to generate an XML
instance describing a model. Use zeligModelDependencies to generate a list of package
dependencies for models. Use zeligGetSpecial to get the name special function, if any,
to apply to the outcome variables. All functions return NULL if results are
not available for that model.
}

\examples{\dontrun{
	# show all available models
	zeligListModels(inZeligOnly=FALSE)
	# show installed models
	zeligInstalledModels()
	# show dependency for normal.bayes
	zeligModelDependency("normal.bayes","http://cran.r-project.org/")
	# description of logit
	cat(zeligDescribeModelXML("ologit"))
	# special function for factor analysis
 	zeligGetSpecial("factor.mix")
}
\dontshow{
\dontrun{

# test model lists
zd= zeligInstalledModels(schemaVersion="1.1")
if (length(zd)<8 || sum(zd=="ls")!=1   || length(zeligListModels())<25 ) {
	stop("Failed zeligListModels/zeligInstalledModels self test")
}

if (zeligModelDependency("poisson.bayes","")[1]!="MCMCpack") {
	stop("Failed zeligModelDependency self test")
}

if (zeligGetSpecial("factor.mix")!="cbind") {
	stop("Failed zeligGetSpecial self test")
}

if (grep("explanatory",  zeligDescribeModelXML("ologit"))!=1) {
	stop("Failed zmodel2string/zeligDescribeModel self test")
}
}
}

}

\author{
Micah Altman
\email{thedata-users\@lists.sourceforge.net}
\url{http://thedata.org}
}


\seealso{ \link[pkg:Zelig]{zelig}}

\keyword{IO}
\keyword{print}
