\name{model.frame.multiple}
\alias{model.frame.multiple}
\title{Extracting the ``environment'' of a model formula}

\description{ Use \code{model.frame.multiple} after \code{\link{parse.par}} to create a
data frame of the unique variables identified in the formula (or list
of formulas).}
  
\usage{
model.frame.multiple(formula, data, eqn = NULL, ...)
}

\arguments{
\item{formula}{a list of formulas of class \code{"multiple"}, returned from \code{\link{parse.par}}}  
\item{data}{a data frame containing all the variables used in \code{formula}}  
\item{eqn}{an optional character string or vector of character strings specifying 
the equations (specified in \code{describe.mymodel}) for which you would like to 
pull out the relevant variables.}
\item{\dots}{additional arguments passed to 
\code{\link{model.frame.default}}} }  

\value{
The output is a data frame (with a terms attribute) containing all the
unique explanatory and response variables identified in the list of
formulas.  By default, missing (\code{NA}) values are listwise deleted.

If \code{as.factor} appears on the left-hand side, the response
variables will be returned as an indicator (0/1) matrix with columns
corresponding to the unique levels in the factor variable.  
	
If any formula contains more than one \code{tag} statement, \code{model.frame.multiple}
will return the original variable in the data frame and use the \code{tag} information in the terms 
attribute only.
}

\examples{
\dontrun{
data(sanction)
formulae <- list(import ~ coop + cost + target,
                 export ~ coop + cost + target)
fml <- parse.formula(formulae, model = "bivariate.logit")
D <- model.frame(fml, data = sanction)
}}

\seealso{\code{\link{model.matrix.default}}, \code{\link{parse.formula}} and the full Zelig manual at
  \url{http://gking.harvard.edu/zelig}}

\author{
  Kosuke Imai <\email{kimai@princeton.edu}>; Gary King
  <\email{king@harvard.edu}>; Olivia Lau <\email{olau@fas.harvard.edu}>; Ferdinand Alimadhi
<\email{falimadhi@iq.harvard.edu}>
}

\keyword{utilities}









