\name{model.matrix.multiple}
\alias{model.matrix.multiple}
\title{Design matrix for multivariate models}

\description{Use \code{model.matrix.multiple} after \code{\link{parse.formula}} to
create a design matrix for multiple-equation models.  }
  
\usage{
model.matrix.multiple(object, data, shape = "compact", eqn = NULL, ...)
}

\arguments{
\item{object}{the list of formulas output from \code{\link{parse.formula}}}
\item{data}{a data frame created with \code{\link{model.frame.multiple}}} 
\item{shape}{a character string specifying the shape of the outputed matrix.  Available options are 
\itemize{
\item{"compact"}{(default) the output matrix will be an \eqn{n \times v}{n x v},
where \eqn{v} is the number of unique variables in all of the equations
(including the intercept term)}
\item{"array"}{the output is an \eqn{n \times K \times J}{n x K x J} array where \eqn{J} is the
total number of equations and \eqn{K} is the total number of parameters
across all the equations.  If a variable is not in a certain equation,
it is observed as a vector of 0s.}
\item{"stacked"}{the output will be a \eqn{2n \times K}{2n x K} matrix where \eqn{K} is the total number of 
parameters across all the equations.}
}}
\item{eqn}{a character string or a vector of character strings identifying the equations from which to 
construct the design matrix. The defaults to \code{NULL}, which only uses the systematic
  parameters (for which \code{DepVar = TRUE} in the appropriate \code{describe.model} function)}  
\item{\dots}{additional arguments passed to \code{model.matrix.default}} 
}

\value{A design matrix or array, depending on the options chosen in \code{shape}, with appropriate terms 
attributes.} 

\examples{

# Let's say that the name of the model is "bivariate.probit", and
# the corresponding describe function is describe.bivariate.probit(),
# which identifies mu1 and mu2 as systematic components, and an
# ancillary parameter rho, which may be parameterized, but is estimated
# as a scalar by default.  Let par be the parameter vector (including
# parameters for rho), formulae a user-specified formula, and mydata
# the user specified data frame.

# Acceptable combinations of parse.par() and model.matrix() are as follows:
## Setting up
\dontrun{ 
data(sanction)
formulae <- cbind(import, export) ~ coop + cost + target
fml <- parse.formula(formulae, model = "bivariate.probit")
D <- model.frame(fml, data = sanction)
terms <- attr(D, "terms")

## Intuitive option
Beta <- parse.par(par, terms, shape = "vector", eqn = c("mu1", "mu2"))
X <- model.matrix(fml, data = D, shape = "stacked", eqn = c("mu1", "mu2")  
eta <- X %*% Beta

## Memory-efficient (compact) option (default)
Beta <- parse.par(par, terms, eqn = c("mu1", "mu2"))
X <- model.matrix(fml, data = D, eqn = c("mu1", "mu2"))   
eta <- X %*% Beta

## Computationally-efficient (array) option
Beta <- parse.par(par, terms, shape = "vector", eqn = c("mu1", "mu2"))
X <- model.matrix(fml, data = D, shape = "array", eqn = c("mu1", "mu2"))
eta <- apply(X, 3, '%*%', Beta)
}}

\seealso{\code{\link{parse.par}}, \code{\link{parse.formula}} and the full Zelig manual at
  \url{http://gking.harvard.edu/zelig}}

\author{
  Kosuke Imai <\email{kimai@princeton.edu}>; Gary King
  <\email{king@harvard.edu}>; Olivia Lau <\email{olau@fas.harvard.edu}>; Ferdinand Alimadhi
<\email{falimadhi@iq.harvard.edu}>
}

\keyword{utilities}











