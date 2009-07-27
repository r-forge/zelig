\name{model.end}
\alias{model.end}
\title{Cleaning up after optimization}

\description{ The \code{model.end} function creates a list of regression output from \code{\link{optim}} 
output.  The list includes coefficients (from the \code{\link{optim}} \code{par} output), a 
variance-covariance matrix (from the \code{\link{optim}} Hessian output), and any terms, contrasts, or 
xlevels (from the model frame).  Use \code{model.end} after calling \code{\link{optim}}, but before 
assigning a 
class to the regression output.}

\usage{
model.end(res, mf)
}

\arguments{
\item{res}{the output from \code{\link{optim}} or another fitting-algorithm}
\item{mf}{the model frame output by \code{\link{model.frame}}}
}

\value{
A list of regression output, including: 
\item{coefficients}{the optimized parameters}
\item{variance}{the variance-covariance matrix (the negative
  inverse of the Hessian matrix returned from the optimization
  procedure)}  
\item{terms}{the terms object.  See \code{\link{terms.object}}
  for more information}
\item{\dots}{additional elements passed from \code{res}}
}

\seealso{The full Zelig manual at \url{http://gking.harvard.edu/zelig} for examples.}

\author{
  Kosuke Imai <\email{kimai@princeton.edu}>; Gary King
  <\email{king@harvard.edu}>; Olivia Lau <\email{olau@fas.harvard.edu}>; Ferdinand Alimadhi
<\email{falimadhi@iq.harvard.edu}>
}

\keyword{utilities}
