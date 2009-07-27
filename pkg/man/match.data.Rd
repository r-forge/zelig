\name{match.data}

\alias{match.data}

\title{Output matched data sets}

\description{The code \code{match.data} creates output data sets from the \code{matchit}
matching algorithm.}

\usage{
match.data <- match.data(object, group = "all")
}

\arguments{ 
\item{object}{Stored output from \code{matchit}.} 

\item{group}{Which units to output.  Selecting "all" (default) gives all
matched units (treated and control), "treat" gives just the matched
treated units, and "control" gives just the matched control units.} }

\value{ The \code{match.data} command generates a matched data set from
the output of the \code{matchit} function, according to the options
selected in the \code{group} argument.  The matched data set contains
the additional variables: 
  \item{pscore}{The propensity score for each unit.}
  \item{psclass}{The subclass index for each unit (if applicable).}
  \item{psweights}{The weight for each unit (generated from the matching
    procedure).}
See the \code{matchit} documentation for more details on these items.
}

\seealso{The complete documentation for \code{matchit} is available online at
  \url{http://gking.harvard.edu/matchit}.}

\author{
  Daniel Ho <\email{deho@fas.harvard.edu}>; Kosuke Imai
  <\email{kimai@princeton.edu}>; Gary King
  <\email{king@harvard.edu}>; Elizabeth Stuart<\email{stuart@stat.harvard.edu}>
}

\keyword{methods}
