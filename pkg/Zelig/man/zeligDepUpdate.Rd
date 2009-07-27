\name{zeligDepUpdate}
\alias{zeligDepUpdate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Download Zelig Dependencies Packages
}
\description{
  Compares the packages in Zelig-matrix of dependencies  to the
  locally installed packages. Finds local packages that have lower
  versions than in the corresponding row of the Zelig-matrix.
  Also, finds packages that \pkg{Zelig} required but are not
  installed locally.
  Downloads packages that are locally installed with lower versions and
  those from the Zelig-matrix that are not installed.
  The download repository is taken from either the default
  \code{\var{repos}} argument or from the \sQuote{URL} column of
  the Zelig-matrix of dependencies.        
}

\usage{
zeligDepUpdate(destdir = NULL, installWithVers = FALSE, lib.loc = NULL,
repos = "http://cran.r-project.org")
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{destdir}{directory to store the compress source-codes of
    packages that are downloaded from web repositories.}
  \item{installWithVers}{if TRUE, will invoke the install of the
    package such that it can be referenced by package version.}
  \item{lib.loc}{character vector describing the location of R
    library trees to search through (and update packages therein). }
  \item{repos}{character vector, the base URL(s) of the repositories to
    use, i.e. the URL of the CRAN master such as
    "http://cran.r-project.org", which is the default, or its Statlib
    mirror, "http://lib.stat.cmu.edu/R/CRAN". Can be NULL to install
    from local zip files.}
}

\value{
  No return value. 
}

\references{
  King, Gary. Zelig: Everyones Statistical Software. 
  \url{http://gking.harvard.edu/zelig}.
}
\author{
  Ferdinand Alimadhi and Elena Villalon
}
\seealso{
  \code{\link{zeligDepStatus}}
  \code{\link{install.packages}}
  
}
\note{
  Installs first level dependencies packages of \pkg{Zelig} using R function
  \code{install.packages} with the variable \code{\var{dependencies}} set
  equal to TRUE.  If the installed packages have versions higher than the
 corresponding entry in Zelig-matrix, they are reported with a
 message. If the R version in the local environment is different from
 the R version that \pkg{Zelig} depends on, then, it is reported with a message and no futher action is taken.
}

\examples{
##checks all libraries curently know for packages
\dontrun{zeligDepUpdate()}
##finds packages only in lib.loc
\dontrun{zeligDepUpdate(lib.loc="~/.R/mylibrary")}
}
\keyword{documentation}

