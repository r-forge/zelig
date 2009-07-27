\name{zeligDepStatus}
\alias{zeligDepStatus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Zelig Dependencies Packages Client Status}
\description{
  Compares Zelig-matrix of dependencies to the locally installed
  packages. Finds those packages that \pkg{Zelig} depends on and are not
  installed in local environment. Also finds those packages that are
  locally installed but with lower versions than those required in the
  dependencies matrix.
  The Zelig-matrix includes any of dependency fields in the
  \sQuote{DESCRIPTION} files, i.e. depends, imports and suggests,
  for any packages directly derived from \pkg{Zelig} and for any of the models that \pkg{Zelig} supports.   
}
\usage{
zeligDepStatus(lib.loc = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lib.loc}{a character vector of directory names 
   of \code{R} libraries, or NULL.
  The default value NULL corresponds to all libraries currently known. If the default is used, the loaded packages are searched before the libraries. }
}

\value{
  Returns a matrix of packages that are either installed locally 
  with lower versions, or packages not installed but listed in the
  Zelig-matrix of dependencies.  The matrix rows correspond to the packages and 
  the columns contain the following fields
   \item{Package}{names of packages.}
  \item{Version}{versions locally installed}
  \item{Zideal}{versions required in Zelig-matrix of dependencies.}}

\references{King, Gary. Zelig: Everyones Statistical Software. 
  \url{http://gking.harvard.edu/zelig}.}

\author{Ferdinand Alimadhi and Elena Villalon}
\seealso{
\code{\link{installed.packages}}
\code{\link{packageDescription}}
\code{\link{zeligDepUpdate}}
}
\note{
If the R version in the local environment is different from the R
version that \pkg{Zelig}  
depends on, then, it is reported with a message and no futher action is taken. 
If the installed packages have versions higher than the corresponding 
values in the Zelig-matrix, it is reported with a message.}

\examples{
## find packages in all libraries currently installed
\dontrun{zstatus <- zeligDepStatus()}
## find packages only in lib.loc
\dontrun{zstatus <- zeligDepStatus(lib.loc="~/.R/mylibrary")}
}
\keyword{documentation}

