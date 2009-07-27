\name{gsource}

\alias{gsource}

\title{Read Data As a Space-Delimited Table}

\description{
  The \code{gsource} function allows you to read a space delimited table
  as a data frame.  Unlike \code{scan}, you may use \code{gsource} in a
  \code{source}ed file, and unlike \code{read.table}, you may use
  \code{gsource} to include a small (or large) data set in a file that
  also contains other commands.
}

\usage{
gsource(var.names = NULL, variables)
}

\arguments{
  \item{var.names}{An optional vector of character strings representing
    the column names.  By default, \code{var.names = NULL}. }
  \item{variables}{A single character string representing the data.}
}

\value{
  The output from \code{gsource} is a data frame, which you may save to
  an object in your workspace.
}

\seealso{\code{read.table}, \code{scan}}

\author{ Olivia Lau <\email{olau@fas.harvard.edu}>
}

\examples{
\dontrun{
data <- gsource(variables =  "
                 1 2 3 4 5    
                 6 7 8 9 10   
                 3 4 5 1 3    
                 6 7 8 1 9    ")

data <- gsource(var.names = "Vote Age Party", variables = "
                             0    23 Democrat             
                             0    27 Democrat             
			     1    45 Republican           
                             1    65 Democrat             ")
}
}

\keyword{file}
