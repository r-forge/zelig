\name{network}
\alias{network}

\title{Format matricies into a data frame for social network analysis}
\description{ This function accepts individual matricies as its inputs,
combining the input matricies into a single data frame which can then be
used in the \code{data} argument for social network analysis (models
\code{"netlm"} and \code{"netlogit"}) in Zelig.}

\usage{
network(...)
}

\arguments{
  \item{...}{matricies representing variables, with rows and columns corresponding to
    individuals.  These can be given as named arguments and should be
    given in the order the in which the user wishes them to appear in
    the output data frame.}
}

\value{ The \code{network} function creates a data frame which
  contains matricies instead of vectors as its variables.  Inputs to the
  function should all be square matricies and can be given as named
  arguments.}


\examples{\dontrun{
## Let Var1, Var2, Var3, Var4, and Var5 be matrices
friendship <- network(Var1, Var2, Var3, Var4, Var5)
}
}

\seealso{ The full Zelig manual is available at
  \url{http://gking.harvard.edu/zelig}.
}

\author{Skyler J. Cranmer}

\keyword{methods}
