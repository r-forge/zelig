\name{repl}

\alias{repl}
\alias{repl.zelig}
\alias{repl.default}

\title{Replicating Analyses}

\description{ The generic function \code{repl} command takes 
\code{\link{zelig}} or
  \code{\link{sim}} output objects and replicates (literally, re-runs)
  the entire analysis.  The results should be an output 
object
  identical to the original input object in the case of
  \code{\link{zelig}} output.  In the case of \code{\link{sim}}
  output, the replicated analyses may differ slightly due to
  stochastic randomness in the simulation procedure.  }

\usage{
repl(object, data, \dots)
\method{repl}{default}(object, data = NULL, \dots)
\method{repl}{zelig}(object, data = NULL, prev = NULL, x = NULL, x1 = NULL,
     bootfn = NULL, \dots) 
}

\arguments{
\item{object}{Stored output from either \code{\link{zelig}} or
    \code{\link{sim}}.}
\item{data}{You may manually input the data frame name rather
  than allowing \code{repl} to draw the data frame name from the object
  to be replicated.}
\item{prev}{When replicating \code{\link{sim}} output, you may
  optionally use the previously simulated parameters to calculate the
  quantities of interest rather than simulating a new set of
  parameters.  For all models, this should produce identical
  quantities of interest.  In addition, for if the parameters were
  bootstrapped in the original analysis, this will save a considerable
  amount of time. }
\item{x}{When replicating \code{\link{sim}} output, you may
  optionally use an alternative \code{\link{setx}} value for the \code{x}
  input. } 
\item{x1}{When replicating \code{\link{sim}} output, you may
  optionally use an alternative \code{\link{setx}} object for the \code{x1}
  input to replicating the \code{\link{sim}} object. }
\item{bootfn}{When replicating \code{\link{sim}} output with
    bootstrapped parameters, you should manually specify the
    \code{bootfn} if a non-default option was used.  }
\item{\dots}{Additional arguments passed to either \code{\link{zelig}} or 
\code{\link{sim}}.  }
}

\value{
For \code{\link{zelig}} output, \code{repl} will create output that is in
every way identical to the original input.  You may check to see
whether they are identical by using the \code{identical} command.  

For \code{\link{sim}} output, \code{repl} output will be will be
identical to the original object if you choose not to simulate new
parameters, and instead choose to calculate quantities of interest
using the previously simulated parameters (using the \code{prev}
option.  If you choose to simulate new parameters, the summary
statistics for each quantity of interest should be identical, up to a
random approximation error.  As the number of simulations increases,
this error decreases.
}

\seealso{ \code{\link{zelig}}, \code{\link{setx}}, and
  \code{\link{sim}}.  In addition, the full Zelig manual may be
  accessed online at \url{http://gking.harvard.edu/zelig}.  }

\examples{
data(turnout)
z.out <- zelig(vote ~ race + educate, model = "logit", data = turnout[1:1000,])
x.out <- setx(z.out)
s.out <- sim(z.out, x = x.out)
z.rep <- repl(z.out)
identical(z.out$coef, z.rep$coef)
z.alt <- repl(z.out, data = turnout[1001:2000,])
s.rep <- repl(s.out, prev = s.out$par)
identical(s.out$ev, s.rep$ev)
}


\author{
  Kosuke Imai <\email{kimai@princeton.edu}>; Gary King
  <\email{king@harvard.edu}>; Olivia Lau <\email{olau@fas.harvard.edu}>
}

\keyword{file}
