% Generated by roxygen2 (4.0.2): do not edit by hand
\name{tuplicated}
\alias{tuplicated}
\title{Find Elements That Are Repeated At Least n Times}
\usage{
tuplicated(x, n, ..., fromLast = FALSE)
}
\arguments{
\item{x}{A vector.}

\item{n}{An integer.}

\item{...}{Other optional arguments are ignored.}

\item{fromLast}{A logical indicating if n-replication should be considered
from the right side of the vector.  If \code{TRUE},
the \code{n-1} last (or rightmost) of replicated
identical elements return \code{FALSE}.}
}
\description{
Finds elements that are repeated at least n times in a vector.
}
\details{
Returns a logical vector that is \code{TRUE} when it runs into
  any but the \code{(n-1)}-st occurrences of an element, analogous
  to \code{\link[base]{duplicated}}.
}
\examples{
x <- c(NA, 1:3, 4:5, rep(6, 6), 3, NA, 4, 3, 3)
all(tuplicated(x, 3) == triplicated(x))
}
\seealso{
\code{\link[base]{duplicated}}
}
\keyword{match}
\keyword{n-replicate,}
\keyword{n-replicated,}
\keyword{repeat,}
\keyword{repeated,}

