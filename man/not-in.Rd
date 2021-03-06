% Generated by roxygen2 (4.0.2): do not edit by hand
\name{not-in}
\alias{\%!in\%}
\alias{not-in}
\title{Mismatch Test}
\usage{
x \%!in\% table
}
\arguments{
\item{x}{A vector of data.}

\item{table}{A table of reference values.}
}
\description{
Test whether some data are not in a table.
}
\details{
This helps avoid code structures like \code{!(x \%in\% table)}.
}
\examples{
1:2 \%!in\% 2:4
}
\seealso{
\code{\link[base]{match}}
}
\keyword{arithmetic}
\keyword{match}
\keyword{time,}

