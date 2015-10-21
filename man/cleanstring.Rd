\name{cleanstring}
\alias{cleanstring}
\title{Extract variable name from formula format}
\description{
  Extracts the variable name from the various formats found in model terms, such
  as \code{s(x)} in a \code{\link{gam}} model, or \code{I(x^n)} in any basis
  expansion. Assumes that the variable names will not contain brackets or the 
  exponentiation operator.
}
\usage{
cleanstring(string)
}
\arguments{
  \item{string}{ of type character}
}
\value{
  A character string.
}
\seealso{
 \code{\link{getvarnames}}
}
\examples{
cleanstring("s(x)")
cleanstring("I(x^2)")
}
\keyword{cleanstring}