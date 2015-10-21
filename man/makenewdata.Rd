\name{makenewdata}
\alias{makenewdata}
\title{Create a data set as a section through predictor space}
\description{
  Creates a data set which represents a partial conditioning, or a section through the
  predictor space.
}
\usage{
makenewdata(xs, xc.cond)
}
\arguments{
  \item{xs}{ a \code{data.frame} with one or two columns, representing a 1-d or 2-d
    grid of values at which to evaluate fitted model values.}
  \item{xc.cond}{ a \code{data.frame} with one row, to be repeated.}
}
\value{
A \code{data.frame}.
}
\author{Mark O'Connell}
\seealso{
  \code{\link{ceplot}},  \code{\link{plotxs}}.
}
\examples{
## Create section across all 'mpg' values holding all else constant at their
## values from the first observation.

data(mtcars)
section <- makenewdata(mtcars[, 1, drop = FALSE], mtcars[1, -1])
print(section)
}
\keyword{makenewdata}
