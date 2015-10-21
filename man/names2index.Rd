\name{names2index}
\alias{names2index}

\title{
  Convert variable names to integer indices.
}
\description{
Takes a character vector of variable names, and the variable names from a dataset and
converts the names to appropriate integer indices.
}
\usage{
names2index(C, names)
}
\arguments{
  \item{C}{ a character vector of variable names}
  \item{names}{ a character vector of variable names in the order they appear in the
  appropriate dataset (i.e. as returned by \code{\link{colnames}})}
}
\value{
A vector of integer indices.
}
\author{
Mark O'Connell
}
\examples{
data(mtcars)
names2index(c("mpg", "cyl", "wt"), colnames(mtcars))
}
\keyword{names2index}
