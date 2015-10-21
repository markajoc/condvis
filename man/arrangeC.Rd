\name{arrangeC}
\alias{arrangeC}

\title{
  Make a list of variable pairings for conditional plots produced by \code{plotxc}
}
\description{
  This function attempts to arrange a number of predictors in pairs, which are
  ordered by their bivariate dependencies. The bivariate dependencies are rated
  using \code{\link{savingby2d}}.
}
\usage{
arrangeC(data)
}
\arguments{
  \item{data}{ a \code{data.frame} containing numeric values or factors.}
}
\value{
A list containing character vectors, representing an arrangement of the variable
names in \code{data}.
}
\author{
Mark O'Connell
}
\seealso{
\code{\link{savingby2d}}
}
\examples{
data(mtcars)
arrangeC(mtcars)
data(wine)
arrangeC(wine)
data(pollution)
arrangeC(pollution)
}

\keyword{ arrangeC }
