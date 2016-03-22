\name{savingby2d}
\alias{savingby2d}

\title{
  Calculate the reduction in predictor space implied by using a 2-d marginal
  view rather than two 1-d views.
}
\description{
  A simple algorithm to evaluate the proportion of the predictor space which is
  seen to contain data by taking a bivariate marginal view of x and y, rather
  than two univariate marginal views.
}
\usage{
savingby2d(x, y = NULL, method = "default")
}

\arguments{
  \item{x}{ a numeric vector or factor. Can also be a dataframe containing x
  and y.}
  \item{y}{ a numeric vector or factor.}
  \item{method}{ criterion used to quantify bivariate relationships. Can be
    \code{"default"}, a scagnostic measure, or \code{"DECR"} to use a density
    estimate confidence region.}
}
\details{
  If given two continuous variables, the variables are both scaled to mean 0 and
  variance 1. Then the returned value is the ratio of the area of the convex
  hull of the data to the area obtained from the product of the ranges of the
  two areas, i.e. the area of the bounding rectangle.

  If given two categorical variables, all combinations are tabulated.  The
  returned value is the number of non-zero table entries divided by the total
  number of table entries.

  If given one categorical and one continuous variable, the returned value is
  the weighted mean of the range of the continuous variable within each category
  divided by the overall range of the continuous variable, where the weights are
  given by the number of observations in each level of the categorical variable.
}
\value{
  A number between 0 and 1.
}
\author{
Mark O'Connell
}

\seealso{
  \code{\link{arrangeC}}
}
\examples{
x <- runif(1000)
y <- runif(1000)
plot(x, y)
savingby2d(x, y)
## value near 1, no real benefit from bivariate view

x1 <- runif(1000)
y1 <- x1 + rnorm(sd = 0.3, n = 1000)
plot(x1, y1)
savingby2d(x1, y1)
## smaller value indicates that the bivariate view reveals some structure
}
\keyword{savingby2d}
