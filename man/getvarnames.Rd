\name{getvarnames}
\alias{getvarnames}
\title{Extract variable names from model object}
\description{
  Extracts the variable names from a fitted model object.
}
\usage{
getvarnames(model)
}
\arguments{
  \item{model}{  an \R model object with a \code{terms} component}
}
\value{
A list containing:
  \item{response}{ a character vector containing the response variable name}
  \item{predictors}{ a character vector containing the predictor variable names}
}
\seealso{
 \code{\link{cleanstring}}
}
\examples{
data(mtcars)
m <- lm(mpg ~ ., data = mtcars)
getvarnames(m)
}
\keyword{getvarnames}