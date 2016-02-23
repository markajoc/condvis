\name{condvis-package}
\alias{condvis-package}
\alias{condvis}
\docType{package}
\title{
Conditional Visualization for Statistical Models
}
\description{
  Exploring statistical models by interactively taking 2-D and 3-D sections in
  data space. The main function for end users is \code{\link{ceplot}} (see
  example below). Requires \href{http://www.xquartz.org/}{Xquartz} on Mac OS,
  and X11 on Linux. Bugs and issues can be raised at
  \href{https://bitbucket.org/markajoc/condvis/issues}{BitBucket}.
}
\details{
  \tabular{ll}{
    Package: \tab condvis\cr
    Type: \tab Package\cr
    Version: \tab 0.3\cr
    Date: \tab 2016-02-22\cr
    License: \tab GPL-2\cr
  }
}
\author{
Mark O'Connell <mark.oconnell@maths.nuim.ie>
}
\keyword{ package }
\examples{
\dontrun{
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)

library(mgcv)
model1 <- list(
  quadratic = lm(mpg ~ cyl + am + qsec + wt + I(wt^2), data = mtcars),
  additive = gam(mpg ~ cyl + am + qsec + s(wt), data = mtcars))

ceplot(data = mtcars, model = model1, S = "wt")
}
}
