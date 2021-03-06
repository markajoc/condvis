\name{condvis-package}
\alias{condvis-package}
\alias{condvis}
\docType{package}
\title{
Conditional Visualization for Statistical Models
}
\description{
  Exploring statistical models by interactively taking 2-D and 3-D sections in
  data space. The main functions for end users are \code{\link{ceplot}} (see
  example below) and \code{\link{condtour}}. Requires
  \href{http://www.xquartz.org/}{XQuartz} on Mac OS, and X11 on Linux. A website
  for the package is available at \href{http://markajoc.github.io/condvis/}{
  markajoc.github.io/condvis}. Source code is available to browse at
  \href{https://github.com/markajoc/condvis/}{GitHub}. Bug reports and feature
  requests are very welcome at
  \href{https://github.com/markajoc/condvis/issues}{GitHub}.
}
\details{
  \tabular{ll}{
    Package: \tab condvis\cr
    Type: \tab Package\cr
    Version: \tab 0.5-1\cr
    Date: \tab 2018-09-13\cr
    License: \tab GPL (>= 2)\cr
  }
}
\author{
Mark O'Connell <mark_ajoc@yahoo.ie>, Catherine Hurley <catherine.hurley@mu.ie>,
Katarina Domijan <katarina.domijan@mu.ie>.
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

ceplot(data = mtcars, model = model1, sectionvars = "wt")
}
}
\references{
O'Connell M, Hurley CB and Domijan K (2017). ``Conditional
  Visualization for Statistical Models: An Introduction to the
  \strong{condvis} Package in R.''\emph{Journal of Statistical Software},
  \strong{81}(5), pp. 1-20. <URL:http://dx.doi.org/10.18637/jss.v081.i05>.
}
