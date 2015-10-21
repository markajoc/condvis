\name{condvis-package}
\alias{condvis-package}
\alias{condvis}
\docType{package}
\title{
Conditional visualization for statistical models
}
\description{
  Visualizations for exploring statistical models. The main function for end 
  users is \code{\link{interactiveceplot}} (see example below).
}
\details{
  \tabular{ll}{
    Package: \tab condvis\cr
    Type: \tab Package\cr
    Version: \tab 0.2.4\cr
    Date: \tab 2015-10-14\cr
    License: \tab GPL-2\cr
  }
}
\author{
Mark O'Connell <mark.oconnell@maths.nuim.ie>
}
\keyword{ package }
\examples{
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)

library(gam)
model1 <- list(quadratic = lm(mpg ~ cyl + am + qsec + wt + I(wt^2), data = mtcars),
               additive = gam(mpg ~ cyl + am + qsec + s(wt), data = mtcars))

interactiveceplot(data = mtcars, model = model1, 
    S = c("wt"), width = 10, height = 7)
}
