\name{ceplot.interactive}
\alias{ceplot.interactive}
\title{Interactive conditional expectation plot; single device}
\description{
  Creates an interactive conditional expectation plot, which consists of two
  main parts.  The left-hand side is a single plot depicting a section through a
  fitted model surface, or conditional expectation.  The right-hand side of the
  plot shows small data summaries which give the current condition, which can
  be altered by clicking with the mouse.
}
\usage{
ceplot.interactive(height = 7, width = 10, ...)
}
\arguments{
  \item{height}{ height of device in inches}
  \item{width}{ width of device in inches}
  \item{...}{ arguments passed to \code{\link{ceplot.static}}}
}
\value{
  Returns no value, but produces an interactive plot as a side-effect.  The
  current \R session is busy until the interactive session is ended by the user,
  or the graphics device is closed. 
}
\author{Mark O'Connell}
\seealso{
  \code{\link{ceplot.static}}, \code{\link{getGraphicsEvent}}
}
\examples{
data(wine)
wine$Class <- as.factor(wine$Class)
library(e1071)

model5 <- list(svm(Class ~ ., data = wine))

ceplot.interactive(data = wine, model = model5, S = c("Alcohol", "Hue"), 
    sigma = 5)
}
\keyword{ceplot.interactive}