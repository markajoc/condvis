\name{visualweight}
\alias{visualweight}

\title{
  Assign weights to observations according to proximity in predictor space
}
\description{
  This function assigns a weight (between 0 and 1) to observations in a data
  set according to their proximity to a given point in the space. 
}
\usage{
visualweight(xc, xc.cond, sigma = NULL, distance = "euclidean")
}
\arguments{
  \item{xc}{ a dataframe containing numeric values or factors.}
  \item{xc.cond}{ a dataframe containing numeric values or factors.}
  \item{sigma}{ a numeric value greater than zero, defaults to 1. Used to scale 
    the distance function for assigning weights. Larger values show more data, 
    smaller values show less data.}
  \item{distance}{ distance metric used, \code{"euclidean"} (default) or 
    \code{"maxnorm"}.}
}
\details{
  Observations where the categorical predictor levels do not match are assigned
  weights 0 first. Then observations are assigned weights between 0 and 1 
  according to a distance measure (\code{\link{dist1}}). 
}
\value{
A list:
  \item{k }{weights between 0 and 1}
  \item{order }{order index of \code{k} from smallest to largest.}
}
\author{
Mark O'Connell
}

\examples{
## Highlighting cases near the first observation

vw <- visualweight(mtcars[1, ], mtcars, sigma = 2)
print(vw)

## Its use in an interactive ceplot
\dontrun{
library(mgcv)
library(e1071)

data(powerplant)
model <- list(gam = gam(PE ~ s(AT) + s(V) + s(AP) + s(RH), data = powerplant), 
              svm = svm(PE ~ ., data = powerplant))

interactiveceplot(data = powerplant, model = model, S = "V",
    height = 6, width = 10, sigma = 0.2)
}
}

\keyword{ visualweight }
