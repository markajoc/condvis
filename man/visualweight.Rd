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
visualweight(xc, xc.cond, sigma = NULL, threshold = NULL)
}
\arguments{
  \item{xc}{ a dataframe containing numeric values or factors.}
  \item{xc.cond}{ a dataframe containing numeric values or factors.}
  \item{sigma}{ a numeric value greater than zero. Used to scale the distance 
    function for assigning weights. Larger values show more data, smaller values 
    show less data.}
  \item{threshold}{ a cut-off value for weights, below which observations will 
    be discarded. Defaults to 0.05.}
}
\details{
  Observations where the categorical predictor levels do not match are assigned
  weights 0 first. Then observations are assigned weights between 0 and 1 
  according to a function of Euclidean distance. 
}
\value{
A list:
  \item{k }{weights between 0 and 1}
  \item{order }{order index of \code{k} from smallest to largest, which excludes 
    values less than \code{threshold}}
}
\author{
Mark O'Connell
}

\examples{
## Highlighting cases near the first observation

vw <- visualweight(mtcars, mtcars[1, ], sigma = 2)
print(vw)
plot(vw$k)
plot(vw$k[vw$order])

## Its use in an interactive ceplot
\dontrun{
library(gam)
library(e1071)

data(powerplant)
model <- list(gam = gam(PE ~ s(AT) + s(V) + s(AP) + s(RH), data = powerplant), 
              svm = svm(PE ~ ., data = powerplant))

interactiveceplot(data = powerplant, model = model, S = "V",
    height = 6, width = 10, sigma = 0.2, threshold = 0.1)
}
}

\keyword{ visualweight }
