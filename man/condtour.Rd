\name{condtour}
\alias{condtour}
\title{Conditional tour: a tour through sections in data space}
\description{
This function produces a tour through sections in data space defined by a path.
}
\usage{
condtour(data, model, path, response = NULL, S = NULL, C = NULL, 
    sigma = NULL, distance = "euclidean", cex.axis = NULL, cex.lab = NULL, 
    tck = NULL, view3d = FALSE, conf = FALSE, select.colour = "blue")
}
\arguments{
  \item{data}{ a data frame}
  \item{model}{ a fitted model object, or list of fitted models}
  \item{path}{a path through the predictor space, i.e. a dataframe with same 
    column names as \code{data[, C]} describing values to condition on}
  \item{response}{ the index of the response variable in \code{data}}
  \item{S}{ the index of the section predictor(s) in \code{data}}
  \item{C}{ the index of the condition predictor(s) in \code{data}}
  \item{sigma}{ parameter to choose how much data to display}
  \item{distance}{ type of distance measure for sigma}
  \item{cex.axis}{ scaling for axis text}
  \item{cex.lab}{ scaling for axis labels}
  \item{tck}{ scaling for axis ticks}
  \item{view3d}{ if S denotes two continuous variables, should the section be 
    visualised as a 3-D mesh}
  \item{conf}{ show approximate confidence bounds for expected values}
  \item{select.colour}{ colour for highlighting selected sections/conditions}
}
\value{
  An interactive graphic.
}
\seealso{
\code{\link{ceplot}}
}
\examples{
\dontrun{
data(powerplant)
library(e1071)
model <- svm(PE ~ ., data = powerplant)
path <- makepath(powerplant[-5], 25)
condtour(data = powerplant, model = model, path = path$path, S = "AT")

##
data(wine)
wine$Class <- as.factor(wine$Class)
library(e1071)
model5 <- list(svm(Class ~ ., data = wine))
path <- makepath(wine[-1], 50)
condtour(data = wine, model = model5, path = path$path, S = c("Hue", 
    "Flavanoids"), sigma = 3)

}
}
\keyword{condtour}