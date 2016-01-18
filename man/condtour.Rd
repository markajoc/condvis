\name{condtour}
\alias{condtour}
\title{Conditional tour: a tour through sections in data space}
\description{
This function produces a tour through sections in data space defined by a path.
}
\usage{
condtour(data, model, path)
}
\arguments{
  \item{}{}
}
\value{
  \item{}{}
}
\seealso{

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