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
}
}
\keyword{condtour}