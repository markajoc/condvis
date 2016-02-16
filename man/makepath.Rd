\name{makepath}
\alias{makepath}
\title{Make a path through predictor space to guide conditional tour}
\description{
Make a path through predictor space to guide conditional tour
}
\usage{
makepath(Xc, ncentroids, ninterp = 4)
}
\arguments{
  \item{Xc}{ a dataframe}
  \item{ncentroids}{number of centroids or prototype points to summarise the 
    data}
  \item{ninterp}{ number of points to interpolate between the 
    centroids/prototypes, defaults to 4.}  
}
\value{
  \item{centroids}{centroids or prototype points to summarise the data}
  \item{path}{final path interpolated between centroids}
}
\seealso{
\code{\link{condtour}}
}
\examples{
d <- data.frame(x = runif(500), y = runif(500))
plot(d)
mp1 <- makepath(d, 5)
points(mp1$centers, type = "b", col = "blue", pch = 16)
mp2 <- makepath(d, 40)
points(mp2$centers, type = "b", col = "red", pch = 16)
}
\keyword{makepath}