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
##
}
\keyword{makepath}