\name{polygonarea}
\alias{polygonarea}
\title{
  Calculate the area of a polygon in the plane
}
\description{
  A simple algorithm to calculate the area of a 2-dimensional polygon, based on
  the code at \url{http://www.mathopenref.com/coordpolygonarea2.html}. The input
  coordinates must trace the polygon in a clockwise direction.
}
\usage{
polygonarea(x, y = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ x coordinates for the vertices of a polygon, or a 2 column
  dataframe containing both x and y coordinates.}
  \item{y}{ y coordinates for the vertices of a polygon.}
}
\value{
A positive number.
}

\author{
Mark O'Connell
}

\seealso{
\code{\link{savingby2d}}
}
\examples{
## square
x1 <- c(0, 0, 1, 1)
y1 <- c(0, 1, 1, 0)
polygonarea(x1, y1)

## circle approximation

theta <- seq(0, 2*pi, length.out = 15)
x2 <- cos(theta)
y2 <- sin(theta)
plot(x2, y2, typ = 'l')
polygonarea(x2, y2)

theta2 <- seq(0, 2*pi, length.out = 5000)
x3 <- cos(theta2)
y3 <- sin(theta2)
plot(x3, y3, typ = 'l')
polygonarea(x3, y3)
}
\keyword{polygonarea}
