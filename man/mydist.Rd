\name{mydist}
\alias{mydist}
\title{Distance function}
\description{
  Calculates Minkowski distance between a single point and a set of other 
  points. (The actual returned value is distance^p to save on computation.)
}
\usage{
    mydist(x, X, p = 2, inf = FALSE)
}
\arguments{
  \item{x}{ vector describing a point}
  \item{X}{ matrix with \code{ncol(X) == length(x)}, defining points to
    calculate distance from \code{x}}
  \item{p}{ power to use in Minkowski distance, defaults to 2 for Euclidean 
    distance.}  
  \item{inf}{ if \code{TRUE} function returns the l-infinity distance, also 
    known as Chebyshev distance.}   
}
\value{
  A numeric vector.
}

\seealso{

}
\examples{
x <- runif(5000)
y <- runif(5000)

x1 <- 0.5
y1 <- 0.5

par(mfrow = c(2, 2))

for(p in c(0.5, 1, 2, 10)){
    d <- mydist(x = c(x1, y1), X = cbind(x, y), p = p) ^ (1/p)
    col <- rep("black", length(x))
    col[d < 0.3] <- "red"
    plot(x, y, pch = 16, col = col, asp = 1, main = paste("p = ", p, sep = ""))
}
}
\keyword{mydist}