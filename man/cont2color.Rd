\name{cont2color}
\alias{cont2color}

\title{
  Convert continuous variable to suitable graded colors.
}
\description{
  Convert continuous variable to suitable graded colors, using 
  \code{RColorBrewer} if it is installed.
}
\usage{
cont2color(x, xrange = NULL, breaks = NULL, colors = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a numeric vector}
  \item{xrange}{ a vector of length 2 denoting the boundaries for the colors at
    either end of the color scale. All values outside this range will be 
    assigned the same color according to which side of the range they fall.
    Defaults to \code{range(x)}}
  \item{breaks}{ an integer denoting the number of splits between different
    color levels. Defaults to 11. Needs to be set if \code{colors} is 
    specified.}
  \item{colors}{ optional color vector. Default value is a diverging color
    scheme (either \code{"PiYG"} from \code{RColorBrewer} if available, or
    \code{\link{cm.colors}}). Otherwise, a vector of colors may be specified, of
    length equal to \code{breaks}.}
}
\details{
  Uses the \code{RColorBrewer} package, if installed.
}
\value{
  A vector of colors.
}
\author{
  Mark O'Connell
}
\seealso{
  \code{\link{factor2color}}
}
\examples{
x <- runif(200)
plot(x, col = cont2color(x, c(0,1)))

plot(x, col = cont2color(x, c(0,0.5)))

plot(sort(x), col = cont2color(sort(x), c(0.25,0.75)), pch = 16)
abline(h = c(0.25, 0.75), lty = 3)
}
\keyword{ cont2color }
