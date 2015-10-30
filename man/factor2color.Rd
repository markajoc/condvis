\name{factor2color}
\alias{factor2color}
\title{
  Convert a factor to suitable colors.
}
\description{
  Takes a factor and returns a vector of colors representing the factor levels.
}
\usage{
factor2color(x, colors = NULL)
}
  \arguments{
    \item{x}{ a factor}
    \item{colors}{ colors to use for the levels of \code{x}, recycled if too
      short. Defaults to a qualitative color scheme, \code{"Set3"} from 
      \code{RColorBrewer} package if available, or \code{\link{rainbow}}.}
}
\value{
  A character vector containing colors.
}
\author{
  Mark O'Connell
}
\seealso{
  \code{\link{cont2color}}
}
\examples{
plot(iris[, c("Petal.Length", "Petal.Width")], pch = 21,
     bg = factor2color(iris$Species))
legend("topleft", legend = levels(iris$Species),
       fill = factor2color(as.factor(levels(iris$Species))))
}
\keyword{ factor2color }
