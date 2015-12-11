\name{update.xcplot}
\alias{update.xcplot}
\title{Update a condition selector plot}
\description{
  Takes an existing object of class \code{'xcplot'} and updates it according to
  the coordinates of a mouseclick.
}
\usage{
\method{update}{xcplot}(object, xclick, yclick, ...)
}
\arguments{
  \item{object}{ an object of class \code{'xcplot'}, as produced by 
    \code{\link{plotxc}}}
  \item{xclick}{ numeric value between 0 and 1, usually captured from mouse}
  \item{yclick}{ numeric value between 0 and 1, usually captured from mouse}
  \item{...}{ not used}  
}
\value{
  The same object of class \code{'xcplot'} with parts updated. A side effect is
  the updating of the plot relating to the object.
}
\seealso{
  \code{\link{update.xsplot}}, \code{\link{plotxc}}
}
\examples{
obj <- plotxc(x = mtcars$mpg, xc.cond = 20)
obj <- update(obj, 0.8, 0.5)
}
\keyword{update.xcplot}