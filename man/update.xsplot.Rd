\name{update.xsplot}
\alias{update.xsplot}
\title{Update a conditional expectation plot}
\description{
  Takes an existing object of class \code{'xsplot'}
}
\usage{
## S3 method for class 'xsplot'
\method{update}{xsplot}(object, xc.cond = NULL, data.colour = NULL, 
    data.order = NULL, view3d = NULL, theta3d = NULL, phi3d = NULL, 
    xs.grid = NULL, prednew = NULL, ...)
}
\arguments{
  \item{object}{ object of class \code{'xsplot'}, as produced by \code{\link{plotxs}}}
  \item{xc.cond}{ to update the section visualised}
  \item{data.colour}{ to update the colour assigned to observations}
  \item{data.order}{ to update the plotting order of observations}
  \item{view3d}{ if \code{TRUE}, prefers a 3-D perspective mesh over a colour 
    contour map}
  \item{theta3d}{ horizontal rotation of perspective mesh plot}
  \item{phi3d}{ vertical rotation of perspective mesh plot}
  \item{xs.grid}{ grid of values at which to evaluate models on section}
  \item{prednew}{ evaluated intersection of models with section, useful when 
    rotating a plot, where the intersection does not need to be recalculated}
  \item{...}{ not used}  
}
\value{
 The same object of class \code{'xsplot'}, with relevant parts updated
}
\seealso{
\code{\link{update.xcplot}}, \code{\link{plotxs}}
}
\keyword{update.xsplot}