\name{spineplot2}
\alias{spineplot2}

\title{
  Modified spineplot.
}
\description{
  A modified version of \code{\link{spineplot}} that returns some structural
  information for subsequently querying and altering the plot.
}
\usage{
spineplot2(x, y = NULL, breaks = NULL, tol.ylab = 0.05, off = NULL,
    ylevels = NULL, col = NULL, main = "", xlab = NULL, ylab = NULL,
    xaxlabels = NULL, yaxlabels = NULL, xlim = NULL, ylim = c(0, 1),
    axes = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x }{an object, the default method expects either a single variable 
  (interpreted to be the explanatory variable) or a 2-way table. See details.}
\item{y }{a "factor" interpreted to be the dependent variable}
\item{breaks }{if the explanatory variable is numeric, this controls how it is 
  discretized. breaks is passed to hist and can be a list of arguments.}
\item{tol.ylab }{convenience tolerance parameter for y-axis annotation. If the 
  distance between two labels drops under this threshold, they are plotted 
  equidistantly.}
\item{off }{vertical offset between the bars (in per cent). It is fixed to 0 for
  spinograms and defaults to 2 for spine plots.}
\item{ylevels }{a character or numeric vector specifying in which order the 
  levels of the dependent variable should be plotted.}
\item{col }{a vector of fill colors of the same length as levels(y). The default
  is to call gray.colors.}
\item{main, xlab, ylab }{character strings for annotation}
\item{xaxlabels, yaxlabels }{character vectors for annotation of x and y axis. 
  Default to levels(y) and levels(x), respectively for the spine plot. For 
  xaxlabels in the spinogram, the breaks are used.}
\item{xlim, ylim }{the range of x and y values with sensible defaults.}
\item{axes }{logical. If FALSE all axes (including those giving level names) are
  suppressed.}
\item{... }{additional arguments passed to rect.}
}
\details{
  This is a modified version of the default method of \code{\link{spineplot}},
  in the \code{\link{graphics}} package.  The modifications are as follows: the
  extra axis showing proportions is not displayed, and the coordinates of each
  rectangle drawn are returned along with the relevant factor combinations in
  a list object.
}
\value{
A list containing:
  \item{ybottom }{the y coordinate of the bottom of each rectangle}
  \item{ytop }{the y coordinate of the top of each rectangle}
  \item{xleft }{the x coordinate of the left of each rectangle}
  \item{xright }{the x coordinate of the right of each rectangle}
  \item{xnames }{the horizontal factor levels as a character vector}
  \item{ynames }{the vertical factor levels as a character vector}
  \item{xat }{variable internal to \code{\link{spineplot}} used to position 
    horizontal factor level labels.}
  \item{yat }{variable internal to \code{\link{spineplot}} used to position 
    vertical factor level labels.}
  \item{nx }{ }
  \item{off }{ }
}
\author{
Originally Achim Zeileis, edited by Mark O'Connell
}

\seealso{
\code{\link{spineplot}}
}

\examples{
\dontrun{
obj <- condvis:::spineplot2(mtcars$cyl, as.factor(mtcars$am))
print(obj)
}
}
\keyword{ spineplot }
