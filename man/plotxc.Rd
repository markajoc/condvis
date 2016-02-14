\name{plotxc}
\alias{plotxc}
\title{Plot a 1-D or 2-D data summary for choosing conditioning values}
\description{
  Used to plot one of a histogram, barplot, boxplot or spineplot, to be used as a
  conditionselector in \code{\link{ceplot}}. Returns a list containing relevant parameters, for
  subsequent updating of the plot. Not designed to be called by the user.
}
\usage{
plotxc(xc, xc.cond, name = NULL, select.colour = NULL, select.lwd = NULL, 
    cex.axis = NULL, cex.lab = NULL, tck = NULL, select.cex = 1, ...)
}
\arguments{
  \item{xc}{ a vector or dataframe with two columns, containing \code{numeric} or
    \code{factor} values.}
  \item{xc.cond}{ a single value of the same type as \code{xc}.}
  \item{name}{ a \code{character} vector containing the names of the variables 
    in \code{xc}.}
  \item{select.colour}{ colour for highlighting current section in condition 
    selector plots. Default is \code{"black"}.}
  \item{select.lwd}{ line thickness for the lines showing conditioning. Default is 2.}
  \item{cex.axis}{ scaling parameter for axis text. Default is 0.7.}
  \item{cex.lab}{ scaling parameter for axis labels. Default is 0.8.}
  \item{tck}{ parameter for axis ticks. Default is -0.05.}
  \item{select.cex}{ scaling used for plot symbols in condition selector plots}  
  \item{...}{ other arguments passed to \code{abline()} for showing conditioning.}
}
\value{
A list with components:
  \item{device}{ the device number on which the function was called.}
  \item{factorcoords}{ a \code{data.frame} giving the horizontal coordinates of factor
    levels for barplots and boxplots. \code{NULL} for other plot types.}
  \item{mar}{ margins used in \code{plot} call.}
  \item{name}{ the name(s) of the variable(s) in \code{xc}}
  \item{plot.type}{ a character value; one of \code{"histogram"}, \code{"barplot"},
    \code{"scatterplot"}, \code{"boxplot"} or \code{"spineplot"}.}
  \item{screen}{ the screen number in which the plot was made (see \code{\link{split.screen}}).}
  \item{select.colour}{ colour for the lines showing conditioning.}
  \item{select.lwd}{line thickness for the lines showing conditioning}
  \item{cex.axis}{ scaling parameter for axis text.}
  \item{cex.lab}{ scaling parameter for axis labels.}
  \item{tck}{ parameter for axis ticks.}
  \item{sptmp}{ an object containing the coordinates of the basic elements of the spineplot,
    including related factor levels. \code{NULL} if \code{plot.type} is not \code{"spineplot"}.}
  \item{usr}{ graphical parameter for converting between device and user coordinates.}
  \item{xc}{ passed directly from inputs.}
  \item{xc.cond.old}{ passed directly from inputs, but renamed.}
}
\author{Mark O'Connell}
\seealso{
  \code{\link{ceplot}},  \code{\link{plotxs}}.
}
\examples{
## histogram, highlighting the first case
data(mtcars)
obj <- plotxc(mtcars[, "mpg"], mtcars[1, "mpg"])
obj$usr

## barplot, highlighting 'cyl' = 6
plotxc(as.factor(mtcars[, "cyl"]), 6, select.colour = "blue")

## scatterplot, highlighting case 25
plotxc(mtcars[, c("qsec", "wt")], mtcars[25, c("qsec", "wt")],
       select.colour = "blue", select.lwd = 1, lty = 3)

## boxplot, where 'xc' contains one factor, and one numeric
mtcars$carb <- as.factor(mtcars$carb)
plotxc(mtcars[, c("carb", "wt")], mtcars[25, c("carb", "wt")],
       select.colour = "red", select.lwd = 3)

## spineplot, where 'xc' contains two factors
mtcars$gear <- as.factor(mtcars$gear)
mtcars$cyl <- as.factor(mtcars$cyl)
plotxc(mtcars[, c("cyl", "gear")], mtcars[25, c("cyl", "gear")],
       select.colour = "red")
}
\keyword{plotxc}