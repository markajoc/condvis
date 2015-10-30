\name{ceplot.separate}
\alias{ceplot.separate}
\title{Interactive conditional expectation plot; separate windows}
\description{
  Creates an interactive conditional expectation plot, which consists of two
  main parts on separate devices. One device shows a single plot depicting a 
  section through a fitted model surface, or conditional expectation. The other 
  device has plots showing data summaries which give the current condition, and 
  which can be altered by clicking with the mouse.
}
\usage{
ceplot.separate(data, model, response = NULL, S = NULL, C = NULL, sigma = NULL, 
    distance = "euclidean", cex.axis = NULL, cex.lab = NULL, tck = NULL, 
    view3d = FALSE, Corder = "default", selectortype = "minimal")
}
\arguments{
  \item{data}{ a dataframe containing the training data for the model(s) 
    in \code{model}}
  \item{model}{ either a single model object, or a list containing model 
    objects.}
  \item{response}{ integer index of the response variable in \code{data}
    (\code{character} variable name is also acceptable)}
  \item{S}{ integer index of the 'free' variable(s) to be used in the expectation 
    plot (\code{character} variable name(s) also acceptable). Defaults to the
    first variable in \code{data} that is not the response.}
  \item{C}{ a list, each element of which is either an integer index of 
    \code{data} or a vector of integer indices of length 2 (\code{character} 
    variable names are also acceptable). Defaults to the list produced by
    \code{\link{arrangeC}}.}
  \item{sigma}{ tuning parameter for colouring data by proximity to current 
    conditioning, passed to \code{\link{visualweight}}.}  
  \item{distance}{ distance metric to be used in \code{\link{visualweight}}, 
    choice of: \code{"euclidean"} (default) or \code{"chebyshev"}}  
  \item{type}{ type of interactive plot. \code{"default"} plots everything on 
    one device. \code{"separate"} plots the condition selector plots on a 
    separate device. \code{"shiny"} creates and opens a Shiny web application.}  
  \item{cex.axis}{ scaling parameter for axis text. Default is 0.7.}
  \item{cex.lab}{ scaling parameter for axis labels. Default is 0.8.}
  \item{tck}{ parameter for axis ticks. Default is -0.05.}
  \item{view3d}{ logical. If \code{TRUE}, displays the model section as a 
    prespective plot if possible.}
  \item{Corder}{ method of arranging variables in \code{C} if \code{C} is not 
    supplied. See \code{\link{arrangeC}}}
  \item{selectortype}{ only used if \code{type = "separate"}. \code{"minimal"} is the default behaviour, simply calling
    \code{\link{plotxc}} on the variables given by \code{C}. \code{"pcp"} gives 
    a parallel coordinates plots from which to choose sections/conditions. 
    \code{"full"} produces a full scatterplot matrix from which to choose
    sections/conditions.}  
}
\value{
  \item{}{}
}
\seealso{
  \code{\link{ceplot}}
}
\examples{
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)

model <- lm(mpg ~ wt + qsec + cyl + hp, data = mtcars)

ceplot.separate(data = mtcars, model = model, response = "mpg", S = "wt", 
    C = c("qsec", "cyl", "hp"))
}
\keyword{ceplot.shiny}