\name{ceplot.static}
\alias{ceplot.static}
\title{Conditional expectation plot; static}
\description{
  Creates a conditional expectation plot, which consists of two main parts. The
  left-hand side is a section through a fitted model surface, or a conditional
  expectation. The right-hand side of the plot shows small data summaries
  (either 1-d or 2-d) which show marginal views of the data, as well as the
  current condition.
}
\usage{
ceplot.static(data, model, response = NULL, S = NULL, C = NULL, sigma = NULL, 
    distance = "euclidean", cex.axis = NULL, cex.lab = NULL, tck = NULL, 
    view3d = FALSE, theta3d = NULL, phi3d = NULL, Corder = "default", 
    Xc.cond = NULL)
}
\arguments{
  \item{data}{ a dataframe containing the training data for the model(s) 
    in \code{model}}
  \item{model}{ either a single model object, or a list containing model 
    objects.}
  \item{response}{ integer index of the response variable in \code{data}
    (\code{character} variable name is also acceptable)}
  \item{S}{ integer index of the 'free' variable(s) to be used in the 
    expectation plot (\code{character} variable name(s) also acceptable). 
    Defaults to the first variable in \code{data} that is not the response.}
  \item{C}{ a list, each element of which is either an integer index of 
    \code{data} or a vector of integer indices of length 2 (\code{character} 
    variable names are also acceptable). Defaults to the list produced by
    \code{\link{arrangeC}}.}
  \item{sigma}{ tuning parameter for colouring data by proximity to current 
    conditioning, passed to \code{\link{visualweight}}.}  
  \item{distance}{ distance metric to be used in \code{\link{visualweight}}, 
    choice of: \code{"euclidean"} (default) or \code{"chebyshev"}}  
  \item{cex.axis}{ scaling parameter for axis text. Default is 0.7.}
  \item{cex.lab}{ scaling parameter for axis labels. Default is 0.8.}
  \item{tck}{ parameter for axis ticks. Default is -0.05.}
  \item{view3d}{ logical; if \code{TRUE}, a 3-d perspective plot is produced 
    if possible}
  \item{theta3d}{horizontal rotation of perspective plot}
  \item{phi3d}{vertical rotation of perspective plot}    
  \item{Corder}{ method of arranging variables in \code{C} if \code{C} is not 
    supplied. See \code{\link{arrangeC}}}
  \item{Xc.cond}{ condition to produce section, defaults to first observation}  
}    
\value{
A list containing:
  \item{Xc}{ observations of the variables in \code{C}}
  \item{sigma}{tuning parameter for colouring data by proximity to current 
    conditioning, passed to \code{\link{visualweight}} }
  \item{screens }{a list containing the screen definitions resulting from 
    calls to \code{\link{split.screen}}}
  \item{xcplots }{a list containing the output from calls to 
    \code{\link{plotxc}}}
  \item{xsplot }{a list containing the output from a call to 
    \code{\link{plotxs}}}
}
\seealso{
  \code{\link{ceplot}}.
}
\examples{
data(crab)
model <- glm(satellites ~ ., data = crab, family = poisson(link = log))

dev.new(height = 6, width = 10)
obj <- ceplot.static(data = crab, model = model, S = "weight")
}
\keyword{ceplot.static}
