% plotxc.Rd
\name{plotxs}
\alias{plotxs}
\title{Expectation plot}
\description{
  Plot depicting a section through a fitted model surface, or conditional 
  expectation.
}
\usage{
plotxs(xs, y, xc.cond, model, model.colour = NULL, model.lwd = NULL,
       model.lty = NULL, model.name = NULL, yhat = NULL, mar = NULL, 
       data.colour = NULL, data.order = NULL)
}
\arguments{
  \item{xs}{ a vector or \code{data.frame} with two columns.}
  \item{y}{ a vector.}
  \item{xc.cond}{ a \code{data.frame} with one row.}
  \item{model}{ a model object, or a list of model objects.}
  \item{model.colour,
  model.lwd,
  model.lty}{ graphical parameters passed to \code{col},
    \code{lwd} and \code{lty} respectively in a call to \code{points} (to 
    differentiate between models).}
  \item{model.name}{ names for the models in \code{model}, used for the legend.}
  \item{yhat}{ a list of fitted values for the models in \code{model}}
  \item{mar}{ margins for plotting.}
  \item{data.colour }{vector of colours for plotting the raw data (\code{xs} 
    and/or \code{y})}
  \item{data.order }{integer vector giving the order in which to plot raw data}  

}
\value{
  A list containing:
    \item{xs}{}
    \item{y}{}
    \item{xc.cond}{}
    \item{model}{}
    \item{model.colour}{}
    \item{model.lwd}{}
    \item{model.lty}{}
    \item{model.name}{}
    \item{yhat}{}
    \item{mar}{}
    \item{data.colour }{}
    \item{data.order}{} 
}
\author{Mark O'Connell}
\seealso{
  \code{\link{ceplot}},  \code{\link{plotxc}}.
}
\examples{
## expected value of 'mpg' conditional on fitted model and predictor values in 
## first observation of mtcars, evaluated for a range of 'wt' values.

data(mtcars)
model <- lm(mpg ~ ., data = mtcars)
plotxs(xs = mtcars[, "wt", drop = FALSE], 
    y = mtcars[, "mpg", drop = FALSE], 
    xc.cond = mtcars[1, ], 
    model = list(model))
}
\keyword{plotxs}