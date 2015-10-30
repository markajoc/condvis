\name{ceplot.shiny}
\alias{ceplot.shiny}
\title{Interactive conditional expectation plot; Shiny web application}
\description{
  Creates an interactive conditional expectation plot, which consists of two
  main parts.  The left-hand side is a single plot depicting a section through a
  fitted model surface, or conditional expectation.  The right-hand side of the
  plot shows small data summaries which give the current condition, which can
  be altered by clicking with the mouse. This version is rendered in a Shiny 
  web application.
}
\usage{
ceplot.shiny(data, model, response = NULL, S = NULL, C = NULL, cex.axis = NULL, 
    cex.lab = NULL, tck = NULL, Corder = "default")
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
  \item{cex.axis}{ scaling parameter for axis text. Default is 0.7.}
  \item{cex.lab}{ scaling parameter for axis labels. Default is 0.8.}
  \item{tck}{ parameter for axis ticks. Default is -0.05.}
  \item{Corder}{ method of arranging variables in \code{C} if \code{C} is not 
    supplied. See \code{\link{arrangeC}}}
}
\value{
  Returns no value, but produces an interactive plot as a side-effect.  The
  current \R session is busy until the interactive session is ended by the user,
  or the graphics device is closed. If using the Shiny option, "Esc" can be 
  pressed to free up the \R session.
}
\author{Mark O'Connell}
\seealso{
  \code{\link{ceplot}}
}
\examples{
\dontrun{
## Example 1: Multivariate regression, xs one continuous predictor

mtcars$cyl <- as.factor(mtcars$cyl)

library(gam)
model1 <- list(quadratic = lm(mpg ~ cyl + hp + wt + I(wt^2), data = mtcars),
               additive = gam(mpg ~ cyl + hp + s(wt), data = mtcars))

conditionvars1 <- list(c("cyl", "hp"))

ceplot.shiny(data = mtcars, model = model1, response = "mpg", S = "wt", 
    C = conditionvars1)

## Example 2: Binary classification, xs one categorical predictor

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)

library(gam)
library(e1071)
model2 <- list(
    svm = svm(am ~ mpg + wt + cyl, data = mtcars, family = "binomial"),
    glm = glm(am ~ mpg + wt + cyl, data = mtcars, family = "binomial"),
    gam = gam(am ~ mpg + s(wt) + cyl, data = mtcars, family = "binomial"))

ceplot.shiny(data = mtcars, model = model2, S = "cyl")
}
}
\keyword{ceplot.shiny}