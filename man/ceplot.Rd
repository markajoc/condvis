\name{ceplot}
\alias{ceplot}
\title{Interactive conditional expectation plot}
\description{
  Creates an interactive conditional expectation plot, which consists of two
  main parts.  The left-hand side is a single plot depicting a section through a
  fitted model surface, or conditional expectation.  The right-hand side of the
  plot shows small data summaries which give the current condition, which can
  be altered by clicking with the mouse.
}
\usage{
ceplot(data, model, response = NULL, S = NULL, C = NULL, sigma = NULL, 
    distance = "euclidean", type = "default", cex.axis = NULL, cex.lab = NULL, 
    tck = NULL, view3d = FALSE, Corder = "default", selectortype = "minimal", 
    width = 9, height = 7)
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
  \item{width}{ width of device in inches, only used when 
    \code{type == "default"} }  
  \item{height}{ height of device in inches, only used when 
    \code{type == "default"} } 
}
\value{
  Returns no value, but produces an interactive plot as a side-effect.  The
  current \R session is busy until the interactive session is ended by the user,
  or the graphics device is closed. If using the Shiny option, "Esc" can be 
  pressed to free up the \R session.
}
\author{Mark O'Connell}
\seealso{
  \code{\link{getGraphicsEvent}}
}
\examples{
## Example 1: Multivariate regression, xs one continuous predictor

mtcars$cyl <- as.factor(mtcars$cyl)

library(gam)
model1 <- list(quadratic = lm(mpg ~ cyl + hp + wt + I(wt^2), data = mtcars),
               additive = gam(mpg ~ cyl + hp + s(wt), data = mtcars))

conditionvars1 <- list(c("cyl", "hp"))

ceplot(data = mtcars, model = model1, response = "mpg", S = "wt", 
    C = conditionvars1, sigma = 0.1, width = 10, height = 6)

## Example 2: Binary classification, xs one categorical predictor

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)

library(gam)
library(e1071)
model2 <- list(
    svm = svm(am ~ mpg + wt + cyl, data = mtcars, family = "binomial"),
    glm = glm(am ~ mpg + wt + cyl, data = mtcars, family = "binomial"),
    gam = gam(am ~ mpg + s(wt) + cyl, data = mtcars, family = "binomial"))

ceplot(data = mtcars, model = model2, S = "cyl", sigma = 0.1, 
    width = 10, height = 6)

## Example 3: Multivariate regression, xs both continuous

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$gear <- as.factor(mtcars$gear)

library(e1071)
model3 <- list(svm(mpg ~ wt + qsec + cyl + hp + gear, 
    data = mtcars, family = "binomial"))

conditionvars3 <- list(c("cyl","gear"), "hp")

ceplot(data = mtcars, model = model3, S = c("wt", "qsec"), 
    sigma = 1, C = conditionvars3, width = 10, height = 6)

## Example 4: Multi-class classification, xs both categorical

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)

library(e1071)
model4 <- list(svm(carb ~ ., data = mtcars, family = "binomial"))

ceplot(data = mtcars, model = model4, S = c("cyl", "am"), 
    sigma = 3, type = "separate", selectortype = "pcp")

## Example 5: Multi-class classification, xs both continuous

data(wine)
wine$Class <- as.factor(wine$Class)
library(e1071)

model5 <- list(svm(Class ~ ., data = wine))

ceplot(data = wine, model = model5, S = c("Alcohol", "Hue"), sigma = 5)

## Example 6: Multi-class classification, xs with one categorical predictor,
##            and one continuous predictor.

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$carb <- as.factor(mtcars$carb)

library(e1071)
model6 <- list(svm(cyl ~ carb + wt + hp, data = mtcars, family = "binomial"))

ceplot(data = mtcars, model = model6, sigma = 1, S = c("carb", "wt"), C = "hp")
}
\keyword{ceplot}