\name{getscreeninfo}
\alias{getscreeninfo}
\title{
  Retrieve screen arrangement information from an object of class 'ceplot'.
}
\description{
Retrieves all the necessary information to guide a mouse event handling function
to the correct screen, by providing screen coordinates and indices.
}
\usage{
getscreeninfo(plotobject)
}
\arguments{
  \item{plotobject}{ object returned by a call to \code{\link{ceplot}}.}
}
\details{
Screen coordinates are in normalized device coordinates (\code{ndc}), see
\code{\link{grconvertX}.}
}
\value{
A \code{data.frame} containing:
    \item{xcplots.index}{ index for a condition plot produced by
    \code{\link{plotxc}} within \code{\link{ceplot}}. (Indexes along
    \code{plotobject$xcplots})}
    \item{screen.number}{ the screen number for the plot indexed by
    \code{xcplots.index}}
    \item{xleft}{ the left x coordinate for the screen}
    \item{xright}{ the right x coordinate for the screen}
    \item{ybottom}{ the bottom y coordinate for the screen}
    \item{ytop}{ the top y coordinate for the screen}
}
\author{
Mark O'Connell
}

\seealso{
\code{\link{ceplot}}, \code{\link{split.screen}}
}
\examples{
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)

library(gam)
model1 <- list(quadratic = lm(mpg ~ I(wt^2) + ., data = mtcars),
               additive = gam(mpg ~ s(wt) + . - wt, data = mtcars))

dev.new(height = 6, width = 8)
obj <- ceplot(data = mtcars, model = model1, S = "wt")

getscreeninfo(obj)
}

\keyword{ getscreeninfo}
