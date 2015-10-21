\name{xy2index}
\alias{xy2index}

\title{
  Identify plot by device coordinates.
}
\description{
Not to be called by the user.
}
\usage{
xy2index(x, y, screen.info)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ x coordinate of a mouse click in normalized device coordinates.}
  \item{y}{ y coordinate of a mouse click in normalized device coordinates.}
  \item{screen.info}{ object containing screen location information, as returned
  by \code{\link{getscreeninfo}}.}
}
\value{
  An integer, relating to the index of the list named \code{C} in 
  \code{\link{ceplot}}.
}
\author{
Mark O'Connell
}

\seealso{
\code{\link{getscreeninfo}}
}
\examples{
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)

library(gam)
model1 <- list(quadratic = lm(mpg ~ . + I(wt^2), data = mtcars))

dev.new(height = 6, width = 10)
obj <- ceplot(data = mtcars, model = model1, response = "mpg", S = "wt")

screen.info <- getscreeninfo(obj)

xy2index(0.8, 0.9, screen.info)
xy2index(0.9, 0.9, screen.info)
}

\keyword{ xy2index }
