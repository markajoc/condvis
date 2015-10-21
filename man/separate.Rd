\name{separate}
\alias{separate}
\title{Interactive conditional expectation plot; separate windows}
\description{

}
\usage{
separate(data, model, response = NULL, S = NULL, C = NULL, sigma = NULL, 
    threshold = NULL, type = "gaussian", cex.axis = NULL, cex.lab = NULL, tck = NULL, 
    view3d = FALSE, method = "default", selectortype = "minimal")
}
\arguments{
  \item{data}{}
  \item{model}{}
  \item{response}{}
  \item{S}{}
  \item{C}{}
  \item{sigma}{}
  \item{threshold}{}
  \item{type}{}
  \item{cex.axis}{}
  \item{cex.lab}{}
  \item{tck}{}
  \item{view3d}{}
  \item{method}{only used if \code{selectortype} is set to \code{"minimal"}}
  \item{selectortype}{}
}
\value{
  \item{}{}
}
\seealso{

}
\examples{
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)

model <- lm(mpg ~ wt + qsec + cyl + hp, data = mtcars)

separate(data = mtcars, model = model, response = "mpg", S = "wt", C = c("qsec", "cyl", "hp"))
}
\keyword{}