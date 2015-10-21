\name{pollution}
\alias{pollution}

\title{
  McDonald and Schwing's pollution data
}

\description{
  Data set for investigating the relationship of age-adjusted mortality rates 
  with socio-economic and environmental variables.\cr\cr
  \code{y} total Age Adjusted Mortality Rate\cr
  \code{x1} mean annual precipitation in inches\cr
  \code{x2} mean January temperature in degrees Fahrenheit\cr
  \code{x3} mean July temperature in degrees Fahrenheit\cr
  \code{x4} percent of 1960 SMSA population that is 65 years of age or over\cr
  \code{x5} population per household, 1960 SMSA\cr
  \code{x6} median school years completed for those over 25 in 1960 SMSA\cr
  \code{x7} percent of housing units that are found with facilities\cr
  \code{x8} population per square mile in urbanized area in 1960\cr
  \code{x9} percent of 1960 urbanized area population that is non-white\cr
  \code{x10} percent employment in white-collar occupations in 1960 urbanized 
    area\cr
  \code{x11} percent of families with income under 3,000 in 1960 urbanized 
    area\cr
  \code{x12} relative population potential of hydrocarbons, HC\cr
  \code{x13} relative pollution potential of oxides of nitrogen, NOx\cr
  \code{x14} relative pollution potential of sulfur dioxide, SO2\cr
  \code{x15} percent relative humidity, annual average at 1 p.m.\cr
}

\format{
  60 observations on 15 continuous variables. Each observation represents a SMSA 
  (Standard Metropolitan Statistical Area) in the United States.
}

\source{
  \url{http://www4.stat.ncsu.edu/~boos/var.select/pollution.html}
}

\references{
  McDonald and Schwing (1973), "Instabilities of Regression Estimates Relating 
  Air Pollution to Mortality," \emph{Technometrics}, \bold{15}, 463-481.
}

\examples{
data(pollution)
model <- lm(y ~ ., data = pollution)
summary(model)
}

\keyword{ pollution }
