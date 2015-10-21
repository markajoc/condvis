\name{wine}
\alias{wine}

\title{
  Italian wine data
}

\description{
\code{Class} 3 different cultivars\cr
\code{Alcohol} Alcohol\cr
\code{Malic} Malic acid\cr
\code{Ash} Ash\cr
\code{Alcalinity} Alcalinity of ash\cr
\code{Magnesium} Magnesium\cr
\code{Phenols} Total phenols\cr
\code{Flavanoids} Flavanoids\cr
\code{Nonflavanoid} Nonflavanoid phenols\cr
\code{Proanthocyanins} Proanthocyanins\cr
\code{Intensity} Color intensity\cr
\code{Hue} Hue\cr
\code{OD280} OD280/OD315 of diluted wines\cr
\code{Proline} Proline\cr
}

\format{
178 observations on 14 variables.
}

\source{
  UCI repository. 
  \url{https://archive.ics.uci.edu/ml/datasets/Wine}
}

\references{
  S. Aeberhard, D. Coomans and O. de Vel (1992), Comparison of Classifiers in 
  High Dimensional Settings, \emph{Technical Report} \bold{92}-02, Dept. of 
  Computer Science and Dept. of Mathematics and Statistics, James Cook 
  University of North Queensland.
}

\examples{
data(wine)
pairs(wine[, -1], col = factor2color(wine$Class))
}

\keyword{ wine }
