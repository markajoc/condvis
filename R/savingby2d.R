#' @title Assess advantage of 2-D view over 1-D view for identifying
#'   extrapolation
#'
#' @description A simple algorithm to evaluate the advantage of by taking a
#'   bivariate marginal view of two variables, when trying to avoid
#'   extrapolations, rather than two univariate marginal views.
#'
#' @param x A numeric or factor vector. Can also be a dataframe containing
#'   \code{x} and \code{y}, if \code{y} is \code{NULL}.
#' @param y A numeric or factor vector.
#' @param method Character; criterion used to quantify bivariate relationships.
#'   Can be \code{"default"}, a scagnostic measure, or \code{"DECR"} to use a
#'   density estimate confidence region.
#'
#' @return A number between 0 and 1. Values near 1 imply no benefit to using a
#'   2-D view, whereas values near 0 imply that a 2-D view reveals structure
#'   hidden in the 1-D views.
#'
#' @details If given two continuous variables, the variables are both scaled to
#'   mean 0 and variance 1. Then the returned value is the ratio of the area of
#'   the convex hull of the data to the area obtained from the product of the
#'   ranges of the two areas, i.e. the area of the bounding rectangle.
#'
#'   If given two categorical variables, all combinations are tabulated. The
#'   returned value is the number of non-zero table entries divided by the total
#'   number of table entries.
#'
#'   If given one categorical and one continuous variable, the returned value is
#'   the weighted mean of the range of the continuous variable within each
#'   category divided by the overall range of the continuous variable, where the
#'   weights are given by the number of observations in each level of the
#'   categorical variable.
#'
#'   Requires package \code{scagnostics} if a scagnostics measure is specified
#'   in \code{method}. Requires package \code{hdrcde} if \code{"DECR"} (density
#'   estimate confidence region) is specified in \code{method}. These only apply
#'   to cases where \code{x} and \code{y} are both numeric.
#'
#' @examples
#' x <- runif(1000)
#' y <- runif(1000)
#' plot(x, y)
#' savingby2d(x, y)
#' ## value near 1, no real benefit from bivariate view
#'
#' x1 <- runif(1000)
#' y1 <- x1 + rnorm(sd = 0.3, n = 1000)
#' plot(x1, y1)
#' savingby2d(x1, y1)
#' ## smaller value indicates that the bivariate view reveals some structure

savingby2d <- function (x, y = NULL, method = "default")
{

  ## Organise inputs

  if(is.data.frame(x) && ncol(x) > 2L)
    stop("'x' should have max 2 columns.")
  if (is.null(y) && identical(ncol(x), 2L)){
    y <- x[, 2L]
    x <- x[, 1L]
  }
  x <- if (is.data.frame(x))
    x[, 1L]
  else x
  y <- if (is.data.frame(y))
    y[, 1L]
  else y
  arefactors <- vapply(list(x, y), is.factor, logical(1L))

  ## Case 1: Two factors

  if (all(arefactors)){
    tab <- table(x, y)
    return(sum(tab != 0) / (ncol(tab) * nrow(tab)))
  } else {

  ## Case 2: One factor, one continuous

    if (any(arefactors)){
      if (is.factor(x)){
        fac <- x
        cont <- y
      } else {
        fac <- y
        cont <- x
      }
      totalarea <- abs(diff(range(cont)))
      weightbyfac <- table(fac) / length(fac)
      lengthbyfac <- vapply(levels(fac), function(x) {
        if (length(cont[as.character(fac) == x]) > 1)
          abs(diff(range(cont[as.character(fac) == x])))
        else 0
      }, numeric(1))
      hullarea <- sum(weightbyfac * lengthbyfac)
      return(hullarea / totalarea)
    } else {

  ## Case 3: Both continuous

  ## Default method compares area of convex hull to a bounding rectangle.

      if (identical(method, "default")){
        if (abs(cor(x, y)) > 0.995)
          return(0)
        x.scaled <- (x - mean(x)) / sd(x)
        y.scaled <- (y - mean(y)) / sd(y)
        totalarea <- abs(diff(range(x.scaled)) * diff(range(y.scaled)))
        conhull <- chull(x.scaled, y.scaled)
        hullarea <- polygonarea(x.scaled[conhull], y.scaled[conhull])
        return(hullarea / totalarea)
      } else {

  ## Scagnostic measure

        if (method %in% c("Outlying", "Skewed", "Clumpy", "Sparse", "Striated",
          "Convex", "Skinny", "Stringy", "Monotonic")){
          if (requireNamespace("scagnostics", quietly = TRUE)){
            ratio <- scagnostics::scagnostics.default(x, y)[method]
            if (method %in% c("Outlying", "Skewed", "Clumpy", "Sparse",
              "Striated", "Skinny", "Stringy", "Monotonic"))
              ratio <- 1 - ratio
            return(ratio)
          } else stop("requires package 'scagnostics'")
        } else {

  ## Density estimate confidence region

          if (identical(method, "DECR")){
            if (requireNamespace("hdrcde", quietly = TRUE)){
              o <- hdrcde::hdr.2d(x, y, prob = 0.05)
              return(sum(o$den$z > o$falpha) / length(o$den$z))
            } else stop("requires package 'hdrcde'")
          } else stop("unknown 'method' specified")
        }
      }
    }
  }
}
