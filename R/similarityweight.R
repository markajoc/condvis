#' @title Calculate the similarity weight for a set of observations
#'
#' @description Calculate the similarity weight for a set of observations, based
#' on their distance from some arbitary points in data space. Observations which
#' are very similar to the point under consideration are given weight 1, while
#' dissimilar points are given weight zero.
#'
#' @param x This is a dataframe describing arbitrary points in the space of the
#'   data (i.e., with same \code{colnames} as \code{data}).
#' @param data This is a dataframe with same names as \code{xc.cond}
#'   representing observed data points.
#' @param threshold This is a threshold distance outside which observations will
#'   be assigned visual weight zero. This is numeric (and should be > 0).
#' @param distance The type of distance measure to be used, currently just two
#'   types of Minkowski distance: \code{"euclidean"} (default), and
#'   \code{"maxnorm"}.
#' @param constant A constant to multiply by the number of categorical
#'   mismatches, before adding to the Minkowski distance, to give a general
#'   dissimilarity measure.
#'
#' @return A numeric vector or matrix, with values between 0 and 1. The
#'   similarity weights for the observations in \code{data} arranged in rows for
#'   each row in \code{x}.
#'
#' @details Similarity weight is assigned to observations based on their
#'   distance from a given point. The distance is calculated as Minkowski
#'   distance between the numeric elements for the observations whose
#'   categorical elements match, with the option to use a more general
#'   dissimilarity measure comprising Minkowski distance and a mismatch count.
#'
#' @examples
#' ## Say we want to find observations similar to the first observation.
#' ## The first observation is identical to itself, so it gets weight 1. The
#' ## second observation is similar, so it gets some weight. The rest are more
#' ## different, and so get zero weight.
#'
#' similarityweight(x = mtcars[1, ], data = mtcars)
#'
#' ## By increasing the threshold, we can find observations which are more
#' ## approximately similar to the first row. Note that the second observation
#' ## now has weight 1, so we lose some ability to discern how similar
#' ## observations are by increasing the threshold.
#'
#' similarityweight(x = mtcars[1, ], data = mtcars, threshold = 5)
#'
#' ## Can provide a number of points to 'x'. Here we see that the Mazda RX4 Wag
#' ## is more similar to the Merc 280 than the Mazda RX4 is.
#'
#' similarityweight(mtcars[1:2, ], mtcars, threshold = 3)


similarityweight <-
function (x, data, threshold = NULL, distance = NULL, constant = NULL)
{
  vwfun <- .visualweight(xc = data)
  k <- matrix(nrow = nrow(x), ncol = nrow(data), dimnames = list(rownames(
    x), rownames(data)))
  rm(data)
  for (i in 1:nrow(x)){
    k[i, ] <- do.call(vwfun, list(xc.cond = x[i, ], sigma = threshold,
      distance = distance, constant = constant))$k
  }
  k[, , drop = TRUE]
}

.visualweight <-
function (xc)
{
  nrow.xc <- nrow(xc)
  if (nrow.xc < 2)
    stop("cannot apply scale to data.frame with less than 2 rows")
  colnames.xc <- colnames(xc)
  arefactors <- vapply(xc, is.factor, logical(1))
  xc.factors <- data.matrix(xc[, arefactors, drop = FALSE])
  xc.num <- data.matrix(xc[, !arefactors, drop = FALSE])
  rm(xc)
  x.scaled <- scale(xc.num)
  k <- rep(0, nrow.xc)
  function (xc.cond, sigma = NULL, distance = c("euclidean", "maxnorm"),
    constant = NULL)
  {
    sigma <- if (is.null(sigma))
      1
    else sigma
    distance <- match.arg(distance)
    constant <- if (is.null(constant))
      10 * sigma
    else sigma
    p <- if (identical(distance, "maxnorm")) 1 else 2
    xc.cond <- xc.cond[, colnames.xc, drop = FALSE]
    xc.cond.factors <- data.matrix(xc.cond[, arefactors, drop = FALSE])
    xc.cond.num <- data.matrix(xc.cond[, !arefactors, drop = FALSE])
    factormatches <- if (any(arefactors)){
      which((nfactormatches <- rowSums(xc.factors == matrix(xc.cond.factors,
        ncol = length(xc.cond.factors), nrow = nrow.xc, byrow = TRUE))) ==
        length(xc.cond.factors))
    } else {rep(TRUE, nrow.xc)}
    if (length(factormatches) > 0){
      if (all(arefactors)){
        k[factormatches] <- 1
      } else {
        xcond.scaled <- (xc.cond.num - attr(x.scaled, "scaled:center")) / attr(
          x.scaled, "scaled:scale")
        d <- dist1(xcond.scaled, x.scaled[factormatches, ], inf = identical(
          distance, "maxnorm")) + if (any(arefactors)) constant * (sum(
          arefactors) - nfactormatches[factormatches]) else 0
        k[factormatches] <- c(1, 0.7, 0.4, 0)[findInterval(d, c(0, (0.3 * sigma)
          ^ p, (0.6 * sigma) ^ p, sigma ^ p))]
      }
    }
    list(k = k, sigma = sigma, distance = distance)
  }
}