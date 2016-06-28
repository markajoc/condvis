#' @title Calculate the similarity weight for a set of observations
#'
#' @description Calculate the similarity weight for a set of observations, based
#' on their distance from some arbitary points in data space. Observations which
#' are very similar to the point under consideration are given weight 1, while
#' observations which are dissimilar to the point are given weight zero.
#'
#' @param x A dataframe describing arbitrary points in the space of the data
#'   (i.e., with same \code{colnames} as \code{data}).
#' @param data A dataframe with same names as \code{xc.cond} representing
#'   observed data points.
#' @param threshold Threshold distance outside which observations will
#'   be assigned similarity weight zero. This is numeric and should be > 0.
#' @param distance The type of distance measure to be used, currently just two
#'   types of Minkowski distance: \code{"euclidean"} (default), and
#'   \code{"maxnorm"}.
#' @param lambda A constant to multiply by the number of categorical
#'   mismatches, before adding to the Minkowski distance, to give a general
#'   dissimilarity measure.
#'
#' @return A numeric vector or matrix, with values from 0 to 1. The similarity
#'   weights for the observations in \code{data} arranged in rows for each row
#'   in \code{x}.
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
#' data(mtcars)
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
function (x, data, threshold = NULL, distance = NULL, lambda = NULL)
{
  ## Initialise the internal function

  vwfun <- .similarityweight(xc = data)

  ## Make empty matrix for weights

  k <- matrix(nrow = nrow(x), ncol = nrow(data), dimnames = list(rownames(
    x), rownames(data)))

  ## Loop through rows of 'x'

  for (i in 1:nrow(x)){
    k[i, ] <- do.call(vwfun, list(xc.cond = x[i, , drop = FALSE], sigma =
      threshold, distance = distance, lambda = lambda))$k
  }

  ## Return the matrix of weights, dropping to vector if possible

  k[, , drop = TRUE]
}

## Internal function which does some preprocessing (particularly scaling) and
## returns a function which calculates similarity weight for a single row of a
## dataframe.

.similarityweight <-
function (xc)
{
  ## Scale the dataframe and calculate a few things for later use.

  nrow.xc <- nrow(xc)
  if (nrow.xc < 2)
    stop("cannot apply scale to data.frame with less than 2 rows")
  colnames.xc <- colnames(xc)
  arefactors <- vapply(xc, is.factor, logical(1))
  xc.factors <- data.matrix(xc[, arefactors, drop = FALSE])
  xc.num <- data.matrix(xc[, !arefactors, drop = FALSE])
  x.scaled <- scale(xc.num)
  k <- rep(0, nrow.xc)

  ## Return a function which will calculate the weights for a single arbitrary
  ## point in the data space.

  function (xc.cond, sigma = NULL, distance = c("euclidean", "maxnorm"),
    lambda = NULL)
  {
    ## Set up values

    sigma <- if (is.null(sigma))
      1
    else sigma
    distance <- match.arg(distance)
    p <- if (identical(distance, "maxnorm")) 1 else 2

    ## If 'sigma' is Inf, return 1s for all observations

    if (identical(sigma, Inf))
      return(list(k = rep(1, nrow.xc), sigma = sigma, distance = distance))

    ## Get the arbitary point in order.

    xc.cond <- xc.cond[, colnames.xc, drop = FALSE]
    xc.cond.factors <- data.matrix(xc.cond[, arefactors, drop = FALSE])
    xc.cond.num <- data.matrix(xc.cond[, !arefactors, drop = FALSE])

    ## 'factormatches' is the index of observations on which we will calculate
    ## the Minkowski distance. Basically pre-filtering for speed.
    ##
    ## If 'lambda' is NULL, require all factors to be equal to even bother
    ## calculating Minkowski distance.
    ##
    ## If 'lambda' is supplied, only want observations with less than
    ## (sigma / lambda) mismatches in the factors.
    ##
    ## If there are no factors, want all rows.

    factormatches <- if (any(arefactors)){
      if (is.null(lambda)){
        which((nfactormatches <- rowSums(xc.factors == matrix(xc.cond.factors,
          ncol = length(xc.cond.factors), nrow = nrow.xc, byrow = TRUE))) ==
          length(xc.cond.factors))
      } else {
        which(length(xc.cond.factors) - (nfactormatches <- rowSums(xc.factors ==
          matrix(xc.cond.factors, ncol = length(xc.cond.factors), nrow = nrow.xc
          , byrow = TRUE))) <= (sigma / lambda))
      }
    } else {rep(TRUE, nrow.xc)}

    ## If any observations make it past the above filtering, calculate the
    ## dissimilarity 'd' as Minkowski distance plus 'lambda' times number of
    ## factor mismatches if 'lambda' is supplied.
    ##
    ## Convert the dissimilarity to similarity weights 'k', between 0 and 1.

    if (length(factormatches) > 0){
      if (all(arefactors)){
        if (is.null(lambda)){
          k[factormatches] <- 1
        } else {
          d <- lambda * (sum(arefactors) - nfactormatches[factormatches]) ^ p
          k[factormatches] <- c(1, 0.7, 0.4, 0)[findInterval(d, c(0, (0.3 *
            sigma) ^ p, (0.6 * sigma) ^ p, sigma ^ p))]
        }
      } else {
        xcond.scaled <- (xc.cond.num - attr(x.scaled, "scaled:center")) / attr(
          x.scaled, "scaled:scale")
        d <- dist1(xcond.scaled, x.scaled[factormatches, ], inf = identical(
          distance, "maxnorm")) + if (any(arefactors) && !is.null(lambda))
          (lambda * (sum(arefactors) - nfactormatches[factormatches])) ^ p
          else 0
        k[factormatches] <- c(1, 0.7, 0.4, 0)[findInterval(d, c(0, (0.3 * sigma)
          ^ p, (0.6 * sigma) ^ p, sigma ^ p))]
      }
    }
    list(k = k, sigma = sigma, distance = distance)
  }
}
