#' @title Calculate the visual weight for a set of observations
#'
#' @description Calculate the visual weight for a set of observations, based on their
#' distance from some sections/conditions
#'
#' @param xc.cond This is a dataframe describing the sections/conditions.
#' @param xc This is a dataframe with same names as \code{xc.cond} representing
#'   observed data points.
#' @param sigma This is a threshold distance outside which observations will be
#'   assigned visual weight zero. This is numeric (and should be > 0).
#' @param distance The type of distance measure to be used, currently just two
#'   types of Minkowski distance: \code{"euclidean"} (default), and
#'   \code{"maxnorm"}.
#' @param constant A constant to multiply by the number of categorical
#'   mismatches, before adding to the Minkowski distance, to give a general
#'   dissimilarity measure.
#'
#' @return The visual weights for the observations in \code{xc} arranged in rows
#'   for each section in \code{xc.cond}.
#'
#' @details Visual weight is assigned to observations based on their
#'   distance from a section (treated as an observation here). The distance is
#'   calculated as Minkowski distance between the numeric elements for the
#'   observations whose categorical elements match, with the option to use a
#'   more general dissimilarity measure comprising Minkowski distance and a
#'   mismatch count.

visualweight <-
function (xc.cond, xc, sigma = NULL, distance = NULL, constant = NULL)
{
  vwfun <- visualweight2(xc = xc)
  k <- matrix(nrow = nrow(xc.cond), ncol = nrow(xc), dimnames = list(rownames(
    xc.cond), rownames(xc)))
  rm(xc)
  for (i in 1:nrow(xc.cond)){
    k[i, ] <- do.call(vwfun, list(xc.cond = xc.cond[i, ], sigma = sigma,
      distance = distance, constant = constant))$k
  }
  k[, , drop = TRUE]
}

visualweight2 <-
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
