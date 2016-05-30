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
