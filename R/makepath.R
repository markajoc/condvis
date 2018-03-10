#' @title Make a default path for conditional tour
#'
#' @description Provides a default path (a set of sections), useful as input to
#'   a conditional tour (\code{\link{condtour}}). Clusters the data using
#'   k-means or partitioning around medoids (from the \code{cluster} package).
#'   The cluster centres/prototypes are then ordered to create a sensible way to
#'   visit each section as smoothly as possible. Ordering uses either the
#'   \code{DendSer} or \code{TSP} package. Linear interpolation is then used to
#'   create intermediate points between the path nodes.
#'
#' @param x A dataframe
#' @param ncentroids The number of centroids to use as path nodes.
#' @param ninterp The number of points to linearly interpolate between path
#'   nodes.
#'
#' @return A list with two dataframes: \code{centers} giving the path nodes, and
#'   \code{path} giving the full interpolated path.
#'
#' @examples
#' d <- data.frame(x = runif(500), y = runif(500))
#' plot(d)
#' mp1 <- makepath(d, 5)
#' points(mp1$centers, type = "b", col = "blue", pch = 16)
#' mp2 <- makepath(d, 40)
#' points(mp2$centers, type = "b", col = "red", pch = 16)
#'
#' @seealso \code{\link{condtour}}

makepath <-
function (x, ncentroids, ninterp = 4)
{
  ## If we have factors, make sure 'ninterp' is odd.

  if (any(arefactors <- vapply(x, is.factor, logical(1L)))){
    if (identical(ninterp %% 2, 0))
      ninterp <- ninterp + 1
  }

  ## If we have factors, do partitioning around medoids (PAM) using the daisy
  ## distance from the 'cluster' package.

  if (any(arefactors)){
    if (!requireNamespace("cluster", quietly = TRUE))
      stop("requires package 'cluster'")
    d <- cluster::daisy(x)
    clustering <- cluster::pam(d, k = ncentroids)
    centers <- x[clustering$medoids, , drop = FALSE]

    ## Order the cluster centres using 'DendSer' if available.

    if (!requireNamespace("DendSer", quietly = TRUE)){
      warning("requires package 'DendSer' to order path, left unordered")
    } else {
      d.centers <- cluster::daisy(centers)
      h <- hclust(d.centers, method = "single")
      o <- DendSer::DendSer(h, d.centers)
      centers <- centers[o, , drop = FALSE]
    }
    path <- as.data.frame(lapply(centers, interpolate, ninterp = ninterp))
  } else {

    ## For all continuous variables, cluster with 'kmeans'

    ## Order the cluster centres with 'TSP'.

    if (!requireNamespace("TSP", quietly = TRUE))
      stop("requires package 'TSP'")
    x <- scale(x)
    means <- attr(x, "scaled:center")
    sds <- attr(x, "scaled:scale")
    #means <- colMeans(x)
    #sds <- apply(x, 2L, sd)
    #x <- scale(x)[, ]
    clustering <- kmeans(x[, ], centers = ncentroids)
    centers <- clustering$centers
    o <- TSP::TSP(dist(centers))
    orderindex <- TSP::solve_TSP(o)
    centers <- centers[orderindex, , drop = FALSE]
    centers <- as.data.frame(t(apply(t(apply(centers, 1L, `*`, sds)), 1L, `+`,
      means)))
    rownames(centers) <- NULL
    path <- as.data.frame(apply(centers, 2L, interpolate, ninterp = ninterp))
  }

  ## Return the cluster centres and the interpolated path.

  list(centers = centers, path = path)
}
