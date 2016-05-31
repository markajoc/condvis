#' @title Make a default path for conditional tour
#'
#' @description Provides a default path (a set of sections) as input to a
#'   conditional tour. Clusters the data using k-means or partitioning around
#'   medoids (from the \code{cluster} package). The cluster centres/prototypes
#'   are then ordered to create a sensible way to visit each section as smoothly
#'   as possible. Ordering uses either the \code{DendSer} or \code{TSP} package.
#'   Linear interpolation is then used to create intermediate points between the
#'   path nodes.
#'
#' @param Xc A dataframe
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

makepath <-
## TODO: rename this function to defaultpath or something
function (Xc, ncentroids, ninterp = 4)
{
  if (any(arefactors <- vapply(Xc, is.factor, logical(1L)))){
    if (identical(ninterp %% 2, 0))
      ninterp <- ninterp + 1
  }
  interp <- function(x, n = ninterp)
  {
    out <- vector()
    if (is.factor(x)){
      for (i in 1:(length(x) - 1L)){
        out <- c(out, rep(x[i], ceiling(n / 2)), rep(x[i + 1], floor(n / 2)))
      }
      return(factor(levels(x)[c(out)], levels = levels(x)))
    } else {
      for (i in 1:(length(x) - 1L)){
        out <- c(out, seq(x[i], x[i + 1L], length.out = n + 1L)[-(n + 1L)])
      }
    }
    out
  }
  if (any(arefactors)){
    if (!requireNamespace("cluster", quietly = TRUE))
      stop("requires package 'cluster'")
    d <- cluster::daisy(Xc)
    clustering <- cluster::pam(d, k = ncentroids)
    centers <- Xc[clustering$medoids, ]
    if (!requireNamespace("DendSer", quietly = TRUE)){
      warning("requires package 'DendSer' to order path, left unordered")
    } else {
      d.centers <- cluster::daisy(centers)
      h <- hclust(d.centers, method = "single")
      o <- DendSer::DendSer(h, d.centers)
      centers <- centers[o, ]
    }
    path <- as.data.frame(lapply(centers, interp))
  } else {
    if (!requireNamespace("TSP", quietly = TRUE))
      stop("requires package 'TSP'")
    means <- colMeans(Xc)
    sds <- apply(Xc, 2L, sd)
    Xc <- scale(Xc)[, ]
    clustering <- kmeans(Xc, centers = ncentroids)
    centers <- clustering$centers
    o <- TSP::TSP(dist(centers))
    orderindex <- TSP::solve_TSP(o)
    centers <- centers[orderindex, , drop = FALSE]
    centers <- as.data.frame(t(apply(t(apply(centers, 1L, `*`, sds)), 1L, `+`,
      means)))
    path <- as.data.frame(apply(centers, 2L, interp))
  }
  list(centers = centers, path = path)
}
