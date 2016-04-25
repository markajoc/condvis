makepath <-
# first attempt at making a path for conditional tour, for continuous
# variables only
# Have now added possibility for factors
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
