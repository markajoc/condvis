#' @title Minkowski distance
#'
#' @description Calculate Minkowski distance between one point and a set of
#'   other points.
#'
#' @param x A numeric vector describing point coordinates.
#' @param X A numeric matrix describing coordinates for several points.
#' @param p The power in Minkowski distance, defaults to 2 for Euclidean
#'   distance.
#' @param inf Logical; switch for calculating maximum norm distance (sometimes
#'   known as Chebychev distance) which is the limit of Minkowski distance as
#'   \eqn{p} tends to infinity.
#'
#' @return A numeric vector. These are distance^p, for speed of computation.
#'
#' @examples
#' x <- runif(5000)
#' y <- runif(5000)
#'
#' x1 <- 0.5
#' y1 <- 0.5
#'
#' dev.new(width = 4, height = 5.3)
#' par(mfrow = c(2, 2))
#'
#' for(p in c(0.5, 1, 2, 10)){
#'   d <- dist1(x = c(x1, y1), X = cbind(x, y), p = p) ^ (1/p)
#'   col <- rep("black", length(x))
#'   col[d < 0.3] <- "red"
#'   plot(x, y, pch = 16, col = col, asp = 1, main = paste("p = ", p, sep = ""))
#'}
#'
#' @seealso \code{\link{similarityweight}}


dist1 <-
function (x, X, p = 2, inf = FALSE)
{
  X <- if (is.null(dim(X)))
    matrix(X, ncol = length(x))
  else as.matrix(X)
  dif <- abs(X - matrix(as.numeric(x), nrow = nrow(X), ncol = ncol(X), byrow =
    TRUE))
  if (inf)
    return(apply(dif, 1, max))
  tmp <- dif ^ p
  rowSums(tmp)
}
