factormatchratio <-
function(x, X)
{
  x <- as.matrix(x)
  X <- if (is.null(dim(X)))
    matrix(X, ncol = length(x))
  else as.matrix(X)
  mp <- rowSums(X == matrix(x, ncol = length(x), nrow = nrow(X), byrow = TRUE))
    / length(x)
}
