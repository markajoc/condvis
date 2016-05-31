## This is a helper function used only in savingby2d. Calculates the area of a
## polygon coming out of grDevices::chull

polygonarea <- function (x, y = NULL)
{
  if (is.null(y) && identical(ncol(x), 2L)){
    y <- x[, 2L]
    x <- x[, 1L]
  }
  area <- 0
  n <- length(x)
  j <- n
  for (i in 1:n){
    area <- area + (x[j] + x[i]) * (y[j] - y[i])
    j <- i
  }
  abs(area) / 2
}
