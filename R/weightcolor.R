weightcolor <-
function(col, weights)
{
  n <- length(weights)
  col <- rep(col, n)
  weightsgr0 <- which(weights > 0)
  data.order <- weightsgr0[order(weights[weightsgr0])]
  newcol <- (col2rgb(col[data.order]) * matrix(rep(weights[data.order], 3),
    nrow = 3, byrow = TRUE) / 255) + matrix(rep(1 - weights[data.order], 3),
    nrow = 3, byrow = TRUE)
  data.colour <- rep(NA, n)
  data.colour[data.order] <- rgb(t(newcol))
  structure(data.colour, order = data.order)
}
