## Function to weight colours according to a weight vector. Not exported.

weightcolor <-
function(col, weights, breaks)
{
  n <- length(weights)
  col <- rep(col, length.out = n)

  ## Discretise `weights`. We just want 3 different shades, as it becomes
  ## difficult to differentiate between them otherwise.

  weights <- c(0, 0.4, 0.7, 1)[findInterval(weights, c(0, .Machine$double.eps,
    0.4, 0.7, 1), rightmost.closed = TRUE)]

  ## We won't perform calculations on elements with `weight` == 0.

  weightsgr0 <- which(weights > 0)
  data.order <- weightsgr0[order(weights[weightsgr0])]

  ## Linearly fade the colours in `col` to white in RGB space according to their
  ## `weights`.

  newcol <- (col2rgb(col[data.order]) * matrix(rep(weights[data.order], 3),
    nrow = 3, byrow = TRUE) / 255) + matrix(rep(1 - weights[data.order], 3),
    nrow = 3, byrow = TRUE)
  data.colour <- rep(NA, n)
  data.colour[data.order] <- rgb(t(newcol))

  ## Return the weighted colours with the order as attribute.

  structure(data.colour, order = data.order)
}
