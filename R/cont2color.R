#' @title Assign colours to numeric vector
#'
#' @description This function assigns colours on a linear scale to a numeric
#'   vector. Default is to try to use RColorBrewer for colours, and \code{
#'   cm.colors} otherwise. Can provide custom range, breaks and colours.
#'
#' @param x A numeric vector.
#' @param xrange The range to use for the colour scale.
#' @param breaks The number of breaks at which to change colour.
#' @param colors The colours to use. Defaults to a diverging colour scheme;
#'   either \code{"PiYG"} from \code{RColorBrewer} if available, or \code{\link{
#'   cm.colors}} otherwise.
#'
#' @return A character vector of colours.
#'
#' @details Uses the \code{RColorBrewer} package if installed.
#'
#' @examples
#' x <- runif(200)
#' plot(x, col = cont2color(x, c(0,1)))
#'
#' plot(x, col = cont2color(x, c(0,0.5)))
#'
#' plot(sort(x), col = cont2color(sort(x), c(0.25,0.75)), pch = 16)
#' abline(h = c(0.25, 0.75), lty = 3)

cont2color <-
function (x, xrange = NULL, breaks = NULL, colors = NULL)
{
  x <- if (!is.numeric(x)){
    as.numeric(x)
  warning("'x' has been coerced to numeric.")
  } else x
  xrange <- if (is.null(xrange))
    range(x)
  else xrange
  breaks <- if(is.null(breaks))
	  11
  else breaks
	br <- c(min(x, min(xrange)) - 1, seq(min(xrange), max(xrange), length.out =
    breaks - 1), max(x, max(xrange)) + 1)
  colors <- if (is.null(colors)){
    if (requireNamespace("RColorBrewer", quietly = TRUE))
	    RColorBrewer::brewer.pal(n = max(breaks, 3L, na.rm = TRUE), name = "PiYG")
		else cm.colors(breaks)
  } else rep(colors, length.out = breaks)
  as.character(cut(x, br, labels = colors, include.lowest = TRUE))
}
