#' @title Assign colours to factor vector
#'
#' @description This function takes a factor vector and returns suitable colours
#'   representing the factor levels. Default is to try to use RColorBrewer for
#'   colours, and \code{rainbow} otherwise. Can provide custom colours.
#'
#' @param x A factor vector.
#' @param colors The colours to use. Defaults to a qualitative colour scheme;
#'   either \code{"Set3"} from \code{RColorBrewer} if available, or
#'   \code{\link{rainbow}} otherwise.
#'
#' @return A character vector of colours.
#'
#' @details Uses the \code{RColorBrewer} package if installed. Coerces \code{x}
#'   to factor with a warning.
#'
#' @examples
#' plot(iris[, c("Petal.Length", "Petal.Width")], pch = 21,
#'   bg = factor2color(iris$Species))
#' legend("topleft", legend = levels(iris$Species),
#'   fill = factor2color(as.factor(levels(iris$Species))))

factor2color <-
function (x, colors = NULL)
{
  x <- if (!is.factor(x)){
    as.factor(x)
    warning("'x' has been coerced to a factor.")
  } else x
  n <- nlevels(x)
  colors <- if (is.null(colors)){
    if (requireNamespace("RColorBrewer", quietly = TRUE))
	    RColorBrewer::brewer.pal(n = max(n, 3L, na.rm = TRUE), name = "Set3")[1L:
        n]
		else rainbow(n)
  } else rep(colors, length.out = n)
  vapply(x, function(y) colors[levels(x) == as.character(y)], character(1L))
}
