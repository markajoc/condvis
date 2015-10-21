cont2color <-
function (x, range, breaks = NULL, colors = NULL)
{
    x <- if (!is.numeric(x)){
        as.numeric(x)
        warning("'x' has been coerced to numeric.")
    } else x
    breaks <- if(is.null(breaks))
	    11
    else breaks
	br <- c(min(x, min(range)) - 1,
	        seq(min(range), max(range), length.out = breaks - 1),
	        max(x, max(range)) + 1)
    colors <- if (is.null(colors))
        if (requireNamespace("RColorBrewer", quietly = TRUE))
		    RColorBrewer::brewer.pal(n = breaks, name = "PiYG")
		else cm.colors(breaks)
    else rep(colors, length.out = breaks)
    as.character(cut(x, br, labels = colors, include.lowest = TRUE))
}
