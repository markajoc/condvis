stanplot <-
function (data, model, response = NULL, S = NULL, C = NULL, sigma = NULL, 
    threshold = NULL, type = "euclidean", cex.axis = NULL, cex.lab = NULL, tck = NULL, 
    view3d = FALSE, method = "default")
{
    data <- na.omit(data)
    C <- if (all(vapply(C, is.numeric, logical(1))))
        as.list(C)
    else if (all(vapply(C, is.character, logical(1))))
            lapply(C, match, table = colnames(data))
        else
            stop("'C' should be a vector or list (containing vectors of length",
                 " 1 or 2) with integer column indices or character variable",
                 " names from 'data'.")
    uniqC <- unique(unlist(C))
    Xc.cond <- data[1, uniqC, drop = FALSE]
    xcplots <- list()
    close.screen(all.screens = T)
    n.selector.cols <- ceiling(length(C) / 4L)
    select.colwidth <- max(min(0.15 * n.selector.cols, 0.45), 0.2)
    main <- split.screen(figs = matrix(c(0, 1 - (select.colwidth),
                         1 - (select.colwidth), 1, 0, 0, 1, 1), ncol = 4))
    selectors <- split.screen(
        figs = c(max(ceiling(length(C) / n.selector.cols), 3), n.selector.cols),
        screen = main[2])
    dev.hold()
    for(C.index in seq_along(C)){
        screen(selectors[C.index])
        xcplots[[C.index]] <- plotxc(xc = data[, C[[C.index]]], xc.cond = 
            data[1L, C[[C.index]]], name = colnames(data)[C[[C.index]]],
            select.colour = "blue", select.lwd = 2, cex.axis = cex.axis, 
            cex.lab = cex.lab, tck = tck)
    }
    screen(main[1])
    Xc <- data[, uniqC, drop = FALSE]
    vw <- visualweight(xc = Xc, xc.cond = Xc.cond, sigma = sigma, threshold = 
        threshold, type = type)
    k <- vw$k
    data.colour <- rgb(1 - k, 1 - k, 1 - k)
    data.order <- vw$order
    xsplot <- plotxs(xs = data[, S, drop = FALSE],
        y = data[, response, drop = FALSE], xc.cond = Xc.cond, model = model,
        model.colour = NULL, model.lwd = NULL, model.lty = NULL,
        model.name = NULL, yhat = NULL, mar = NULL, 
        data.colour = data.colour, data.order = data.order, view3d = view3d)
    dev.flush()
    output <- list(Xc = Xc, sigma = sigma, threshold = threshold, type = type, 
        xcplots = xcplots, xsplot = xsplot,
	    screens = list(main = main, selectors = selectors))
	class(output) <- "ceplot"
	output
}