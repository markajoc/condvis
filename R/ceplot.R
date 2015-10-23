ceplot <-
function (data, model, response = NULL, S = NULL, C = NULL, sigma = NULL, 
    threshold = NULL, type = "euclidean", cex.axis = NULL, cex.lab = NULL, tck = NULL, 
    view3d = FALSE, method = "default")
{
    model <- if (!identical(class(model), "list"))
        list(model)
    else model
    model.name <- if (!is.null(names(model)))
        names(model)
    else NULL
    varnamestry <- try(getvarnames(model[[1]]), silent = TRUE)
    response <- if (is.null(response))
        if (class(varnamestry) != "try-error")
           which(colnames(data) == varnamestry$response[1])
        else stop("could not extract response from 'model'.")
    else if (is.character(response))
            which(colnames(data) == response)
        else response
    S <- if(is.null(S)){
         (1:ncol(data))[-response][1L]
        #cat(paste("\n'S' was not specified, picked", colnames(data)[S]))
        } else if (is.character(S))
            vapply(S, function(x) which(colnames(data) == x), numeric(1))
            else S
    C <- if (is.null(C))
        if (class(varnamestry) != "try-error"){
            possibleC <- unique(unlist(lapply(
                lapply(model, getvarnames), `[[`, 2)))
            arrangeC(data[, possibleC[!(possibleC %in% colnames(data)[S])], 
                drop = FALSE], method = method)
        } else arrangeC(data[, -c(response, S)])
    else C
    C <- if (all(vapply(C, is.numeric, logical(1))))
        as.list(C)
    else if (all(vapply(C, is.character, logical(1))))
            lapply(C, names2index, names = colnames(data))
        else
            stop("'C' should be a vector or list (containing vectors of length",
                 " 1 or 2) with integer column indices or character variable",
                 " names from 'data'.")
    uniqC <- unique(unlist(C))
    if (any(response %in% uniqC))
        stop("cannot have 'response' variable in 'C'")
    if (any(response %in% S))
        stop("cannot have 'response' variable in 'S'")
    if (!identical(length(unique(vapply(lapply(model, getvarnames), 
        `[[`, character(1), 1))), 1L))
        stop("cannot compare models with different response variables") 
    if (!identical(length(intersect(S, uniqC)), 0L))
        stop("cannot have variables common to both 'S' and 'C'")    
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
    if (length(uniqC) > 0){
        for(C.index in seq_along(C)){
            screen(selectors[C.index])
            xcplots[[C.index]] <- plotxc(xc = data[, C[[C.index]]], xc.cond = 
                data[1L, C[[C.index]]], name = colnames(data)[C[[C.index]]],
                select.colour = "blue", select.lwd = 2, cex.axis = cex.axis, 
                cex.lab = cex.lab, tck = tck)
        }
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
        model.name = model.name, yhat = NULL, mar = NULL, 
        data.colour = data.colour, data.order = data.order, view3d = view3d)
    dev.flush()
    output <- list(Xc = Xc, sigma = sigma, threshold = threshold, type = type, 
        xcplots = xcplots, xsplot = xsplot,
	    screens = list(main = main, selectors = selectors))
	class(output) <- "ceplot"
	output
}