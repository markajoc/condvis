condtour <-
function(data, model, path, response = NULL, S = NULL, C = NULL, sigma = NULL, 
    distance = "euclidean", cex.axis = NULL, cex.lab = NULL, tck = NULL, view3d 
    = FALSE, conf = FALSE)
{
    data <- na.omit(data)
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
        } else if (is.character(S))
            vapply(S, function(x) which(colnames(data) == x), numeric(1))
            else S       
    C <- if (is.null(C))
        arrangeC(data[, -c(response, S)])
    else C
    try(
        if (class(varnamestry) != "try-error"){
            possibleC <- unique(unlist(lapply(lapply(model, getvarnames), `[[`, 
                2)))
            possibleC <- possibleC[possibleC %in% colnames(data)]
            C <- arrangeC(data[, possibleC[!(possibleC %in% colnames(data)[S])], 
                drop = FALSE], method = Corder)
        }     
    , silent = TRUE) 
    C <- if (all(vapply(C, is.numeric, logical(1))))
        as.list(C)
    else if (all(vapply(C, is.character, logical(1))))
            lapply(C, match, table = colnames(data))
        else
            stop("'C' should be a vector or list (containing vectors of length",
                 " 1 or 2) with integer column indices or character variable",
                 " names from 'data'.")
    uniqC <- unique(unlist(C))
    C <- uniqC
    xc.cond <- path[1, , drop = FALSE]
    if (any(response %in% uniqC))
        stop("cannot have 'response' variable in 'C'")
    if (any(response %in% S))
        stop("cannot have 'response' variable in 'S'")
    if (!identical(length(intersect(S, uniqC)), 0L))
        stop("cannot have variables common to both 'S' and 'C'")
    xcplots <- list()
    coords <- matrix(ncol = 4L, nrow = length(C))    
    plotlegend <- length(S) == 2
    n.selector.cols <- ceiling(length(C) / 4L)
    selector.colwidth <- 2
    height <- 8    
    width <- height + 0.5 * plotlegend + selector.colwidth * n.selector.cols
    if (identical(version$os, "linux-gnu"))
        x11(type = "Xlib", height = height, width = height + 0.5 * plotlegend)
    else x11(height = height, width = height + 0.5 * plotlegend)
    devexp <- dev.cur()    
    close.screen(all.screens = TRUE)    
    
    legendwidth <- 1.15 / height
    xsscreens <- if (plotlegend){
        split.screen(figs = matrix(c(0, 1 - legendwidth, 1 - legendwidth, 1, 
            0, 0, 1, 1), ncol = 4))
    } else split.screen()
    if (plotlegend){
        screen(xsscreens[2L])
        xslegend(data[, response], colnames(data)[response])
    }
    screen(xsscreens[1L])
    vw <- visualweight(xc = data[, uniqC, drop = FALSE], xc.cond = xc.cond, 
        sigma = sigma, distance = distance)
    par(mar = c(3, 3, 3, 3))
    xsplot <- plotxs1(xs = data[, S, drop = FALSE], data[, response, 
        drop = FALSE], xc.cond = xc.cond, model = model, data.colour = rgb(1 
        - vw$k, 1 - vw$k, 1 - vw$k), data.order = vw$order, view3d = view3d, 
        conf = conf)
    xscoords <- par("fig") 
    xcwidth <- selector.colwidth * n.selector.cols
    n.selector.rows <- ceiling(length(C) / n.selector.cols)
    xcheight <- selector.colwidth * n.selector.rows    
    if (identical(version$os, "linux-gnu"))
        x11(type = "Xlib", height = xcheight, width = xcwidth)
    else x11(height = xcheight, width = xcwidth)
    devcond <- dev.cur()    
    close.screen(all.screens = TRUE)
    xcscreens <- split.screen(c(n.selector.rows, n.selector.cols))
    for (i in seq_along(uniqC)){
        screen(xcscreens[i])
        xcplots[[i]] <- plotxc(xc = data[, C[[i]]], xc.cond = data[1L, C[[i]]], 
            name = colnames(data[, C[[i]], drop = FALSE]), select.colour = 
            "blue")
        coords[i, ] <- par("fig")
    }         
}