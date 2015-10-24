separate <- 
function (data, model, response = NULL, S = NULL, C = NULL, sigma = NULL, 
    threshold = NULL, type = "euclidean", cex.axis = NULL, cex.lab = NULL, tck = NULL, 
    view3d = FALSE, method = "default", selectortype = "minimal")
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
            lapply(C, match, table = colnames(data))
        else
            stop("'C' should be a vector or list (containing vectors of length",
                 " 1 or 2) with integer column indices or character variable",
                 " names from 'data'.")
    uniqC <- unique(unlist(C))
    n.selector.cols <- ceiling(length(C) / 4L)
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
    Xc <- data[, uniqC, drop = FALSE]
    close.screen(all = TRUE)
    if (identical(version$os, "linux-gnu"))
        x11(type = "Xlib")
    else
        x11()
    vw <- visualweight(Xc, Xc.cond, sigma, threshold, type = type)
    k <- vw$k
    data.colour <- rgb(1 - k, 1 - k, 1 - k)
    data.order <- vw$order
    close.screen(all = TRUE)
    plotxsobject <- plotxs(xs = data[, S, drop = FALSE],
        y = data[, response, drop = FALSE], xc.cond = Xc.cond, model = model,
        model.colour = NULL, model.lwd = NULL, model.lty = NULL,
        model.name = model.name, yhat = NULL, mar = NULL, 
        data.colour = data.colour, data.order = data.order, view3d = view3d)
    expectationwindow <- dev.cur()
    height <- if (identical(selectortype, "pcp"))
        3
    else if (identical(selectortype, "full")) 
        6 
    else 2 * ceiling(length(C) / n.selector.cols)
    width <- if (identical(selectortype, "pcp"))
        7
    else if (identical(selectortype, "full")) 
        height   
    else 2 * n.selector.cols        
    if (identical(version$os, "linux-gnu"))
        x11(type = "Xlib", height = height, width = width)
    else
        x11(height = height, width = width)
    selectorwindow <- dev.cur() 
    if (identical(selectortype, "minimal")){   
	    setGraphicsEventHandlers(
            onMouseDown = if (exists(".mouseclick.separate")) .mouseclick.separate,
            onMouseUp = if (exists(".mouserelease.separate")) .mouserelease.separate,
            onMouseMove = if (exists(".mousemove.separate")) .mousemove.separate,
            onKeybd = if (exists(".keystroke.separate")) .keystroke.separate)
    } else { 
        if (identical(selectortype, "pcp")){   
	        setGraphicsEventHandlers(
                onMouseDown = if (exists(".mouseclick.separate.pcp")) .mouseclick.separate.pcp,
                onMouseUp = if (exists(".mouserelease.separate.pcp")) .mouserelease.separate.pcp,
                onMouseMove = if (exists(".mousemove.separate.pcp")) .mousemove.separate.pcp,
                onKeybd = if (exists(".keystroke.separate")) .keystroke.separate)
        } else {
            if (identical(selectortype, "full")){
	            setGraphicsEventHandlers(
                    onMouseDown = if (exists(".mouseclick.separate.full")) .mouseclick.separate.full,
                    onMouseUp = if (exists(".mouserelease.separate.full")) .mouserelease.separate.full,
                    onMouseMove = if (exists(".mousemove.separate.full")) .mousemove.separate.full,
                    onKeybd = if (exists(".keystroke.separate.full")) .keystroke.separate)
            }
        }
    }
    eventEnv <- getGraphicsEventEnv()
	assign(x = "plotxcobject", value = conditionselectors(Xc, type = 
        selectortype, select.colour = "blue"), envir = eventEnv)
    assign(x = "plotxsobject", value = plotxsobject, envir = eventEnv)
    assign(x = "expectationwindow", value = expectationwindow, envir = eventEnv)
    assign(x = "selectorwindow", value = selectorwindow, envir = eventEnv)
    assign(x = "Xc", value = Xc, envir = eventEnv)
    assign(x = "vwargs", value = list(sigma = sigma, threshold = threshold, type = type), envir = eventEnv)
    getGraphicsEvent()
    #on.exit(cat("\nInteractive session ended")) 
    on.exit(dev.off())  
}
