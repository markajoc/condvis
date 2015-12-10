ceplot.interactive2 <- 
function (data, model, response = NULL, S = NULL, C = NULL, sigma = NULL, 
    distance = "euclidean", cex.axis = NULL, cex.lab = NULL, tck = NULL, 
    view3d = FALSE, Corder = "default", conf = FALSE)
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
            possibleC <- possibleC[possibleC %in% colnames(powerplant)]
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
    if (any(response %in% uniqC))
        stop("cannot have 'response' variable in 'C'")
    if (any(response %in% S))
        stop("cannot have 'response' variable in 'S'")
    if (!identical(length(intersect(S, uniqC)), 0L))
        stop("cannot have variables common to both 'S' and 'C'")    
    xc.cond <- data[1, uniqC, drop = FALSE]
 
    xcplots <- list()
    coords <- matrix(ncol = 4, nrow = length(C))    
    plotlegend <- length(S) == 2
    n.selector.cols <- ceiling(length(C) / 4L)
    selector.colwidth <- 2
    height <- 8
    width <- height + 0.5 * plotlegend + selector.colwidth * n.selector.cols
    
    if (identical(version$os, "linux-gnu"))
        x11(type = "Xlib", height = height, width = width)
    else
        x11(height = height, width = width)
    close.screen(all.screens = TRUE)
    xcwidth <- selector.colwidth * n.selector.cols / width
    mainscreens <- split.screen(figs = matrix(c(0, 1 - xcwidth, 1 - xcwidth, 1, 
        0, 0, 1, 1), ncol = 4))
    xcscreens <- split.screen(c(4, n.selector.cols), screen = mainscreens[2])
    for (i in seq_along(C)){
        screen(xcscreens[i])
        xcplots[[i]] <- plotxc(xc = data[, C[[i]]], xc.cond = data[1, C[[i]]], 
            name = colnames(data[, C[[i]], drop = FALSE]), select.colour = "blue")
        coords[i, ] <- par("fig")
    }    
    legendwidth <- 1 / height
    xsscreens <- if (plotlegend){
        split.screen(figs = matrix(c(0, 1 - legendwidth, 1 - legendwidth, 1, 
            0, 0, 1, 1), ncol = 4), screen = mainscreens[1])
    } else mainscreens[1]
    if (plotlegend){
        screen(xsscreens[2])
        xslegend(data[, response], colnames(data)[response])
    }
    screen(xsscreens[1])
    vw <- visualweight(xc = data[, uniqC, drop = FALSE], xc.cond = xc.cond, 
        sigma = sigma, distance = distance)
    par(mar = c(3,3,3,3))
    xsplot <- plotxs1(xs = data[, S, drop = FALSE], data[, response, 
        drop = FALSE], xc.cond = xc.cond, model = model, data.colour = rgb(1 - 
        vw$k, 1 - vw$k, 1 - vw$k), data.order = vw$order, view3d = view3d, 
        theta3d = 45, phi3d = 20, conf = conf)
    xscoords <- par("fig")  
    xold <- NULL
    yold <- NULL 
    mouseclick <- function ()
    {
        function (buttons, x, y)
        {
            plotindex <- which(apply(coords, 1, `%inrectangle%`, point = 
                c(x, y)))
            if (length(plotindex) > 0 && if(exists("buttons")) 0 %in% buttons){
                dev.hold()
                xcplots[[plotindex]] <<- update(xcplots[[plotindex]], x, y)
                xc.cond[, xcplots[[plotindex]]$name] <<- 
                    xcplots[[plotindex]]$xc.cond.old
                vw <<- visualweight(xc = data[, uniqC, drop = FALSE], xc.cond = 
                    xc.cond, sigma = sigma, distance = distance)
                xsplot <<- update(xsplot, xc.cond = xc.cond, data.colour = 
                    rgb(1 - vw$k, 1 - vw$k, 1 - vw$k), data.order = vw$order)
                dev.flush()
            }
            if (all(findInterval(x, xscoords[1:2]) == 1, identical(
                xsplot$plot.type, "ccc"), xsplot$view3d, 0 %in% buttons)){
                if (!is.null(xold))
                    xsplot <<- update(xsplot, theta3d = xsplot$theta3d + 1 * 
                        (xold > x) - 1 * (xold < x), phi3d = xsplot$phi3d + 1 * 
                        (yold > y) - 1 * (yold < y), xs.grid = xsplot$xs.grid, 
                        prednew = xsplot$prednew)
                xold <<- x
                yold <<- y                    
            }
        points(NULL)
        }
    }    
    keystroke <- function ()
    {
        function (key)
        {
            if (identical(key, "q")){
                cat("\nInteractive session ended.\n")
                return(invisible(1))            
            } 
            if (identical(xsplot$plot.type, "ccc") & xsplot$view3d & 
                key %in% c("Up", "Down", "Left", "Right")){
                xsplot <<- update(xsplot, theta3d = xsplot$theta3d - 2 * 
                    (key == "Right") + 2 * (key == "Left"), phi3d = xsplot$phi3d 
                    - 2 * (key == "Up") + 2 * (key == "Down"), xs.grid = 
                    xsplot$xs.grid, prednew = xsplot$prednew)                
            }
            if (identical(xsplot$plot.type, "ccc") & identical(key, "3"))
                xsplot <<- update(xsplot, view3d = !xsplot$view3d)
            if (key %in% c(",", ".")){
                sigma <<- sigma + 0.01 * sigma * (key == ".") - 0.01 * sigma * 
                    (key == ",")
                vw <<- visualweight(xc = data[, uniqC, drop = FALSE], 
                    xc.cond = xc.cond, sigma = sigma, distance = distance)
                xsplot <<- update(xsplot, data.colour = rgb(1 - vw$k, 1 - vw$k, 
                    1 - vw$k), data.order = vw$order, xs.grid = xsplot$xs.grid, 
                    prednew = xsplot$prednew)    
            }
            if (identical(key, "s")){
                filename <- paste("snapshot_", gsub(":", ".", gsub(" ", "_", Sys.time())), ".pdf", sep = "") 
                pdf(file = filename, width = width, height = height)
                close.screen(all.screens = TRUE)
                mainscreens <- split.screen(figs = matrix(c(0, 1 - xcwidth, 1 - xcwidth, 1, 
                    0, 0, 1, 1), ncol = 4))
                xcscreens <- split.screen(c(4, n.selector.cols), screen = mainscreens[2])
                for (i in seq_along(C)){
                    screen(xcscreens[i])
                    plotxc(xc = data[, C[[i]]], xc.cond = xcplots[[i]]$xc.cond, 
                        name = colnames(data[, C[[i]], drop = FALSE]), select.colour = "blue")
                }    
                xsscreens <- if (plotlegend){
                    split.screen(figs = matrix(c(0, 1 - legendwidth, 1 - legendwidth, 1, 
                        0, 0, 1, 1), ncol = 4), screen = mainscreens[1])
                } else mainscreens[1]
                if (plotlegend){
                    screen(xsscreens[2])
                    xslegend(data[, response], colnames(data)[response])
                }
                screen(xsscreens[1])
                plotxs1(xs = data[, S, drop = FALSE], data[, response, 
                    drop = FALSE], xc.cond = xc.cond, model = model, data.colour = rgb(1 - 
                    vw$k, 1 - vw$k, 1 - vw$k), data.order = vw$order, view3d = view3d, 
                    theta3d = 45, phi3d = 20, conf = conf)
               dev.off()    
                cat(paste("\nSnapshot saved: '", filename,"'", sep = ""))
                cat("\n")            
            }        
            points(NULL)
        }
    }      
    setGraphicsEventHandlers(
        onMouseDown = mouseclick(), 
        onMouseMove = mouseclick(), 
        onKeybd = keystroke())
    getGraphicsEventEnv()
    getGraphicsEvent()
}