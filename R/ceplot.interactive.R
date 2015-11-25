ceplot.interactive <- 
function (height = 7, width = 10, ...)
{
        if (identical(version$os, "linux-gnu"))
            x11(type = "Xlib", height = height, width = width)
        else
            x11(height = height, width = width)
	    setGraphicsEventHandlers(
            onMouseDown = if (exists(".mouseclick.ceplot")) .mouseclick.ceplot,
            onMouseUp = if (exists(".mouserelease.ceplot")) .mouserelease.ceplot,
            onMouseMove = if (exists(".mousemove.ceplot")) .mousemove.ceplot,
            onKeybd = if (exists(".keystroke.ceplot")) .keystroke.ceplot)
        eventEnv <- getGraphicsEventEnv()
        tmp <- list(...)
        tmp <- tmp[!vapply(names(tmp), identical, logical(1), "selectortype")]
        assign(x = "plotobject", value = do.call(ceplot.static, args = tmp), envir = eventEnv)
        if(exists("Xc.cond.hist"))
            rm(Xc.cond.hist, envir = .GlobalEnv)
        getGraphicsEvent()
        on.exit(cat("\nInteractive session ended.\n")) 
}
