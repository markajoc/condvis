interactiveceplot <- 
function (height = 7, width = 10, separate = FALSE, ...)
{
    if (separate){
        separate(...)
    } else {
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
        if (class(tmp$model)[1L] != "stanfit")
            assign(x = "plotobject", value = do.call(staticceplot, args = tmp), envir = eventEnv)
        else assign(x = "plotobject", value = do.call(stanplot, args = tmp), envir = eventEnv)
        getGraphicsEvent()
        on.exit(cat("\nInteractive session ended.\n")) 
    }
}
