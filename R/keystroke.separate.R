.keystroke.separate <-
function (key)
{
    if (identical(key, "q")) return(invisible(1))
    if (any(vapply(c("Up", "Down", "Left", "Right"), identical, logical(1), key))){
    expectationwindow <- get("expectationwindow", envir = parent.frame())
    selectorwindow <- get("selectorwindow", envir = parent.frame())
    plotxsobject <- get("plotxsobject", envir = parent.frame())
    Xc <- get("Xc", envir = parent.frame())
    Xc.cond <- plotxsobject$xc.cond
    if (plotxsobject$view3d){
            try(plotxsobject$xc.cond <- get("Xc.cond", envir = parent.frame()), silent = TRUE)
            if (identical(key, "Down"))
                plotxsobject$phi3d <- plotxsobject$phi3d + 2
            if (identical(key, "Up"))
                plotxsobject$phi3d <- plotxsobject$phi3d - 2
            if (identical(key, "Left"))
                plotxsobject$theta3d <- plotxsobject$theta3d + 2
            if (identical(key, "Right"))
                plotxsobject$theta3d <- plotxsobject$theta3d - 2
            dev.set(expectationwindow)
            par(bg = "white")
            close.screen(all.screens = TRUE)
            do.call(plotxs, plotxsobject)
            assign(x = "plotxsobject", value = plotxsobject, envir = parent.frame())
        }    
    }
    dev.set(selectorwindow)
    points(NULL)
}
