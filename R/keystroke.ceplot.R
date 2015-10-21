.keystroke.ceplot <-
function (key)
{
    if (identical(key, "q")) return(invisible(1))
    if (any(vapply(c("Up", "Down", "Left", "Right"), identical, logical(1), key))){
    plotobject <- get("plotobject", envir = parent.frame())
        if (plotobject$xsplot$view3d){
            try(plotobject$xsplot$xc.cond <- get("Xc.cond", envir = parent.frame()), silent = TRUE)
            if (identical(key, "Down"))
                plotobject$xsplot$phi3d <- plotobject$xsplot$phi3d + 2
            if (identical(key, "Up"))
                plotobject$xsplot$phi3d <- plotobject$xsplot$phi3d - 2
            if (identical(key, "Left"))
                plotobject$xsplot$theta3d <- plotobject$xsplot$theta3d + 2
            if (identical(key, "Right"))
                plotobject$xsplot$theta3d <- plotobject$xsplot$theta3d - 2
            par(bg="white")    
            screen(plotobject$screen$main[1L], new = TRUE)
            do.call(plotxs, plotobject$xsplot)
            assign(x = "plotobject", value = plotobject, envir = parent.frame())
        }    
    }
    points(NULL)
}
