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
    if (identical(key, "s")){
        plotobject <- get("plotobject", envir = parent.frame())
        lenC <- length(plotobject$xcplots)
        Xc.cond <- plotobject$xsplot$xc.cond
        n.selector.cols <- ceiling(lenC / 4L)
        select.colwidth <- max(min(0.18 * n.selector.cols, 0.45), 0.2)
        select.lwd <- plotobject$xcplots[[1]]$select.lwd
        select.col <- plotobject$xcplots[[1]]$select.col
        sigma <- if (is.null(plotobject$sigma))
            1
        else plotobject$sigma 
        filename <- paste("snapshot_", gsub(":", ".", gsub(" ", "_", Sys.time())), ".pdf", sep = "") 
        #width <- 10 + 2 * n.selector.cols  
        width <- dev.size()[1]  
        height <- dev.size()[2]         
        pdf(file = filename, width = width, height = height)
        close.screen(all.screens = T)
        main <- split.screen(figs = matrix(c(0, 1 - (select.colwidth), 1 - (select.colwidth), 1, 0, 0, 1, 1), ncol = 4))
        selectors <- split.screen(figs = c(max(ceiling(lenC / n.selector.cols), 3), n.selector.cols),
        screen = main[2])   
        screen(main[1])
        do.call(plotxs, plotobject$xsplot)
        for (i in 1:lenC){
            screen(selectors[i])
            name <- plotobject$xcplots[[i]]$name
            plotxc(xc = plotobject$xcplots[[i]]$xc, xc.cond = Xc.cond[1, name], 
                select.lwd = select.lwd, select.col = select.col, name = name)
        }
        dev.off()
        cat(paste("\nSnapshot saved: '", filename,"'", sep = ""))
        cat(paste("\ndistance: ", plotobject$type, ", sigma: ", sigma, sep = ""))
    }
    points(NULL)
}
