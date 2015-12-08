.mousemove.separate <-
function (buttons, x, y)
{
    if (version$os != "linux-gnu"){
    if (exists("buttons")){
	if (0 %in% buttons){
    expectationwindow <- get("expectationwindow", envir = parent.frame())
    selectorwindow <- get("selectorwindow", envir = parent.frame())
    plotxcobject <- get("plotxcobject", envir = parent.frame())
    plotxsobject <- get("plotxsobject", envir = parent.frame())
    Xc <- get("Xc", envir = parent.frame())
    Xc.cond <- plotxsobject$xc.cond
    screen.info <- getscreeninfo(plotxcobject)
    screen.index <- C.index <- xy2index(x, y, screen.info)
    
    if (length(screen.index) > 0){
        screen.num <- plotxcobject$screens[screen.index]
        par(bg = "white")
        screen(screen.num, new = F)
        par(mar = plotxcobject$xcplots[[screen.index]]$mar)
        par(usr = plotxcobject$xcplots[[screen.index]]$usr)
        plot.type <- plotxcobject$xcplots[[screen.index]]$plot.type
        xc <- plotxcobject$xcplots[[screen.index]]$xc
        xclick <- grconvertX(x, "ndc", "user")
        yclick <- grconvertY(y, "ndc", "user")
        if (identical(plot.type, "histogram")){
            varnames <- plotxcobject$xcplots[[screen.index]]$name
            xc.cond.new <- if (xclick < min(xc))
                min(xc)
            else if (xclick > max(xc))
                    max(xc)
                else xclick
            xc.cond.new <- data.frame(xc.cond.new)
        } else {
            if (identical(plot.type, "scatterplot")){
                varnames <- plotxcobject$xcplots[[screen.index]]$name
                xc.cond.new.x <- if (xclick < min(xc[, 1L]))
                    min(xc[, 1L])
                else if (xclick > max(xc[, 1L]))
                        max(xc[, 1L])
                    else xclick
                xc.cond.new.y <- if (yclick < min(xc[, 2L]))
                    min(xc[, 2L])
                else if (yclick > max(xc[, 2L]))
                        max(xc[, 2L])
                    else yclick
                xc.cond.new <- data.frame(xc.cond.new.x, xc.cond.new.y)
            } else {
                if (identical(plot.type, "barplot")){
                    varnames <- plotxcobject$xcplots[[screen.index]]$name
                    factorcoords <- 
                        plotxcobject$xcplots[[screen.index]]$factorcoords
                    xc.cond.new <- 
                        data.frame(as.factor(factorcoords$level)[which.min( 
                            abs(xclick - factorcoords$x) )])
                } else {
                    if(identical(plot.type, "boxplot")){
                        varnames <- plotxcobject$xcplots[[screen.index]]$name
                        factorcoords <- 
                            plotxcobject$xcplots[[screen.index]]$factorcoords
                        xc.cond.new.fact <- 
                            as.factor(factorcoords$level)[which.min( 
                                abs(xclick - factorcoords$x) )]
                        xc.cond.new.cont <- if (yclick < min(xc[, 2L]))
                            min(xc[, 2L])
                        else if (yclick > max(xc[, 2L]))
                                max(xc[, 2L])
                            else yclick
                        xc.cond.new <- data.frame(xc.cond.new.fact, 
                            xc.cond.new.cont)
                    } else {
                        if (identical(plot.type, "spineplot")){
                            varnames <- plotxcobject$xcplots[[screen.index]]$name
                            sptmp <- plotxcobject$xcplots[[screen.index]]$sptmp
                            rectcoords <- data.frame(sptmp$xleft, sptmp$xright, 
                                sptmp$ybottom, sptmp$ytop)
                            if (c(xclick, yclick) %inrectangle% 
                                c(min(sptmp$xleft), max(sptmp$xright) ,
                                min(sptmp$ybottom), max(sptmp$ytop)) ){
                                comb.index <- apply(rectcoords, 1L, 
                                    `%inrectangle%`, point = c(xclick, yclick))
                                if (any(comb.index)){
                                    xc.cond.new <- data.frame( 
                                        as.factor(sptmp$xnames)[comb.index], 
									    as.factor(sptmp$ynames)[comb.index] )
                                } #else xc.cond.new <- Xc.cond[1L, varnames]    
                            } #else xc.cond.new <- Xc.cond[1L, varnames]                             
                        } 
                    }
                }
            }
        }
    }
    if (exists("xc.cond.new") && exists("varnames")){ 
        screen(screen.num, new = T)
	    colnames(xc.cond.new) <- varnames
        Xc.cond[1L, varnames] <- xc.cond.new
        plotxc(xc = xc, xc.cond = Xc.cond[1L, varnames], name = varnames, 
               select.colour = plotxcobject$xcplots[[screen.index]]$select.colour,
               select.lwd = plotxcobject$xcplots[[screen.index]]$select.lwd) 
        dev.set(expectationwindow)
        dev.hold()
        par(bg = "white") 
        close.screen(all.screens = TRUE)
        xsplot <- plotxsobject
        xsplot$xc.cond <- Xc.cond
        vwargs <- get("vwargs", envir = parent.frame())
        vw <- visualweight(xc = Xc, xc.cond = Xc.cond, 
            sigma = vwargs$sigma, distance = vwargs$distance)
        k <- vw$k
        xsplot$data.colour <- rgb(1-k,1-k,1-k)
        xsplot$data.order <- vw$order
        do.call(plotxs, xsplot)
        dev.flush()
	    assign(x = "Xc.cond", value = Xc.cond, envir = parent.frame())
        assign(x = "plotxsobject", value = xsplot, envir = parent.frame())              
        assign(x = "Xc.cond", value = Xc.cond, envir = parent.frame())
	} 
    
    #dev.set(expectationwindow)
    #points(runif(1), runif(1), col = sample(rainbow(12), 1))
    dev.set(selectorwindow)
    }}}
    points(NULL)
}