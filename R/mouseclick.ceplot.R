.mouseclick.ceplot <-
function (buttons, x, y)
{
    dev.hold()
    plotobject <- get("plotobject", envir = parent.frame())
	Xc.cond <- if ("Xc.cond" %in% ls(parent.frame()))
	    get("Xc.cond", envir = parent.frame())
	else plotobject$xsplot$xc.cond
    screen.info <- getscreeninfo(plotobject)
    screen.index <- C.index <- xy2index(x, y, screen.info)
    if (length(screen.index) > 0){
        screen.num <- plotobject$screens$selectors[screen.index]
        par(bg = "white")
        screen(screen.num, new = F)
        par(mar = plotobject$xcplots[[screen.index]]$mar)
        par(usr = plotobject$xcplots[[screen.index]]$usr)
        plot.type <- plotobject$xcplots[[screen.index]]$plot.type
        xc <- plotobject$xcplots[[screen.index]]$xc
        xclick <- grconvertX(x, "ndc", "user")
        yclick <- grconvertY(y, "ndc", "user")
        if (identical(plot.type, "histogram")){
            varnames <- plotobject$xcplots[[screen.index]]$name
            xc.cond.new <- if (xclick < min(xc))
                min(xc)
            else if (xclick > max(xc))
                    max(xc)
                else xclick
            xc.cond.new <- data.frame(xc.cond.new)
        } else {
            if (identical(plot.type, "scatterplot")){
                varnames <- plotobject$xcplots[[screen.index]]$name
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
                    varnames <- plotobject$xcplots[[screen.index]]$name
                    factorcoords <- 
                        plotobject$xcplots[[screen.index]]$factorcoords
                    xc.cond.new <- 
                        data.frame(as.factor(factorcoords$level)[which.min( 
                            abs(xclick - factorcoords$x) )])
                } else {
                    if(identical(plot.type, "boxplot")){
                        varnames <- plotobject$xcplots[[screen.index]]$name
                        factorcoords <- 
                            plotobject$xcplots[[screen.index]]$factorcoords
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
                            varnames <- plotobject$xcplots[[screen.index]]$name
                            sptmp <- plotobject$xcplots[[screen.index]]$sptmp
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
        rownames(Xc.cond) <- NULL
        #Xc.cond.hist <<- if(exists("Xc.cond.hist"))
        #    rbind(Xc.cond.hist, Xc.cond)
        #else Xc.cond    
        plotxc(xc = xc, xc.cond = Xc.cond[1L, varnames], name = varnames, 
               select.colour = plotobject$xcplots[[screen.index]]$select.colour,
               select.lwd = plotobject$xcplots[[screen.index]]$select.lwd) 
        screen(plotobject$screen$main[1L])
        xsplot <- plotobject$xsplot
        xsplot$xc.cond <- Xc.cond
        vw <- visualweight(xc = plotobject$Xc, xc.cond = Xc.cond, 
            sigma = plotobject$sigma, distance = plotobject$distance)
        k <- vw$k
        xsplot$data.colour <- rgb(1-k,1-k,1-k)
        xsplot$data.order <- vw$order
        do.call(plotxs, xsplot)
	    assign(x = "Xc.cond", value = Xc.cond, envir = parent.frame())
        plotobject$xsplot <- xsplot
        assign(x = "plotobject", value = plotobject, envir = parent.frame())
	}
	dev.flush()
    points(NULL)
}
