update.xcplot <- 
function (object, xclick, yclick, ...)
{
    if (dev.cur() != object$device)
        dev.set(object$device)   
    screen(n = object$screen, new = FALSE)
    par(usr = object$usr)
    par(mar = object$mar) 
    screen(n = object$screen, new = FALSE)
    xclickconv <- grconvertX(xclick, "ndc", "user")
    yclickconv <- grconvertY(yclick, "ndc", "user")
    if (identical(object$plot.type, "histogram")){
        xc.cond.new <- max(min(xclickconv, max(object$xc, na.rm = TRUE)), 
            min(object$xc, na.rm = TRUE), na.rm = TRUE)
        if (xc.cond.new != object$xc.cond.old){
            abline(v = object$xc.cond.old, lwd = 2 * object$select.lwd, 
                col = "white")
            break4redraw <- which.min(abs(object$histmp$breaks - 
                object$xc.cond.old))
            rect(xleft = object$histmp$breaks[break4redraw + c(-1, 0)], xright = 
                object$histmp$breaks[break4redraw + c(0, 1)], ybottom = c(0, 0), 
                ytop = object$histmp$counts[break4redraw + c(-1, 0)])
            abline(v = xc.cond.new, lwd = object$select.lwd, col = 
                object$select.col) 
            object$xc.cond.old <- xc.cond.new
        } 
    } else if (identical(object$plot.type, "barplot")){
        xc.cond.new <- as.factor(object$factorcoords$level)[which.min(abs(
            xclickconv - object$factorcoords$x))]
        if (xc.cond.new != object$xc.cond.old){
            barindex.old <- levels(object$xc) == object$xc.cond.old
            rect(xleft = object$bartmp$w.l[barindex.old], xright = 
                object$bartmp$w.r[barindex.old], ybottom = 0, ytop = 
                object$bartmp$height[barindex.old], col = "gray")
            barindex.new <- levels(object$xc) == xc.cond.new
            rect(xleft = object$bartmp$w.l[barindex.new], xright = 
                object$bartmp$w.r[barindex.new], ybottom = 0, ytop = 
                object$bartmp$height[barindex.new], col = object$select.colour, 
                density = 25)
            object$xc.cond.old <- xc.cond.new
        }
    } else if (identical(object$plot.type, "scatterplot")){
        xc.cond.new.x <- max(min(xclickconv, max(object$xc[, 1], na.rm = TRUE)), 
            min(object$xc[, 1], na.rm = TRUE), na.rm = TRUE)
        xc.cond.new.y <- max(min(yclickconv, max(object$xc[, 2], na.rm = TRUE)), 
            min(object$xc[, 2], na.rm = TRUE), na.rm = TRUE)
        xc.cond.new <- c(xc.cond.new.x, xc.cond.new.y)
        if (any(xc.cond.new != object$xc.cond.old)){
            if (nrow(object$xc) > 2000 && requireNamespace("gplots", quietly = 
                TRUE)){
                par(bg = "white")
                dev.hold()
                screen(new = TRUE)
                object <- plotxc(xc = object$xc, xc.cond = xc.cond.new, 
                    name = object$name, select.colour = 
                    object$select.colour, select.lwd = object$select.lwd, 
                    cex.axis = object$cex.axis, cex.lab = object$cex.lab, 
                    tck = object$tck)
                dev.flush()    
            } else {
            abline(v = object$xc.cond.old[1], h = object$xc.cond.old[2], lwd = 
                2 * object$select.lwd, col = "white")
            xrange <- abs(diff(range(object$xc[, 1])))
            yrange <- abs(diff(range(object$xc[, 2])))
            redrawindex.x <- findInterval(object$xc[, 1], object$xc.cond.old[1] 
                + xrange * c(-0.125, 0.125) ) == 1
            redrawindex.y <- findInterval(object$xc[, 2], object$xc.cond.old[2] 
                + yrange * c(-0.125, 0.125) ) == 1
            points(object$xc[redrawindex.x | redrawindex.y, ])
            box()
            abline(v = xc.cond.new.x, h = xc.cond.new.y, lwd = 
                object$select.lwd, col = object$select.colour)   
            object$xc.cond.old <- xc.cond.new
            }
        }
    } else if (identical(object$plot.type, "boxplot")){
        xc.cond.new.x <- as.factor(object$factorcoords$level)[
            which.min(abs(xclickconv - object$factorcoords$x))]
        xc.cond.new.y <- if (abs(yclickconv - object$xc.cond.old[, 2]) > 
            0.025 * abs(diff(range(object$xc[, 2])))){
            max(min(yclickconv, max(object$xc[, 2], na.rm = TRUE)), 
                min(object$xc[, 2], na.rm = TRUE), na.rm = TRUE)
        } else object$xc.cond.old[, 2]    
        xc.cond.new <- c(xc.cond.new.x, xc.cond.new.y)
        if (any(xc.cond.new != object$xc.cond.old)){
            if (xc.cond.new.x != object$xc.cond.old[, 1]){
                abline(v = as.integer(object$xc.cond.old[, 1]), lwd = 2 * 
                    object$select.lwd, col = "white")
            }
                
            if (xc.cond.new.y != object$xc.cond.old[, 2]) {
                abline(h = object$xc.cond.old[, 2], lwd = 2 * object$select.lwd, 
                    col = "white")  
            }   
            par(new = TRUE)
            bxp(object$boxtmp, xaxt = "n", yaxt = "n")
            abline(v = as.integer(xc.cond.new.x), h = xc.cond.new.y, lwd = 
                object$select.lwd, col = object$select.colour)
            xc.cond.new <- data.frame(xc.cond.new.x, xc.cond.new.y)
            names(xc.cond.new) <- names(object$xc.cond.old)
            object$xc.cond.old <- xc.cond.new
        }
    } else if (identical(object$plot.type, "spineplot")){
        sptmp <- object$sptmp
        rectcoords <- data.frame(sptmp$xleft, sptmp$xright, sptmp$ybottom, 
            sptmp$ytop)
        if (c(xclickconv, yclickconv) %inrectangle% c(min(sptmp$xleft), 
            max(sptmp$xright) , min(sptmp$ybottom), max(sptmp$ytop)) ){
            comb.index <- apply(rectcoords, 1L, `%inrectangle%`, point = 
                c(xclickconv, yclickconv))
            if (any(comb.index)){
                xc.cond.new <- data.frame(as.factor(sptmp$xnames)[comb.index], 
                    as.factor(sptmp$ynames)[comb.index])
                names(xc.cond.new) <- names(object$xc.cond.old)
                if (any(xc.cond.new != object$xc.cond.old)){
                    object$xc.cond.old <- xc.cond.new
                    par(bg = "white")
                    screen(new = TRUE)
                    object <- plotxc(xc = object$xc, xc.cond = xc.cond.new, 
                        name = object$name, select.colour = 
                        object$select.colour, select.lwd = object$select.lwd, 
                        cex.axis = object$cex.axis, cex.lab = object$cex.lab, 
                        tck = object$tck)
                }
            }   
        }        
    }
    object
}

update.xsplot <- 
function (object, xc.cond = NULL, data.colour = NULL, data.order = NULL, 
    view3d = NULL, theta3d = NULL, phi3d = NULL, xs.grid = NULL, prednew = NULL,
    ...)
{
    if (dev.cur() != object$device)
        dev.set(object$device)
    par(bg = "white")
    screen(n = object$screen, new = FALSE)
    par(usr = object$usr)
    par(mar = object$mar) 
    xc.cond <- if (!is.null(xc.cond))
        xc.cond
    else object$xc.cond
    data.colour <- if (!is.null(data.colour))
        data.colour
    else object$data.colour
    data.order <- if (!is.null(data.order))
        data.order
    else object$data.order
    view3d <- if (!is.null(view3d))
        view3d
    else object$view3d
    theta3d <- if (!is.null(theta3d))
        theta3d
    else object$theta3d
    phi3d <- if (!is.null(phi3d))
        phi3d
    else object$phi3d
    conf <- object$conf
    dev.hold()
    if (identical(object$plot.type, "cc")){
        if (any(xc.cond != object$xc.cond)){
            newdata <- makenewdata(xs = object$xs.grid, xc.cond = xc.cond)
            prednew <- lapply(object$model, predict1, newdata = newdata)
        } else {
            newdata <- object$newdata
            prednew <- object$prednew
        }
        screen(n = object$screen, new = FALSE)
        rect(object$usr[1], object$usr[3], object$usr[2], object$usr[4], col = 
            "white", border = NA)
        if (length(data.order) > 0)
            points(object$xs[data.order, 1L], object$y[data.order, 1L], col = 
                data.colour[data.order])
        for (i in seq_along(object$model)){
                points.default(object$xs.grid[, 1L], prednew[[i]], type = 'l',
                    col = object$model.colour[i], lwd = object$model.lwd[i], lty 
                    = object$model.lty[i]) 
        }                     
        if (conf){
            prednew2 <- lapply(object$model, confpred, newdata = newdata)     
            for (i in seq_along(object$model)){
                points.default(object$xs.grid[, 1L], prednew[[i]], type = 'l',
                    col = object$model.colour[i], lwd = object$model.lwd[i], lty 
                    = object$model.lty[i])
                if (all(c("lwr", "upr") %in% colnames(prednew2[[i]]))){
                    points.default(object$xs.grid[, 1L], prednew2[[i]][, "lwr"], 
                        type = 'l', lty = 2, col = object$model.colour[i], lwd = 
                        0.75 * object$model.lwd[i])
                    points.default(object$xs.grid[, 1L], prednew2[[i]][, "upr"], 
                        type = 'l', lty = 2, col = object$model.colour[i], lwd = 
                        0.75 * object$model.lwd[i])    
                }
            }        
        }    
        if (is.numeric(object$xs[, 1L])){
            pos <- if (cor(object$xs, object$y) < 0)
                "topright"
            else "bottomright"
            legend(pos, legend = object$model.name, col = object$model.colour, 
                lwd = object$model.lwd, lty = object$model.lty)
            } else {
                legend("topright", legend = object$model.name, col = 
                    object$model.colour, lwd = object$model.lwd, lty = 
                    object$model.lty)
            }    
        dev.flush()
        return(object)
    }
    if (!is.null(prednew)){
    screen(n = object$screen, new = TRUE)
    obj <- (plotxs1(xs = object$xs, y = object$y, xc.cond = xc.cond, 
        model = object$model, model.colour = object$model.colour, model.lwd = 
        object$model.lwd, model.lty = object$model.lty, model.name = 
        object$model.name, yhat = object$yhat, mar = object$mar, data.colour = 
        data.colour, data.order = data.order, view3d = view3d, theta3d = 
        theta3d, phi3d = phi3d, xs.grid = object$xs.grid, prednew = prednew))
    dev.flush()
    return(obj)    
    }
    screen(n = object$screen, new = TRUE)
    obj <- plotxs1(xs = object$xs, y = object$y, xc.cond = xc.cond, 
        model = object$model, model.colour = object$model.colour, model.lwd = 
        object$model.lwd, model.lty = object$model.lty, model.name = 
        object$model.name, yhat = object$yhat, mar = object$mar, data.colour = 
        data.colour, data.order = data.order, view3d = view3d, theta3d = 
        theta3d, phi3d = phi3d, conf = object$conf)
    dev.flush()
    obj    
}