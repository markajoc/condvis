update.xcplot <- 
function (object, xclick, yclick)
{
    screen(n = object$screen, new = FALSE)
    screen(n = object$screen, new = FALSE) 
    par(usr = object$usr)
    par(mar = object$mar)    
    xclickconv <- grconvertX(xclick, "ndc", "user")
    yclickconv <- grconvertY(yclick, "ndc", "user")
    if (identical(object$plot.type, "histogram")){
        xc.cond.new <- max(min(xclickconv, max(object$xc, na.rm = TRUE)), min(object$xc, na.rm = TRUE), na.rm = TRUE)
        if (xc.cond.new != object$xc.cond.old){
            abline(v = object$xc.cond.old, lwd = 2 * object$select.lwd, col = "white")
            break4redraw <- which.min(abs(object$histmp$breaks - object$xc.cond.old))
            rect(xleft = object$histmp$breaks[break4redraw + c(-1, 0)], xright = 
                object$histmp$breaks[break4redraw + c(0, 1)], ybottom = c(0, 0), ytop = 
                object$histmp$counts[break4redraw + c(-1, 0)])
            abline(v = xc.cond.new, lwd = object$select.lwd, col = object$select.col)
            object$xc.cond.old <- xc.cond.new
        } 
    } else if (identical(object$plot.type, "barplot")){
        xc.cond.new <- as.factor(object$factorcoords$level)[which.min(abs(
            xclickconv - object$factorcoords$x))]
        if (xc.cond.new != object$xc.cond.old){
            barindex.old <- levels(object$xc) == object$xc.cond.old
            rect(xleft = object$bartmp$w.l[barindex.old], xright = object$bartmp$w.r[barindex.old], 
                ybottom = 0, ytop = object$bartmp$height[barindex.old], col = "gray")
            barindex.new <- levels(object$xc) == xc.cond.new
            rect(xleft = object$bartmp$w.l[barindex.new], xright = object$bartmp$w.r[barindex.new], 
                ybottom = 0, ytop = object$bartmp$height[barindex.new], col = object$select.colour, density = 25)
            object$xc.cond.old <- xc.cond.new
        }
    } else if (identical(object$plot.type, "scatterplot")){
        xc.cond.new.x <- max(min(xclickconv, max(object$xc[, 1], na.rm = TRUE)), min(object$xc[, 1], na.rm = TRUE), na.rm = TRUE)
        xc.cond.new.y <- max(min(yclickconv, max(object$xc[, 2], na.rm = TRUE)), min(object$xc[, 2], na.rm = TRUE), na.rm = TRUE)
        xc.cond.new <- c(xc.cond.new.x, xc.cond.new.y)
        if (any(xc.cond.new != object$xc.cond.old)){
            abline(v = object$xc.cond.old[1], h = object$xc.cond.old[2], lwd = 2 * object$select.lwd, col = "white")
            xrange <- abs(diff(range(object$xc[, 1])))
            yrange <- abs(diff(range(object$xc[, 2])))
            redrawindex.x <- findInterval(object$xc[, 1], object$xc.cond.old[1] + xrange * c(-0.125, 0.125) ) == 1
            redrawindex.y <- findInterval(object$xc[, 2], object$xc.cond.old[2] + yrange * c(-0.125, 0.125) ) == 1
            points(object$xc[redrawindex.x | redrawindex.y, ])
            box()
            abline(v = xc.cond.new.x, h = xc.cond.new.y, lwd = object$select.lwd, col = object$select.colour)
            object$xc.cond.old <- xc.cond.new
        }
    } else if (identical(object$plot.type, "boxplot")){
        xc.cond.new.x <- as.factor(object$factorcoords$level)[which.min(abs(xclickconv - object$factorcoords$x))]
        xc.cond.new.y <- if (abs(yclickconv - object$xc.cond.old[, 2]) > 0.025 * abs(diff(range(object$xc[, 2]))))
            max(min(yclickconv, max(object$xc[, 2], na.rm = TRUE)), min(object$xc[, 2], na.rm = TRUE), na.rm = TRUE)
        else object$xc.cond.old[, 2]    
        xc.cond.new <- c(xc.cond.new.x, xc.cond.new.y)
        if (any(xc.cond.new != object$xc.cond.old)){
            if (xc.cond.new.x != object$xc.cond.old[, 1]){
                abline(v = as.integer(object$xc.cond.old[, 1]), lwd = 2 * object$select.lwd, col = "white")
                print(par("usr"))
                cat(paste("Drew vertical white line at ", as.integer(object$xc.cond.old[, 1]), "\n"))
            }
                
            if (xc.cond.new.y != object$xc.cond.old[, 2]) {
                abline(h = object$xc.cond.old[, 2], lwd = 2 * object$select.lwd, col = "white")  
                cat(paste("Drew horizontal white line at ", object$xc.cond.old[, 2], "\n"))
            }   
                   
            par(new = TRUE)
            bxp(object$boxtmp, xaxt = "n", yaxt = "n")
            abline(v = as.integer(xc.cond.new.x), h = xc.cond.new.y, lwd = object$select.lwd, col = object$select.colour)
            xc.cond.new <- data.frame(xc.cond.new.x, xc.cond.new.y)
            names(xc.cond.new) <- names(object$xc.cond.old)
            object$xc.cond.old <- xc.cond.new
        }
    } else if (identical(object$plot.type, "spineplot")){
        sptmp <- object$sptmp
        rectcoords <- data.frame(sptmp$xleft, sptmp$xright, sptmp$ybottom, sptmp$ytop)
        if (c(xclickconv, yclickconv) %inrectangle% c(min(sptmp$xleft), max(sptmp$xright) , min(sptmp$ybottom), max(sptmp$ytop)) ){
            comb.index <- apply(rectcoords, 1L, `%inrectangle%`, point = c(xclickconv, yclickconv))
            if (any(comb.index)){
                xc.cond.new <- data.frame(as.factor(sptmp$xnames)[comb.index], as.factor(sptmp$ynames)[comb.index])
                names(xc.cond.new) <- names(object$xc.cond.old)
                if (any(xc.cond.new != object$xc.cond.old)){
                    object$xc.cond.old <- xc.cond.new
                    par(bg = "white")
                    screen(new = TRUE)
                    object <- plotxc(xc = object$xc, xc.cond = xc.cond.new, name = object$name, select.colour = object$select.colour, select.lwd = object$select.lwd, 
                        cex.axis = object$cex.axis, cex.lab = object$cex.lab, tck = object$tck)
                }
            }   
        }        
    }
    object
}