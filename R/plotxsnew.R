plotxs1 <- 
function (xs, y, xc.cond, model, model.colour = NULL, model.lwd = NULL, 
    model.lty = NULL, model.name = NULL, yhat = NULL, mar = NULL, data.colour = 
    NULL, data.order = NULL, view3d = FALSE, theta3d = 45, phi3d = 20)
{
    if (!(ncol(xs) %in% 1:2))
        stop("xs must be a dataframe with 1 or 2 columns")
    if (ncol(y) != 1)
        stop("y must be a dataframe with 1 column")
    model <- if (!is.list(model))
        list(model)
    else model    
    model.colour <- if (is.null(model.colour)){
        if (requireNamespace("RColorBrewer", quietly = TRUE))
		    RColorBrewer::brewer.pal(n = max(length(model), 3L), name = "Dark2")
		else rainbow(max(length(model), 4L))   
    } else rep(model.colour, length.out = length(model))
    model.lwd <- if (is.null(model.lwd)) 
        rep(2, length(model))
    else rep(model.lwd, length.out = length(model))
    model.lty <- if (is.null(model.lty)) 
        rep(1, length(model))
    else rep(model.lty, length.out = length(model))
    model.name <- if (is.null(model.name)) 
        vapply(model, function(x) tail(class(x), n = 1L), character(1))
    else model.name
    data.order <- if (is.null(data.order))
        1:nrow(xs)
    else data.order 
    data.colour <- if (is.null(data.colour))
        rep("gray", length(data.order))
    else data.colour 
    par(mar = c(5, 4, 3, 2))    
    if (identical(ncol(xs), 1L)){
        # xs has one column
        xs.grid <- if (!is.factor(xs[, 1L]))
            data.frame(seq(min(xs[, 1L], na.rm = TRUE), max(xs[, 1L], na.rm = 
                TRUE), length.out = if (view3d) {20L} else 50L))
        else data.frame(as.factor(levels(xs[, 1L])))
        colnames(xs.grid) <- colnames(xs)
        newdata <- makenewdata(xs = xs.grid, xc.cond = xc.cond)
	    prednew <- lapply(model, predict, newdata = newdata, type = "response")
        if (is.factor(xs[, 1L])){
            # xs is a factor
            if (is.factor(y[, 1L])){
                # y is factor
                plot.type <- "ff"
                if (identical(nlevels(y[, 1L]), 2L)){
                    plot(unique(xs[, 1L]), rep(0, length(levels(xs[, 1L]))), col = NULL, 
                        main = "Conditional expectation", 
                        ylab = paste("Probability ", colnames(y)[1L], "=", 
                        levels(y[, 1L])[2L]), ylim = c(0, 1))
				    points.default((as.numeric(xs[data.order, 1L])), 
                        (as.integer(y[data.order, 1L]) - 1), col = data.colour[data.order])
                    for (i in seq_along(model)){
                        if ("glm" %in% class(model[[i]])){
                            points.default(xs.grid[, 1L], prednew[[i]], 
                                type = 'l', col = model.colour[i], 
                                lwd = model.lwd[i], lty = model.lty[i])
                        } else {
                            points.default(xs.grid[, 1L], 
                                as.numeric(prednew[[i]]) - 1, type = 'l',
                                col = model.colour[i], lwd = model.lwd[i], 
                                lty = model.lty[i])                    
                        }
                    }
                    legend("topright", legend = model.name, col = model.colour, 
                        lwd = model.lwd, lty = model.lty)
                } else {
                    plot(range(as.numeric(xs[, 1L])) + c(0, 0.1 * abs(diff(
                        range(as.numeric(xs[, 1L])))) ), range(as.integer(
                        y[, 1L])), col = NULL, xlab = colnames(xs)[1L], ylab = 
                        colnames(y)[1L], yaxt = "n", main = "Conditional 
                        expectation", xaxt = if (is.factor(xs[, 1L])) "n" else NULL)
                    axis(2, at = 1:nlevels(y[, 1L]), labels = levels(y[, 1L]))
                    if (is.factor(xs[, 1L])) axis(1, at = 1:nlevels(xs[, 1L]), labels = levels(xs[, 1L]))              
                    if (length(data.order) > 0) 
                        points(as.numeric(xs[data.order, 1L]), as.integer(y[data.order, 1L]), 
                            col = data.colour[data.order]) 
                    for (i in seq_along(model)){
                        points.default(as.numeric(xs.grid[, 1L]), as.integer(prednew[[i]]),
                            type = 'l', col = model.colour[i], 
                            lwd = model.lwd[i], lty = model.lty[i])
                    }
                }
            } else {
                # y is continuous
                plot.type <- "cf"
                plot(unique(xs[, 1L]), rep(0, length(levels(xs[, 1L]))), col = NULL, 
                    main = "Conditional expectation", xlab = colnames(xs)[1L], 
                    ylab = colnames(y)[1L], ylim = range(y[, 1L]))
                if (length(data.order) > 0)
                    points(xs[data.order, 1L], y[data.order, 1L], col = data.colour[data.order])
                prednew2 <- lapply(model, confpred, newdata = newdata)     
                for (i in seq_along(model)){
                    points.default(xs.grid[, 1L], prednew[[i]], type = 'l',
                        col = model.colour[i], lwd = model.lwd[i], lty = model.lty[i])
                    if (all(c("lwr", "upr") %in% colnames(prednew2[[i]]))){
                        points.default(xs.grid[, 1L], prednew2[[i]][, "lwr"], 
                            type = 'l', lty = 3, col = model.colour[i], lwd = 
                            model.lwd[i])
                        points.default(xs.grid[, 1L], prednew2[[i]][, "upr"], 
                            type = 'l', lty = 3, col = model.colour[i], lwd = 
                            model.lwd[i])    
                    }
                }
                legend("topright", legend = model.name, col = model.colour, 
                    lwd = model.lwd, lty = model.lty)
            }
        } else {
            #xs is continuous
            if (is.factor(y[, 1L])){
                # y is factor
                plot.type <- "fc"
                if (identical(nlevels(y[, 1L]), 2L)){
                    plot(range(xs[, 1L]), c(0, 0), col = NULL, 
                        main = "Conditional expectation", 
                        ylab = paste("Probability ", colnames(y)[1L], "=", 
                        levels(y[, 1L])[2L]), ylim = c(0, 1))
                    if (length(data.order) > 0)    
				        points.default(xs[data.order, 1L], as.integer(y[
                            data.order, 1L]) - 1, col = data.colour[data.order])
                    for (i in seq_along(model)){
                        if ("glm" %in% class(model[[i]])){
                            points.default(xs.grid[, 1L], prednew[[i]], 
                                type = 'l', col = model.colour[i], 
                                lwd = model.lwd[i], lty = model.lty[i])
                        } else {
                            points.default(xs.grid[, 1L], 
                                as.numeric(prednew[[i]]) - 1, type = 'l',
                                col = model.colour[i], lwd = model.lwd[i], 
                                lty = model.lty[i])                    
                        }
                    }
                    legend("topright", legend = model.name, col = model.colour, 
                        lwd = model.lwd, lty = model.lty)
                } else {
                    plot(range(xs[, 1L]), range(as.integer(y[, 1L])), col = NULL, 
                        xlab = colnames(xs)[1L], ylab = colnames(y)[1L], 
                        yaxt = "n", main = "Conditional expectation", xaxt = 
                        if (is.factor(xs[, 1L])) "n" else NULL)
                    axis(2, at = 1:nlevels(y[, 1L]), labels = levels(y[, 1L]))
                    if (is.factor(xs[, 1L])) axis(1, at = 1:nlevels(xs[, 1L]), labels = levels(xs[, 1L]))              
                    if (length(data.order) > 0) 
                        points(xs[data.order, 1L], as.integer(y[data.order, 1L]), 
                            col = data.colour[data.order]) 
                    for (i in seq_along(model)){
                        points.default(as.numeric(xs.grid[, 1L]), as.integer(prednew[[i]]),
                            type = 'l', col = model.colour[i], 
                            lwd = model.lwd[i], lty = model.lty[i])
                    }
                }
            } else {
                # y is continuous
                plot.type <- "cc"







                plot(range(xs[, 1L]), range(y[, 1L]), col = NULL, 
                    main = "Conditional expectation", xlab = colnames(xs)[1L], 
                    ylab = colnames(y)[1L], ylim = range(y[, 1L]))
                if (length(data.order) > 0)
                    points(xs[data.order, 1L], y[data.order, 1L], col = data.colour[data.order])
                prednew2 <- lapply(model, confpred, newdata = newdata)     
                for (i in seq_along(model)){
                    points.default(xs.grid[, 1L], prednew[[i]], type = 'l',
                        col = model.colour[i], lwd = model.lwd[i], lty = model.lty[i])
                    if (all(c("lwr", "upr") %in% colnames(prednew2[[i]]))){
                        points.default(xs.grid[, 1L], prednew2[[i]][, "lwr"], 
                            type = 'l', lty = 3, col = model.colour[i], lwd = 
                            model.lwd[i])
                        points.default(xs.grid[, 1L], prednew2[[i]][, "upr"], 
                            type = 'l', lty = 3, col = model.colour[i], lwd = 
                            model.lwd[i])    
                    }
                }
                pos <- if (cor(xs, y) < 0)
                    "topright"
                else "bottomright"
                legend(pos, legend = model.name, col = model.colour, 
                    lwd = model.lwd, lty = model.lty)









                
            }
        }   
    } else {
        # xs has two columns
        arefactorsxs <- vapply(xs, is.factor, logical(1L))
        xs.grid1 <- if (!is.factor(xs[, 1L]))
            seq(min(xs[, 1L], na.rm = TRUE), max(xs[, 1L], na.rm = TRUE), 
                length.out = if (view3d) {20L} else 50L)
        else as.factor(levels(xs[, 1L]))
        xs.grid2 <- if (!is.factor(xs[, 2L]))
            seq(min(xs[, 2L], na.rm = TRUE), max(xs[, 2L], na.rm = TRUE), 
                length.out = if (view3d) {20L} else 50L)
        else as.factor(levels(xs[, 2L]))
        xs.grid <- data.frame(
            rep(xs.grid1, by = length(xs.grid2)), 
		    rep(xs.grid2, each = length(xs.grid1)))
        colnames(xs.grid) <- colnames(xs) 
        newdata <- makenewdata(xs = xs.grid, xc.cond = xc.cond)
	    prednew <- lapply(model, predict, newdata = newdata, type = "response")        
        if (all(arefactorsxs)){
            # xs are both factors
            if (is.factor(y[, 1L])){
                # y is factor
                plot.type <- "fff"
            } else {
                # y is continuous
                plot.type <- "cff"
            }
        } else {
            if (any(arefactorsxs)){
                # xs is one factor, one continuous
                if (is.factor(y[, 1L])){
                    # y is factor
                    plot.type <- "ffc"
                } else {
                    # y is continuous
                    plot.type <- "cfc"
                }
            } else {
                # xs are both continuous
                if (is.factor(y[, 1L])){
                    # y is factor
                    plot.type <- "fcc"
                } else {
                    # y is continuous
                    plot.type <- "ccc"
                    if (view3d){
                        yhat <- if (is.null(yhat))
                            lapply(model[1], predict, type = "response")
                        else yhat
                        
                    } else {
                    
                    }
                }
            }
        }
    }  
}