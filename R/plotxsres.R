plotxsres <-
function (xs, y, xc.cond, model, model.colour = NULL, model.lwd = NULL,
    model.lty = NULL, model.name = NULL, yhat = NULL, mar = NULL, data.colour =
    NULL, data.order = NULL, view3d = FALSE, theta3d = 45, phi3d = 20, xs.grid =
    NULL, prednew = NULL, conf = FALSE, probs = FALSE, pch = 1)
{
    dev.hold()
    #if (!(ncol(xs) %in% 1:2))
    #  stop("xs must be a dataframe with 1 or 2 columns")
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
    model.name <- if(!is.null(names(model)))
      names(model)
    else seq_along(model)
    data.order <- if (is.null(data.order))
      1:nrow(y)
    else data.order
    data.colour <- if (is.null(data.colour))
      rep("gray", length(data.order))
    else data.colour
    yhat <- if (is.null(yhat))
      lapply(model, predict1, ylevels = NULL)
    else yhat
    par(mar = c(5, 4, 3, 2))



    if (is.null(xs)){
      if (is.null(prednew)){
        newdata <- xc.cond
        prednew <- lapply(model, predict1, newdata = newdata, ylevels = if (
          nlevels(y[, 1L]) > 2) levels(y[, 1L]) else NULL)
        }
      o <- hist(y[data.order, 1L], plot = FALSE)
      a1 <- hist(y[, 1L], plot = FALSE)
      abline(v = unlist(prednew), col = model.colour)



    } else {
    if (identical(ncol(xs), 1L)){
      # xs has one column
      if (is.factor(xs[, 1L])){
        # xs is a factor
        if (is.factor(y[, 1L])){
          # y is factor
          stop("residuals only defined for continuous response")
        } else {
          # y is continuous
          plot.type <- "cf"
          plot(unique(xs[, 1L]), rep(-888, length(levels(xs[, 1L]))), col = NULL
            , main = "Conditional expectation", xlab = colnames(xs)[1L], ylab =
            colnames(y)[1L], ylim = range(y[, 1L]))
          if (length(data.order) > 0)
            points(xs[data.order, 1L], y[data.order, 1L], col = data.colour[
              data.order], pch = pch[data.order])
          for (i in seq_along(model)){
            points.default(xs.grid[, 1L], prednew[[i]], type = 'l', col =
              model.colour[i], lwd = model.lwd[i], lty =  model.lty[i])
            }
          #legend("topright", legend = model.name, col = model.colour, lwd =
          #  model.lwd, lty = model.lty)
        }
      } else {
        #xs is continuous
        if (is.factor(y[, 1L])){
          # y is factor
          stop("residuals only defined for continuous response")
        } else {
          # y is continuous
          plot.type <- "cc"
          plot(range(xs[, 1L]), range(y[, 1L]), col = NULL, main =
            "Conditional expectation", xlab = colnames(xs)[1L], ylab = colnames(
            y)[1L], ylim = range(y[, 1L]))
          if (length(data.order) > 0)
            points(xs[data.order, 1L], y[data.order, 1L], col = data.colour[
              data.order], pch = pch[data.order])
            for (i in seq_along(model)){
              points.default(xs.grid[, 1L], prednew[[i]], type = 'l', col =
                model.colour[i], lwd = model.lwd[i], lty = model.lty[i])
            }
          pos <- if (cor(xs, y) < 0)
            "topright"
          else "bottomright"
          legend(pos, legend = model.name, col = model.colour, lwd = model.lwd,
            lty = model.lty)
        }
      }
    } else {
      # xs has two columns
      arefactorsxs <- vapply(xs, is.factor, logical(1L))
      if (all(arefactorsxs)){
        # xs are both factors
			  xrect <- as.integer(xs.grid[, 1L])
			  yrect <- as.integer(xs.grid[, 2L])
		  	xoffset <- abs(diff(unique(xrect)[1:2])) / 2.1
			  yoffset <- abs(diff(unique(yrect)[1:2])) / 2.1
			  plot(xrect, yrect, col = NULL, xlab = colnames(xs)[1L], ylab = colnames(
          xs)[2L], xlim = c(min(xrect) - xoffset, max(xrect) + xoffset), xaxt =
          "n", bty = "n", ylim = c(min(yrect) - yoffset, max(yrect) + yoffset),
          yaxt = "n", main = "Conditional expectation")
			  rect(xleft = xrect - xoffset, xright = xrect + xoffset, ybottom = yrect
          - yoffset, ytop = yrect + yoffset, col = color)
        if (length(data.order) > 0)
          points(jitter(as.integer(xs[data.order, 1L]), amount = 0.6 * xoffset),
            jitter(as.integer(xs[data.order, 2L]), amount = 0.6 * yoffset), bg =
            ybg, col = data.colour[data.order], pch = pch[data.order])
		    axis(1L, at = unique(xrect), labels = levels(xs[, 1L]), tick = FALSE)
			  axis(2L, at = unique(yrect), labels = levels(xs[, 2L]), tick = FALSE)
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
          plot.type <- if (is.factor(y[, 1L]))
            "ffc" # y is factor
          else "cfc" # y is continuous
    	    xrect <- xs.grid[, !arefactorsxs]
			    yrect <- as.integer(xs.grid[, arefactorsxs])
			    xoffset <- abs(diff(unique(xrect)[1:2])) / 2
			    yoffset <- abs(diff(unique(yrect)[1:2])) / 2.1
		      plot(0, 0, col = NULL, xlab = colnames(xs)[!arefactorsxs], ylab =
            colnames(xs)[arefactorsxs], xlim = c(min(xrect) - xoffset, max(xrect
            ) + xoffset), bty = "n", main = "Conditional expectation", ylim =
            c(min(yrect) - yoffset, max(yrect) + yoffset), yaxt = "n")
		      rect(xleft = xrect - xoffset, xright = xrect + xoffset, ybottom =
            yrect - yoffset, ytop = yrect + yoffset, col = color, border = NA)
          if (length(data.order) > 0)
            points(jitter(xs[data.order, !arefactorsxs]), jitter(as.integer(xs[
              data.order, arefactorsxs])), bg = ybg, col = data.colour[
              data.order], pch = pch[data.order])
			    axis(2L, at = unique(yrect), labels = levels(xs[, arefactorsxs]),
            tick = FALSE)
        } else {
          # xs are both continuous
          if (is.factor(y[, 1L])){
            # y is factor
            stop("residuals only defined for continuous response")
          } else {
            # y is continuous
            plot.type <- "ccc"
            if (view3d){
              x.persp <- unique(xs.grid[, 1L])
              y.persp <- unique(xs.grid[, 2L])
              z.persp <- matrix(0, ncol = length(x.persp), nrow = length(
                y.persp))
              par(mar = c(3, 3, 3, 3))
              persp.object <- suppressWarnings(persp(x = x.persp, y = y.persp,
                border = rgb(0.3, 0.3, 0.3), lwd = 0.1, z = z.persp, col = NULL,
                zlim = c(-4, 4), xlab = colnames(xs)[1L], ylab = colnames(xs)[2L
                ], zlab = colnames(y)[1L], d = 10, ticktype = "detailed", main =
                "Residuals", theta = theta3d, phi = phi3d))
              if (length(data.order) > 0){
                points(trans3d(xs[data.order, 1L], xs[data.order, 2L], y[
                  data.order, 1L], pmat = persp.object), col = data.colour[
                  data.order], pch = pch[data.order])
                linestarts <- trans3d(xs[data.order, 1L], xs[data.order, 2L],
                  y[data.order, 1L], pmat = persp.object)
                lineends <- trans3d(xs[data.order, 1L], xs[data.order, 2L],
                  yhat[[1]][data.order], pmat = persp.object)
                segments(x0 = linestarts$x, y0 = linestarts$y, x1 = lineends$x
                  , y1 = lineends$y, col = data.colour[data.order])
              }
            } else {
              xoffset <- abs(diff(unique(xs.grid[, 1L])[1:2])) / 2
              yoffset <- abs(diff(unique(xs.grid[, 2L])[1:2])) / 2
              plot(range(xs.grid[, 1L]), range(xs.grid[, 2L]), col = NULL,
                xlab = colnames(xs)[1L], ylab = colnames(xs)[2L], main =
                "Conditional expectation")
              rect(xleft = xs.grid[, 1L] - xoffset, xright = xs.grid[, 1L] +
                xoffset, ybottom = xs.grid[, 2L] - yoffset, ytop = xs.grid[,
                2L] + yoffset, col = color, border = NA)
              if (length(data.order) > 0)
                points(xs[data.order, , drop = FALSE], bg = ybg, col =
                  data.colour[data.order], pch = pch[data.order])
            }
          }
        }
      }
    }
    }
    dev.flush()
    structure(list(xs = xs, y = y, xc.cond = xc.cond, model = model,
      model.colour = model.colour, model.lwd = model.lwd, model.lty = model.lty,
      model.name = model.name, yhat = yhat, mar = par("mar"), data.colour =
      data.colour, data.order = data.order, view3d = view3d, theta3d = theta3d,
      usr = par("usr"), phi3d = phi3d, plot.type = if (exists("plot.type"))
      plot.type else NULL, screen = screen(), device = dev.cur(), xs.grid =
      xs.grid, newdata = newdata, prednew = prednew, xs.grid = xs.grid, conf =
      conf, probs = probs, pch = pch), class = "xsresplot")
}
