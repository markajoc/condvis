## This is the default interactive plot for visualising sections which are
## chosen interactively. NOT EXPORTED.

ceplot.interactive <-
function (data, model, response = NULL, S = NULL, C = NULL, sigma = NULL,
  distance = "euclidean", cex.axis = NULL, cex.lab = NULL, tck = NULL, view3d =
  FALSE, Corder = "default", conf = FALSE, separate = TRUE, select.colour =
  "blue", select.cex = 1, select.lwd = 2, select.type = "minimal", probs = FALSE
  , col = "black", pch = 1, residuals = FALSE, xc.cond = NULL)
{
  uniqC <- unique(unlist(C))
  xc.cond <- if (is.null(xc.cond))
    data[1, !colnames(data) %in% c(S, response)]
  else xc.cond
  #data.frame(lapply(data[, !colnames(data) %in% c(S, response)], mode1))
  xcplots <- list()
  coords <- matrix(ncol = 4L, nrow = length(C))
  plotlegend <- length(S) == 2
  n.selector.cols <- ceiling(length(C) / 4L)
  selector.colwidth <- 2
  height <- 8
  col <- rep(col, length.out = nrow(data))
  vwfun <- .visualweight(xc = data[, uniqC, drop = FALSE])
  if (separate){

## Plot condition selectors on a separate device

## Set up section visualisation first

    width <- height + 0.5 * plotlegend
    opendev(width = width, height = height)
    devexp <- dev.cur()
    close.screen(all.screens = TRUE)
    legendwidth <- 1.15 / height
    xsscreens <- if (plotlegend){
      split.screen(figs = matrix(c(0, 1 - legendwidth, 1 - legendwidth, 1, 0, 0,
        1, 1), ncol = 4))
    } else split.screen()
    if (plotlegend){
      screen(xsscreens[2L])
      xslegend(data[, response], response)
    }
    screen(xsscreens[1L])
    vw <- vwfun(xc.cond = xc.cond, sigma = sigma, distance = distance)
    par(mar = c(3, 3, 3, 3))

## Check whether response should be raw or residual

    if (residuals){
      xsplot <- plotxsres(xs = data[, S, drop = FALSE], data[, response, drop =
        FALSE], xc.cond = xc.cond, model = model, col = col, weights = vw$k,
        view3d = view3d, conf = conf, probs = probs, pch = pch)
    } else {
      xsplot <- plotxs1(xs = data[, S, drop = FALSE], data[, response, drop =
        FALSE], xc.cond = xc.cond, model = model, col = col, weights = vw$k,
        view3d = view3d, conf = conf, probs = probs, pch = pch)
    }
    xscoords <- par("fig")

## Produce the condition selector plots. Can be either "minimal", meaning
## bivariate and univariate plots, "pcp" for parallel coordinates or "full" for
## a full scatterplot matrix.

    if (identical(select.type, "minimal")){
      xcwidth <- selector.colwidth * n.selector.cols
      n.selector.rows <- ceiling(length(C) / n.selector.cols)
      xcheight <- selector.colwidth * n.selector.rows
      opendev(height = xcheight, width = xcwidth)
      close.screen(all.screens = TRUE)
      xcscreens <- split.screen(c(n.selector.rows, n.selector.cols))
      for (i in seq_along(C)){
        screen(xcscreens[i])
        xcplots[[i]] <- plotxc(xc = data[, C[[i]]], xc.cond = xc.cond[1L, C[[i]]
          ], name = colnames(data[, C[[i]], drop = FALSE]), select.colour =
          select.colour, select.cex = select.cex)
        coords[i, ] <- par("fig")
      }
    } else if (identical(select.type, "pcp")){
      xcwidth <- 7
      xcheight <- 3
      opendev(height = xcheight, width = xcwidth)
      xcplots <- plotxc.pcp(Xc = data[, uniqC, drop = FALSE], Xc.cond = xc.cond[
        1, uniqC, drop = FALSE], select.colour = select.colour, select.lwd =
        select.lwd, cex.axis = cex.axis, cex.lab = cex.lab, tck = tck,
        select.cex = select.cex)
    } else if (identical(select.type, "full")){
      xcwidth <- 7
      opendev(height = xcwidth, width = xcwidth)
      xcplots <- plotxc.full(Xc = data[, uniqC, drop = FALSE], Xc.cond =
        xc.cond[1, uniqC, drop = FALSE], select.colour = select.colour,
        select.lwd = select.lwd, cex.axis = cex.axis, cex.lab = cex.lab, tck =
        tck, select.cex = select.cex)
    } else stop("'select.type' must be one of 'minimal', 'pcp' or 'full'")
    devcond <- dev.cur()
  } else {

## Otherwise, put everything on one device.

## Do condition selectors first

    width <- height + 0.5 * plotlegend + selector.colwidth * n.selector.cols
    opendev(width = width, height = height)
    close.screen(all.screens = TRUE)
    xcwidth <- selector.colwidth * n.selector.cols / width
    mainscreens <- split.screen(figs = matrix(c(0, 1 - xcwidth, 1 - xcwidth, 1,
      0, 0, 1, 1), ncol = 4L))
    xcscreens <- split.screen(c(4L, n.selector.cols), screen = mainscreens[2L])
    for (i in seq_along(C)){
      screen(xcscreens[i])
      xcplots[[i]] <- plotxc(xc = data[, C[[i]]], xc.cond = xc.cond[1L, C[[i]]],
        name = colnames(data[, C[[i]], drop = FALSE]), select.colour =
        select.colour, select.cex = select.cex)
      coords[i, ] <- par("fig")
    }

## Do section visualisation

    legendwidth <- 1.15 / height
    xsscreens <- if (plotlegend){
    split.screen(figs = matrix(c(0, 1 - legendwidth, 1 - legendwidth, 1, 0, 0, 1
      , 1), ncol = 4), screen = mainscreens[1L])
    } else mainscreens[1L]
    if (plotlegend){
      screen(xsscreens[2L])
      xslegend(data[, response], response)
    }
    screen(xsscreens[1L])
    vw <- vwfun(xc.cond = xc.cond, sigma = sigma, distance = distance)
    par(mar = c(3, 3, 3, 3))
    if (residuals){
      xsplot <- plotxsres(xs = data[, S, drop = FALSE], data[, response, drop =
        FALSE], xc.cond = xc.cond, model = model, col = col, weights = vw$k,
        view3d = view3d, conf = conf, probs = probs, pch = pch)
    } else {
      xsplot <- plotxs1(xs = data[, S, drop = FALSE], data[, response, drop =
        FALSE], xc.cond = xc.cond, model = model, col = col, weights = vw$k,
        view3d = view3d, conf = conf, probs = probs, pch = pch)
    }
    xscoords <- par("fig")
    xold <- NULL
    yold <- NULL
  }

## Define event handling functions; mouseclick and keystroke

  mouseclick <- function (separate = FALSE)
  {
    function (buttons, x, y)
    {
      if (0 %in% buttons){
      needupdate <- FALSE
      if (identical(select.type, "minimal")){
        plotindex <- which(apply(coords, 1, `%inrectangle%`, point = c(x, y)))
        if ((length(plotindex) > 0) && (0 %in% buttons)){
          xcplots[[plotindex]] <<- update(xcplots[[plotindex]], x, y)
          if (any(xc.cond[, xcplots[[plotindex]]$name] != xcplots[[plotindex]
            ]$xc.cond.old)){
            needupdate <- TRUE
            xc.cond[, xcplots[[plotindex]]$name] <<- xcplots[[plotindex]
              ]$xc.cond.old
          }
        }
      } else if (select.type %in% c("pcp", "full")){
        xcplots <<- update(xcplots, x, y)
        if (any(xc.cond[, uniqC] != xcplots$Xc.cond[, uniqC])){
          needupdate <- TRUE
          xc.cond[, uniqC] <<- xcplots$Xc.cond
        }
      }
      if (needupdate){
        vw <<- vwfun(xc.cond = xc.cond, sigma = vw$sigma, distance =
          vw$distance)
        xsplot <<- update(xsplot, xc.cond = xc.cond, weights = vw$k)
      }
      if (all(!separate, findInterval(x, xscoords[1:2]) == 1, identical(
        xsplot$plot.type, "ccc"), xsplot$view3d, 0 %in% buttons)){
        if (!is.null(xold))
          xsplot <<- update(xsplot, theta3d = xsplot$theta3d + 1 * (xold > x) -
            1 * (xold < x), phi3d = xsplot$phi3d + 1 * (yold > y) - 1 * (yold <
            y), xs.grid = xsplot$xs.grid, prednew = xsplot$prednew)
        xold <<- x
        yold <<- y
      }
      points(NULL)
    }
  }
  }
  keystroke <- function ()
    {
    function (key)
      {
      if (identical(key, "q")){
        cat("\nInteractive session ended.\n")
        return(invisible(1))
      }
      if (identical(xsplot$plot.type, "ccc") & xsplot$view3d & key %in% c("Up",
        "Down", "Left", "Right")){
        xsplot <<- update(xsplot, theta3d = xsplot$theta3d - 2 * (key == "Right"
          ) + 2 * (key == "Left"), phi3d = xsplot$phi3d - 2 * (key == "Up") + 2
          * (key == "Down"), xs.grid = xsplot$xs.grid, prednew = xsplot$prednew)
      }
      if (key %in% c(",", ".")){
        sigma <- vw$sigma + 0.01 * vw$sigma * (key == ".") - 0.01 * vw$sigma *
          (key == ",")
        vw <<- vwfun(xc.cond = xc.cond, sigma = sigma, distance = vw$distance)
        xsplot <<- update(xsplot, weights = vw$k, xs.grid = xsplot$xs.grid,
          newdata = xsplot$newdata, prednew = xsplot$prednew)
      }
      if (identical(key, "s")){
        if (separate){
          filename <- paste("snapshot_", gsub(":", ".", gsub(" ", "_",
            Sys.time())), c("-expectation.pdf", "-condition.pdf"), sep = "")
          dev.set(devexp)
          devexpsize <- dev.size()
          pdf(file = filename[1L], width = devexpsize[1L], height =
            devexpsize[2L])
          close.screen(all.screens = TRUE)
          xsscreens <- if (plotlegend){
            split.screen(figs = matrix(c(0, 1 - legendwidth, 1 - legendwidth, 1,
              0, 0, 1, 1), ncol = 4L))
          } else split.screen()
          if (plotlegend){
            screen(xsscreens[2L])
            xslegend(data[, response], response)
          }
          screen(xsscreens[1L])
          if (residuals){
            plotxsres(xs = data[, S, drop = FALSE], data[, response, drop = FALSE],
              xc.cond = xc.cond, model = model, col = col, weights = vw$k,
              view3d = xsplot$view3d, theta3d = xsplot$theta3d, phi3d =
              xsplot$phi3d, conf = conf, probs = probs, pch = pch)
          } else {
            plotxs1(xs = data[, S, drop = FALSE], data[, response, drop = FALSE],
              xc.cond = xc.cond, model = model, col = col, weights = vw$k,
              view3d = xsplot$view3d, theta3d = xsplot$theta3d, phi3d =
              xsplot$phi3d, conf = conf, probs = probs, pch = pch)
          }
          dev.off()
          cat(paste("\nSnapshot saved: '", filename[1L],"'", sep = ""))
          dev.set(devcond)
          devcondsize <- dev.size()
          pdf(file = filename[2L], width = devcondsize[1L], height =
            devcondsize[2L])
          close.screen(all.screens = TRUE)
          xcscreens <- split.screen(c(n.selector.rows, n.selector.cols))
          for (i in seq_along(C)){
            screen(xcscreens[i])
            plotxc(xc = xcplots[[i]]$xc, xc.cond = xcplots[[i]]$xc.cond.old,
              name = xcplots[[i]]$name, select.colour = xcplots[[i]
              ]$select.colour, select.cex = xcplots[[i]]$select.cex)
          }
          dev.off()
          cat(paste("\nSnapshot saved: '", filename[2L],"'", sep = ""))
          cat("\n")
        } else {
          filename <- paste("snapshot_", gsub(":", ".", gsub(" ", "_",
            Sys.time())), ".pdf", sep = "")
          pdf(file = filename, width = width, height = height)
          close.screen(all.screens = TRUE)
          xcwidth <- selector.colwidth * n.selector.cols / width
          mainscreens <- split.screen(figs = matrix(c(0, 1 - xcwidth, 1 -
            xcwidth, 1, 0, 0, 1, 1), ncol = 4))
          xcscreens <- split.screen(c(4, n.selector.cols), screen =
            mainscreens[2L])
          for (i in seq_along(C)){
            screen(xcscreens[i])
            plotxc(xc = xcplots[[i]]$xc, xc.cond = xcplots[[i]]$xc.cond.old,
              name = xcplots[[i]]$name, select.colour = xcplots[[i]
              ]$select.colour, select.cex = xcplots[[i]]$select.cex)
          }
          xsscreens <- if (plotlegend){
            split.screen(figs = matrix(c(0, 1 - legendwidth, 1 - legendwidth, 1,
              0, 0, 1, 1), ncol = 4L), screen = mainscreens[1L])
          } else mainscreens[1L]
          if (plotlegend){
            screen(xsscreens[2L])
            xslegend(data[, response], response)
          }
          screen(xsscreens[1L])
          if (residuals){
            plotxsres(xs = data[, S, drop = FALSE], data[, response, drop =
              FALSE], xc.cond = xc.cond, model = model, col = col, weights =
              vw$k, view3d = xsplot$view3d, theta3d = xsplot$theta3d, phi3d =
              xsplot$phi3d, conf = conf, probs = probs, pch = 1)
          } else {
            plotxs1(xs = data[, S, drop = FALSE], data[, response, drop = FALSE],
              xc.cond = xc.cond, model = model, col = col, weights = vw$k,
              view3d = xsplot$view3d, theta3d = xsplot$theta3d, phi3d =
              xsplot$phi3d, conf = conf, probs = probs, pch = 1)
          }
          dev.off()
          cat(paste("\nSnapshot saved: '", filename,"'\n", sep = ""))
        }
      }
      points(NULL)
    }
  }
  setGraphicsEventHandlers(
    onMouseDown = mouseclick(separate),
    onMouseMove = mouseclick(separate),
    onKeybd = keystroke())
  #getGraphicsEventEnv()
  getGraphicsEvent()
}
