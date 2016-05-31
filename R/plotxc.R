plotxc <-
function (xc, xc.cond, name = NULL, select.colour = NULL, select.lwd = NULL,
  cex.axis = NULL, cex.lab = NULL, tck = NULL, select.cex = 1, ...)
{
  select.colour <- if (is.null(select.colour))
    "black"
  else select.colour
  select.lwd <- if (is.null(select.lwd))
    2
  else select.lwd
  mar <- if (!exists("mar"))
    c(3, 3, 0.5, 0.5)
  else mar
  cex.axis <- if (identical(version$os, "linux-gnu"))
    1
  else if (is.null(cex.axis))
    0.7
  else cex.axis
  cex.lab <- if (identical(version$os, "linux-gnu"))
    1
  else if (is.null(cex.lab))
    0.8
  else cex.lab
  tck <- if (is.null(tck))
    - 0.2
  else tck
  par(mar = mar)
  par(mgp = c(1.5, 0.5, 0))
  if (is.vector(xc) | is.factor(xc)){
    if (!is.factor(xc)){
      if (diff(range(xc, na.rm = TRUE)) / diff(range(xcnew <- xc[findInterval(
        xc, quantile(xc, c(0.025, 0.975), na.rm = TRUE)) == 1], na.rm = TRUE)) >
        3){
        xc <- xcnew
      }
      histmp <- hist(xc, xlab = name, ylab = "", main = "", cex.axis = cex.axis,
        cex.lab = cex.lab, tcl = tck, mgp = c(1.5, 0.5, 0.1))
      lines(x = rep(xc.cond, 2L), y = c(0, max(histmp$counts)), col =
        select.colour, lwd = select.lwd)
      plot.type <- "histogram"
    } else {
      bartmp <- barplot2(table(xc), main = "", xlab = name, cex.axis = cex.axis,
        cex.lab = cex.lab, tcl = tck)
      factorcoords <- data.frame(level = levels(xc), x = - 0.5 + 1.2 * (1:
        length(levels(xc))))
      barindex <- factorcoords$level == as.character(xc.cond)
      rect(xleft = bartmp$w.l[barindex], xright = bartmp$w.r[barindex], ybottom
        = 0, ytop = bartmp$height[barindex], col = select.colour, density = -1)
      plot.type <- "barplot"
      xc.cond <- factor(xc.cond, levels(xc))
    }
  } else {
    if (is.data.frame(xc) & identical(ncol(xc), 2L)){
      are.factors <- vapply(xc,is.factor, logical(1))
      if (all(are.factors)){
        sptmp <- spineplot2(table(xc), ...)
        xmatch <- as.character(xc.cond[, 1]) == levels(xc[, 1])
        ymatch <- as.character(xc.cond[, 2]) == levels(xc[, 2])
        xlev <- levels(xc[, 1])[xmatch]
        ylev <- levels(xc[, 2])[ymatch]
        match.index <- (sptmp$xnames == xlev) & (sptmp$ynames == ylev)
        rect(xleft = sptmp$xleft[match.index], ybottom = sptmp$ybottom[
          match.index], xright = sptmp$xright[match.index], ytop = sptmp$ytop[
          match.index], col = select.colour, density = -1)
        axis(1, at = ((sptmp$xat[1L:sptmp$nx] + sptmp$xat[2L:(sptmp$nx + 1L)] -
          sptmp$off)/2)[xmatch], labels = unique(sptmp$xnames)[xmatch], tick =
          FALSE, col.axis = select.colour)
        axis(2, at = sptmp$yat[ymatch], labels = unique(sptmp$ynames)[ymatch],
          col.axis = select.colour, tick = FALSE)
        plot.type <- "spineplot"
      } else {
        if (any(are.factors)){
          boxx <- xc[, are.factors]
          boxy <- xc[, !are.factors]
          boxtmp <- boxplot(boxy ~ boxx, xlab = name[are.factors], ylab = name[
            !are.factors], cex.axis = cex.axis, cex.lab = cex.lab)
          factorcoords <- data.frame(level = levels(xc[, are.factors]), x = 1:
            length(levels(xc[, are.factors])))
          abline(v = factorcoords$x[as.character(factorcoords$level) ==
            as.character(xc.cond[,are.factors])], h = xc.cond[!are.factors], lwd
            = select.lwd, col = select.colour)
          plot.type <- "boxplot"
          xc <- xc[, order(!are.factors)]
          xc.cond <- data.frame(factor(xc.cond[, are.factors],
          levels(boxx)), xc.cond[, !are.factors])
          name <- name[order(!are.factors)]
          names(xc.cond) <- name
        } else {
          if (nrow(xc) > 2000 && requireNamespace("gplots", quietly = TRUE)){
            b <- seq(0.35, 1, length.out = 16)
            gplots::hist2d(xc[, 1], xc[, 2], nbins = 50, col = c("white", rgb(1
              - b, 1 - b, 1 - b)), xlab = colnames(xc)[1], ylab = colnames(xc)[
              2], cex.axis = cex.axis, cex.lab = cex.lab, tcl = tck)
            box()
          } else {
            plot.default(xc[, 1], xc[, 2], xlab = colnames(xc)[1], ylab =
              colnames(xc)[2], cex.axis = cex.axis, cex.lab = cex.lab, tcl = tck
              , cex = select.cex)
          }
          abline(v = xc.cond[1], h = xc.cond[2], lwd = select.lwd, col =
            select.colour)
          plot.type <- "scatterplot"
        }
      }
    } else stop("Unexpected value for 'xc'")
  }
  structure(list(xc = xc, xc.cond.old = xc.cond, name = name, select.colour =
    select.colour, mar = mar, select.lwd = select.lwd, select.cex = select.cex,
    cex.axis = cex.axis, cex.lab = cex.lab, tck = tck, device = dev.cur(), usr =
    par("usr"), screen = screen(), screen.coords = par("fig"), plot.type =
    plot.type, sptmp = if (exists("sptmp")) sptmp else NULL, factorcoords = if (
    exists("factorcoords")) factorcoords else NULL, histmp = if (exists("histmp"
    )) histmp else NULL, bartmp = if (exists("bartmp")) bartmp else NULL, boxtmp
    = if (exists("boxtmp")) boxtmp else NULL, ...), class = "xcplot")
}

plotxc.pcp <-
function (Xc, Xc.cond, select.colour = NULL, select.lwd = 3,
    cex.axis = NULL, cex.lab = NULL, tck = NULL, select.cex = 1, ...)
{
  select.colour <- if (is.null(select.colour))
    "blue"
  else select.colour
  cex.axis <- if (identical(version$os, "linux-gnu"))
    1
  else if (is.null(cex.axis))
      0.7
    else cex.axis
  cex.lab <- if (identical(version$os, "linux-gnu"))
    1
  else if (is.null(cex.lab))
      0.8
    else cex.lab
  tck <- if (is.null(tck))
    - 0.2
  else tck
  factorindex <- which(vapply(Xc, is.factor, logical(1)))
  Xc.num <- vapply(Xc, as.numeric, numeric(nrow(Xc)))
  Xc.num.scaled <- apply(Xc.num, 2, scale2unit)
  Xc.cond.num <- vapply(Xc.cond, as.numeric, numeric(1L))
  xcoord <- 1:ncol(Xc)
  ycoord <- (Xc.cond.num - apply(Xc.num, 2L, min))/(apply(Xc.num, 2L, max) -
    apply(Xc.num, 2L, min))
  parcoord(Xc.num, main = "Condition selector")
  points(xcoord, ycoord, col = select.colour, type = "l", lwd = select.lwd)
  points(xcoord, ycoord, col = select.colour, pch = 16)
  structure(list(Xc = Xc, Xc.cond = Xc.cond, Xc.num.scaled = Xc.num.scaled,
    xc.num.max = apply(Xc.num, 2, max), xc.num.min = apply(Xc.num, 2, min),
    xcoord = xcoord, ycoord = ycoord, plot.type = "pcp", select.colour =
    select.colour, select.cex = select.cex, select.lwd = select.lwd, mar =
    par("mar"), usr = par("usr"), factorindex = factorindex, device = dev.cur(),
    screen = screen()), class = "xcplot")
}

plotxc.full <-
function (Xc, Xc.cond, select.colour = NULL, select.lwd = 3,
    cex.axis = NULL, cex.lab = NULL, tck = NULL, select.cex = 0.6, ...)
{
  select.colour <- if (is.null(select.colour))
    "blue"
  else select.colour
  cex.axis <- if (identical(version$os, "linux-gnu"))
    1
  else if (is.null(cex.axis))
      0.7
    else cex.axis
  cex.lab <- if (identical(version$os, "linux-gnu"))
    1
  else if (is.null(cex.lab))
      0.8
    else cex.lab
  tck <- if (is.null(tck))
    - 0.2
  else tck
  factorindex <- vapply(Xc, is.factor, logical(1))
  Xc.num <- vapply(Xc, as.numeric, numeric(nrow(Xc)))
  Xc.cond.num <- vapply(Xc.cond, as.numeric, numeric(1L))
  close.screen(all.screens = TRUE)
  scr <- split.screen(c(ncol(Xc) + 2, ncol(Xc) + 2))
  scr2 <- as.vector(matrix(scr, ncol = ncol(Xc) + 2)[c(-1,
    -(ncol(Xc) + 2)), c(-1, -(ncol(Xc) + 2))])
  usr.matrix <- mar.matrix <- fig.matrix <- matrix(ncol = 4L, nrow = length(
    scr2))
  rows <- rep(1:ncol(Xc), each = ncol(Xc))
  cols <- rep(1:ncol(Xc), ncol(Xc))
  dev.hold()
  for (i in seq_along(scr2)){
    screen(scr2[i])
    par(mar = c(0.1, 0.1, 0.1, 0.1))
    par(mgp = c(3, 0.25, 0.15))
    plot(Xc.num[,cols[i]], Xc.num[,rows[i]], cex = select.cex, xlab = "",
      ylab = "", xaxt = "n", yaxt = "n", col = if (identical(
      rows[i], cols[i])) NULL else "black")
    if (!identical(rows[i], cols[i]))
      abline(v = Xc.cond.num[cols[i]], h = Xc.cond.num[rows[i]],
        col = select.colour, lwd = select.lwd)
    if (identical(rows[i], 1L) & (2 * (round(cols[i] / 2)) == cols[i]))
      axis(3, cex.axis = 0.7, tcl = -0.2)
    if (identical(rows[i], ncol(Xc)) & !(2 * (round(cols[i] / 2)) == cols[i]))
      axis(1, cex.axis = 0.7, tcl = -0.2)
    if (identical(cols[i], 1L) & (2 * (round(rows[i] / 2)) == rows[i]))
      axis(2, cex.axis = 0.7, tcl = -0.2)
    if (identical(cols[i], ncol(Xc)) & !(2 * (round(rows[i] / 2)) == rows[i]))
      axis(4, cex.axis = 0.7, tcl = -0.2)
    if (identical(rows[i], cols[i]))
      text(x = mean(range(Xc.num[,rows[i]])), y = mean(range(Xc.num[,cols[i]])),
        labels = colnames(Xc.num)[rows[i]])
    mar.matrix[i, ] <- par("mar")
    usr.matrix[i, ] <- par("usr")
    fig.matrix[i, ] <- par("fig")
  }
  #coords <- data.frame(t(vapply(scr2,
  #  function(i) {
  #    screen(i, new = F)
  #    par("fig")
  #  }, numeric(4))))
  coords <- data.frame(fig.matrix)
  names(coords) <- c("xleft", "xright", "ybottom", "ytop")
  coords$xcplots.index <- scr2
  dev.flush()
  structure(list(Xc = Xc, Xc.cond = Xc.cond, Xc.num = Xc.num, Xc.cond.num =
    Xc.cond.num, rows = rows, cols = cols, factorindex = factorindex, scr2 =
    scr2, coords = coords, plot.type = "full", device = dev.cur(), select.colour
    = select.colour, select.lwd = select.lwd, select.cex = select.cex,
    mar.matrix = mar.matrix, usr.matrix = usr.matrix), class = "xcplot")
}
