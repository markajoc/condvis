## Function to make a static snapshot of ceplot.interactive

ceplot.static <-
function (data, model, response = NULL, S = NULL, C = NULL, weights = NULL, col
  = "black", cex.axis = NULL, cex.lab = NULL, tck = NULL, view3d = FALSE,
  theta3d = 45, phi3d = 20, Corder = "default", xc.cond = NULL, select.colour =
  "blue", select.cex = 1, conf = FALSE, probs = FALSE)
{
  uniqC <- unique(unlist(C))
  xc.cond <- if (is.null(xc.cond))
    data[1, !colnames(data) %in% c(S, response)]
  else xc.cond
  #data.frame(lapply(data[, !colnames(data) %in% c(S, response)], mode1))
  xcplots <- list()
  close.screen(all.screens = T)
  n.selector.cols <- ceiling(length(C) / 4L)
  selector.colwidth <- 2
  height <- 8
  width <- height + 0.5 + selector.colwidth * n.selector.cols
  xcwidth <- selector.colwidth * n.selector.cols / width
  main <- split.screen(figs = matrix(c(0, 1 - xcwidth, 1 - xcwidth, 1,
    0, 0, 1, 1), ncol = 4))
  selectors <- split.screen(figs = c(4, n.selector.cols), screen = main[2])
  dev.hold()
  if (length(uniqC) > 0){
    for(i in seq_along(C)){
      screen(selectors[i])
      xcplots[[i]] <- plotxc(xc = data[, C[[i]]], xc.cond = xc.cond[1L, C[[i]]],
        name = colnames(data[, C[[i]], drop = FALSE]), select.colour =
        select.colour, select.cex = select.cex)
    }
  }
  screen(main[1])
  Xc <- data[, uniqC, drop = FALSE]
  xsplot <- plotxs(xs = data[, S, drop = FALSE], y = data[, response, drop =
    FALSE], xc.cond = xc.cond, model = model, model.colour = NULL, model.lwd =
    NULL, model.lty = NULL, yhat = NULL, mar = NULL, weights = weights, col =
    col, view3d = view3d, theta3d = theta3d, phi3d = phi3d, conf = conf, probs =
    probs)
  dev.flush()
}
