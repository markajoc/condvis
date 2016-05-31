## This is a diagnostic plot to be used with condtour. The plot shows the sum of
## the visual weights for each section along the entire path. The plot is given
## its own update method.

plotap <-
function (k, pathindex = 1, lcol = "blue")
{
  rsk <- rowSums(k)
  plot(rsk, type = "l", xlab = "Path index", ylab = "sum of k")
  abline(v = pathindex, col = lcol)
  structure(list(k = k, rsk = rsk, pathindex = pathindex, device = dev.cur(),
    screen = screen(), mar = par()$mar, usr = par()$usr, lcol = lcol),
    class = "ap")
}

update.ap <-
function (object, pathindex = NULL, ...)
{
  if (dev.cur() != object$device)
    dev.set(object$device)
  screen(n = object$screen, new = FALSE)
  par(mar = object$mar)
  par(usr = object$usr)
  if (!is.null(pathindex)){
    abline(v = object$pathindex, col = "white")
    refreshindex <- max((object$pathindex - 5), 1):min((object$pathindex + 5),
      nrow(object$k))
    points(refreshindex, object$rsk[refreshindex], type = "l")
    abline(v = pathindex, col = object$lcol)
    box()
    object$pathindex <- pathindex
  }
  object
}
