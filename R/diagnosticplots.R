## NOT EXPORTED.

## plotap a diagnostic plot to be used with condtour. The plot shows the sum of
## the similarity weights for each section along the entire path. The plot is
## given its own update method.

plotap <-
function (k, pathindex = 1, lcol = "blue")
{
  rsk <- rowSums(k) / ncol(k)
  plot(rsk, type = "l", xlab = "Path index", ylab = "sum of k/n")
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

## plotmaxk a diagnostic plot to be used with condtour. This is a static plot.
## Shows the deciles of the maximum similarity weight given to the data by the
## current conditional tour. If too many observations are reaching maximum
## similarity weight of 1, the 'threshold' used in similarityweight might be too
## big. If no observations are reaching maximum similarity weights of 0.3, we
## may not see any data on the sections.

plotmaxk <-
function (maxk)
{
  seq01 <- seq(0, 1, 0.1)
  q <- quantile(maxk, probs = seq01)
  plot(q, seq01, type = "l", ylab = "proportion of data", xlab =
    "max k attained", ylim = c(0, 1))
  points(q, seq01, pch = 16)
}

#update.maxk <-
#function (object, ...)
#{
#
#}
