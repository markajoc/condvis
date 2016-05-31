## These are some helper functions to be used when plotxs is representing the
## predicted class probabilities using little barcharts (i.e., probs = TRUE)

myglyph <-
function (x, y, xw, yw, heights, col)
{
  left <- x - 0.5 * xw
  right <- x + 0.5 * xw
  top <- y + 0.5 * yw
  bottom <- y - 0.5 * yw
  barwidth <- xw / length(heights)
  barleft <- seq(left, right - barwidth, barwidth)
  if (any(heights < 0))
    stop("cannot handle negative 'heights'")
  if (any(heights > 1))
    heights <- heights / max(heights)
  rect(xleft = barleft, xright = barleft + barwidth,
    ybottom = bottom, ytop = bottom + heights * yw,
    col = col
  )
}

myglyph2 <-
function (x, y, xw, yw, heights, col)
{
  left <- x - 0.5 * xw
  right <- x + 0.5 * xw
  top <- y + 0.5 * yw
  bottom <- y - 0.5 * yw
  barwidth <- xw / length(heights)
  barleft <- seq(left, right - barwidth, barwidth)
  if (any(heights < 0))
    stop("cannot handle negative 'heights'")
  if (any(heights > 1))
    heights <- heights / max(heights)
  cbind(barleft, barleft + barwidth, bottom, bottom + heights*yw, 1:length(heights))
}

extractprobs <-
function (model, pred)
{
  if (any(c("svm", "gbm") %in% class(model))){
    if ("probabilities" %in% names(attributes(pred))){
      p <- attr(pred, "probabilities")
    } else stop("predictions do not have 'probabilities' attribute,\n ",
      "maybe svm was fitted without 'probability == TRUE'")
  } else stop("cannot display class probabilities for this model class")
  p
}
