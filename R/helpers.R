## Helper function for extracting variable names from model objects.

cleanstring <-
function(string)
{
  if(!is.character(string))
    stop("'string' should be of type character")
  s <- unlist(strsplit(string, split = ""))
  if (any(c("(", ")", "^") %in% s)){
    beg <- if ("(" %in% s)
      which(s == "(")[1] + 1
    else 1
    end <- if (any(c(")", "^") %in% s))
      which(s == ")" | s == "^")[1] - 1
    else length(s)
    s <- s[beg:end]
  }
  paste(s, collapse = "")
}

## Helper function for extracting variable names from model objects

getvarnames <-
function (model)
{
  if (is.null(model$terms))
    stop("model has no terms slot")
  string1 <- deparse(model$terms[[3L]])
  string2 <- unlist(strsplit(string1, split = NULL))
  string3 <- paste(string2[string2 != " "], collapse = "")
  predictors1 <- unlist(strsplit(string3, split = "+", fixed = TRUE))
  predictors2 <- unique(vapply(predictors1, cleanstring, character(1L)))
  response <- unlist(deparse(model$terms[[2L]]))
  list(response = response, predictors = predictors2)
}

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

## Helper function to check if a point is in a rectangle

`%inrectangle%` <-
function (point, rectangle)
{
  ## Assuming (x, y) and (xleft, xright, ybottom, ytop)
  check1 <- point[1] >= rectangle[1]
  check2 <- point[1] < rectangle[2]
  check3 <- point[2] >= rectangle[3]
  check4 <- point[2] < rectangle[4]
  check1 && check2 && check3 && check4
}

## Helper function to make a dataframe representing a section defined by xc.cond

makenewdata <-
function (xs, xc.cond)
{
  if (is.null(xs) || identical(ncol(xs), 0L))
    return(xc.cond)
  newdata <- cbind(xs, xc.cond[rep(1L, nrow(xs)), ])
  colnames(newdata) <- c(colnames(xs), colnames(xc.cond))
  rownames(newdata) <- NULL
  newdata
}

## Helper function to calculate a reasonable mode for numerics and factors. Can
## be used to initialise 'xc.cond'. Best used as data.frame(lapply(data, mode1))

mode1 <-
function (x, breaks = 10)
{
  if (is.factor(x)){
    ux <- unique(x)
    out <- ux[which.max(tabulate(match(x, ux)))]
  } else if (is.numeric(x)){
    cx <- cut(x, breaks = breaks)
    ucx <- unique(cx)
    bin <- ucx[which.max(tabulate(match(cx, ucx)))]
    out <- mean(as.numeric((strsplit(gsub("]", "", gsub("\\(", "",
      as.character(bin))), ",")[[1]])))
  } else stop("mode function expects factors or numerics only")
  out
}

## Helper function to open a device suitable for interactivity with
## grDevices::getGraphicsEvent

opendev <-
function (width = 7, height = 7)
{
  orig <- options("device")
  if (identical(.Platform$OS.type, "windows")){
    options(device = "windows")
  } else {
    options(device = "X11")
    if (identical(version$os, "linux-gnu")){
      X11.options(type = "Xlib")
    }
  }
  dev.new(width = width, height = height)
  options(orig)
}

## This is a helper function for use in parallel coordinates plots in plotxc.pcp

scale2unit <-
function (x)
{
  x <- as.numeric(x)
  mn <- min(x)
  mx <- max(x)
  out <- (x - mn) / (mx - mn)
  out
}

## This is a helper function used only in savingby2d. Calculates the area of a
## polygon coming out of grDevices::chull

polygonarea <- function (x, y = NULL)
{
  if (is.null(y) && identical(ncol(x), 2L)){
    y <- x[, 2L]
    x <- x[, 1L]
  }
  area <- 0
  n <- length(x)
  j <- n
  for (i in 1:n){
    area <- area + (x[j] + x[i]) * (y[j] - y[i])
    j <- i
  }
  abs(area) / 2
}
