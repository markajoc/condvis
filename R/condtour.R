#' @title Conditional tour; a tour through sections in data space
#'
#' @description Whereas \code{\link{ceplot}} allows the user to interactively
#'   choose sections to visualise, \code{condtour} allows the user to pre-select
#'   all sections to visualise, order them, and cycle through them one by one.
#'   ']' advances the tour, and '[' goes back.
#'
#' @param data A dataframe.
#' @param model A fitted model object, or a list of such objects.
#' @param path A dataframe, describing the sections to take. Basically a
#'   dataframe with its \code{colnames} being a subset of the
#'   \code{colnames(data)}.
#' @param response Character name of response variable in \code{data}.
#' @param sectionvars Character name(s) of variables in \code{data} on which to
#'   take sections.
#' @param conditionvars Character name(s) of variables in \code{data} on which
#'   to condition.
#' @param threshold Threshold distance. Observed data which are a distance
#'   greater than \code{threshold} from the current section are not visible.
#' @param distance The type of distance measure to use, either
#'   \code{"euclidean"} (default) or \code{"maxnorm"}.
#' @param view3d Logical; if \code{TRUE}, plots a three-dimensional regression
#'   surface when possible.
#' @param conf Logical; if \code{TRUE}, plots confidence bounds or equivalent
#'   when possible.
#' @param col Colour for observed data points.
#' @param pch Plot symbols for observed data points.
#' @param xcplotpar Plotting parameters for condition selector plots as a list,
#'   passed to \code{\link{plotxc}}. Can specify \code{cex.axis}, \code{cex.lab}
#'   , \code{tck}, \code{col} for highlighting current section, \code{cex}.
#'
#' @return Produces a set of interactive plots. One device displays the current
#'   section. A second device shows the the current section in the space of the
#'   conditioning predictors given by \code{conditionvars}. A third device shows
#'   some simple diagnostic plots; one to show approximately how much data are
#'   visible on each section, and another to show what proportion of data are
#'   \emph{visited} by the tour.
#'
#' @seealso \code{\link{ceplot}}, \code{\link{similarityweight}}
#'
#' @examples
#' \dontrun{
#'
#' data(powerplant)
#' library(e1071)
#' model <- svm(PE ~ ., data = powerplant)
#' path <- makepath(powerplant[-5], 25)
#' condtour(data = powerplant, model = model, path = path$path,
#'   sectionvars = "AT")
#'
#' data(wine)
#' wine$Class <- as.factor(wine$Class)
#' library(e1071)
#' model5 <- list(svm(Class ~ ., data = wine))
#' conditionvars1 <- setdiff(colnames(wine), c("Class", "Hue", "Flavanoids"))
#' path <- makepath(wine[, conditionvars1], 50)
#' condtour(data = wine, model = model5, path = path$path, sectionvars = c("Hue"
#'   , "Flavanoids"), threshold = 3)
#'
#'}

condtour <-
function(data, model, path, response = NULL, sectionvars = NULL, conditionvars =
  NULL, threshold = NULL, distance = c("euclidean", "maxnorm"), view3d = FALSE,
  conf = FALSE, col = "black", pch = 1, xcplotpar = NULL)
{
  ## Rename for internal

  S <- sectionvars
  C <- conditionvars
  sigma <- threshold

  ## Check for optional inputs

  cex.axis <- xcplotpar$cex.axis
  cex.lab <- xcplotpar$cex.lab
  tck <- xcplotpar$tck
  select.colour <- if (is.null(xcplotpar$col))
    "blue"
  else xcplotpar$col
  select.cex <- if (is.null(xcplotpar$select.cex))
    1
  else xcplotpar$select.cex

  ## Set up interactive functions for mousemove, mouseclick and keystroke.

  xold <- NULL
  yold <- NULL
  mousemove <- function ()
  {
    function (buttons, x, y)
    {

      ## Rotate 3-D perspective plot from plotxs.

      if (all(findInterval(x, xscoords[1:2]) == 1, identical(
        xsplot$plot.type, "ccc"), xsplot$view3d, 0 %in% buttons)){
        if (!is.null(xold))
          xsplot <<- update(xsplot, theta3d = xsplot$theta3d + 1 * (xold > x)
            - 1 * (xold < x), phi3d = xsplot$phi3d + 1 * (yold > y) - 1 * (
            yold < y), xs.grid = xsplot$xs.grid, prednew = xsplot$prednew)
        xold <<- x
        yold <<- y
      }
      points(NULL)
    }
  }
  mouseclick <- function ()
  {
    function (buttons, x, y)
    {
      if (0 %in% buttons){

        ## Clicking the mouse advances the tour by one.

        pathindex <<- max(min(pathindex + 1, max(pathindexrange)), min(
          pathindexrange))
        applot <<- update(applot, pathindex = pathindex)
        xc.cond[, colnames(path)] <- path[pathindex, , drop = FALSE]
        xsplot <<- update(xsplot, xc.cond = xc.cond, weights = k[pathindex, ])
        for (i in seq_along(C)){
          xcplots[[i]] <<- update(xcplots[[i]], xc.cond = path[pathindex,
            colnames(data)[C[i]]])
        }
      }
      points(NULL)
    }
  }
  keystroke <- function ()
  {
    function (key)
    {

      ## 'q' key ends the interactive session.

      if (identical(key, "q")){
        cat("\nInteractive session ended.\n")
        return(invisible(1))
      }

      ## Arrow keys rotate a 3-D perspective plot from plotxs.

      if (identical(xsplot$plot.type, "ccc") & xsplot$view3d &
        key %in% c("Up", "Down", "Left", "Right")){
        xsplot <<- update(xsplot, theta3d = xsplot$theta3d - 2 * (key == "Right"
          ) + 2 * (key == "Left"), phi3d = xsplot$phi3d - 2 * (key == "Up") + 2
          * (key == "Down"), xs.grid = xsplot$xs.grid, prednew = xsplot$prednew)
      }

      ## '[' and ']' reverse and advance the tour respectively.

      if (key %in% c("[", "]")){
        pathindex <<- max(min(pathindex + 1 * (key == "]") - 1 * (key == "["),
          max(pathindexrange)), min(pathindexrange))
        applot <<- update(applot, pathindex = pathindex)
        xc.cond[, colnames(path)] <- path[pathindex, , drop = FALSE]
        xsplot <<- update(xsplot, xc.cond = xc.cond, weights = k[pathindex, ])
        for (i in seq_along(C)){
          xcplots[[i]] <<- update(xcplots[[i]], xc.cond = path[pathindex,
            colnames(data)[C[i]]])
        }
      }
      points(NULL)
    }
  }

  ## Set up variable default values etc.

  data <- na.omit(data)
  model <- if (!identical(class(model), "list"))
    list(model)
  else model
  model.name <- if (!is.null(names(model)))
    names(model)
  else NULL
  varnamestry <- try(getvarnames(model[[1]]), silent = TRUE)
  response <- if (is.null(response))
    if (class(varnamestry) != "try-error")
      which(colnames(data) == varnamestry$response[1])
    else stop("could not extract response from 'model'.")
  else if (is.character(response))
    which(colnames(data) == response)
    else response
  S <- if(is.null(S)){
    (1:ncol(data))[-response][1L]
  } else if (is.character(S))
    vapply(S, function(x) which(colnames(data) == x), numeric(1))
    else S
  C <- if (is.null(C))
    arrangeC(data[, -c(response, S)])
  else C
  try(
    if (class(varnamestry) != "try-error"){
      possibleC <- unique(unlist(lapply(lapply(model, getvarnames), `[[`, 2)))
      possibleC <- possibleC[possibleC %in% colnames(data)]
      C <- arrangeC(data[, possibleC[!(possibleC %in% colnames(data)[S])],
        drop = FALSE])
    }
    , silent = TRUE)
  C <- if (all(vapply(C, is.numeric, logical(1))))
    as.list(C)
  else if (all(vapply(C, is.character, logical(1))))
    lapply(C, match, table = colnames(data))
  else stop("'C' should be a vector or list (containing vectors of length",
    " 1 or 2) with integer column indices or character variable",
    " names from 'data'.")
  uniqC <- unique(unlist(C))
  C <- uniqC
  col <- rep(col, length.out = nrow(data))
  pch <- rep(pch, length.out = nrow(data))
  distance <- match.arg(distance)
  pathindex <- 1
  pathindexrange <- c(1, nrow(path))
  xc.cond <- data[, setdiff(colnames(data), c(S, response))]
  xc.cond[, colnames(path)] <- path[pathindex, , drop = FALSE]
  if (any(response %in% uniqC))
    stop("cannot have 'response' variable in 'C'")
  if (any(response %in% S))
    stop("cannot have 'response' variable in 'S'")
  if (!identical(length(intersect(S, uniqC)), 0L))
    stop("cannot have variables common to both 'S' and 'C'")
  xcplots <- list()
  coords <- matrix(ncol = 4L, nrow = length(C))
  plotlegend <- length(S) == 2
  n.selector.cols <- ceiling(length(C) / 4L)
  selector.colwidth <- 2
  height <- 8
  width <- height + 0.5 * plotlegend

  ## Calculate the similarity weights for the entire tour.

  k <- similarityweight(x = path, data = data[, colnames(path), drop = FALSE],
    threshold = sigma, distance = distance)

  ## Do section visualisation.

  opendev(width = width, height = height)
  devexp <- dev.cur()
  close.screen(all.screens = TRUE)
  legendwidth <- 1.15 / height
  xsscreens <- if (plotlegend){
    split.screen(figs = matrix(c(0, 1 - legendwidth, 1 - legendwidth, 1, 0, 0, 1
      , 1), ncol = 4))
  } else split.screen()
  if (plotlegend){
    screen(xsscreens[2L])
    xslegend(data[, response], colnames(data)[response])
  }
  screen(xsscreens[1L])
  par(mar = c(3, 3, 3, 3))
  xsplot <- plotxs(xs = data[, S, drop = FALSE], data[, response, drop = FALSE]
    , xc.cond = xc.cond, model = model, weights = k[pathindex, ], col = col,
    view3d = view3d, conf = conf, pch = pch)
  xscoords <- par("fig")
  setGraphicsEventHandlers(
    onMouseMove = mousemove(),
    onKeybd = keystroke())

  ## Do diagnostic plots.

  opendev(width = 4, height = 6)
  devdiag <- dev.cur()
  close.screen(all.screens = TRUE)
  diagscreens <- split.screen(c(2, 1))
  screen(diagscreens[1L])
  par(mar = c(4, 4, 2, 2))
  plotmaxk(apply(k, 2, max))
  screen(diagscreens[2L])
  par(mar = c(4, 4, 2, 2))
  applot <- plotap(k)
  setGraphicsEventHandlers(
    onMouseDown = mouseclick(),
    onKeybd = keystroke())

  ## Do condition plots, so we can see where we are in the data space.

  xcwidth <- selector.colwidth * n.selector.cols
  n.selector.rows <- ceiling(length(C) / n.selector.cols)
  xcheight <- selector.colwidth * n.selector.rows
  opendev(width = xcwidth, height = xcheight)
  devcond <- dev.cur()
  close.screen(all.screens = TRUE)
  xcscreens <- split.screen(c(n.selector.rows, n.selector.cols))
  for (i in seq_along(uniqC)){
    screen(xcscreens[i])
    xcplots[[i]] <- plotxc(xc = data[, C[[i]]], xc.cond = path[pathindex,
      colnames(data)[C[i]]], name = colnames(data[, C[[i]], drop = FALSE]),
      select.colour = select.colour)
    coords[i, ] <- par("fig")
  }
  setGraphicsEventHandlers(
    onMouseDown = mouseclick(),
    onKeybd = keystroke())
  getGraphicsEventEnv()
  getGraphicsEvent()
}
