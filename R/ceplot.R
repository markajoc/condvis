#' @title Interactive conditional expectation plot
#'
#' @description
#'
#' @param data A dataframe containing the data to plot
#' @param model A model object, or list of model objects
#' @param response Character name of response in \code{data}
#' @param S Character name of variable(s) from \code{data} on which to take a
#'   section, can be of length 1 or 2.
#' @param C Character names of conditioning variables from \code{data}. Can be a
#'   list of vectors of length 1 or 2. Can be a character vector, which is then
#'   paired up using \link{\code{arrangeC}}. If \code{NULL}, an attempt will be
#'   made to extract all variable names which are not \code{response} or
#'   \code{S} from \code{model}, and these will be arranged using \link{\code{
#'   arrangeC}}.
#' @param sigma This is a threshold distance. Points further than \code{sigma}
#'   away from the current section will not be visible. Passed to \link\code{
#'   visualweight}}
#' @param distance A character vector describing the type of distance measure to
#'   use, either \code{"euclidean"} (default) or \code{"maxnorm"}.
#' @param type This specifies the type of interactive plot. \code{"default"}
#'   places everything on one device. \code{"separate"} places condition
#'   selectors on one device and the section on another. (These two options
#'   require XQuartz on OS X). \code{"shiny"} produces a Shiny application.

ceplot <-
function (data, model, response = NULL, S = NULL, C = NULL, sigma = NULL,
  distance = c("euclidean", "maxnorm"), type = c("default", "separate", "shiny")
  , cex.axis = NULL, cex.lab = NULL, tck = NULL, view3d = FALSE, Corder =
  "default", selectortype = "minimal", conf = FALSE, select.colour = "blue",
  select.cex = 1, probs = FALSE, col = "black", pch = NULL, residuals = FALSE)
{
  data <- na.omit(data)
  model <- if (!inherits(model, "list"))
    list(model)
  else model
  model.name <- if (!is.null(names(model)))
    names(model)
  else paste("model", 1:length(model), sep = "_")
  varnamestry <- try(getvarnames(model[[1]]), silent = TRUE)
  response <- if (is.null(response))
    if (class(varnamestry) != "try-error")
      varnamestry$response[1L]
    else stop("could not extract response from 'model'.")
  else if (is.integer(response)){
    colnames(data)[response]
  } else response

  type <- match.arg(type)

## Need to check here if XQuartz is available when on OS X platform.
## capabilities() is hanging up my R session when called with no XQuartz
## installed. The problem seems to be that .Internal(capabilitiesX11()) just
## keeps waiting for X11 to be found, instead of returning FALSE, when some X11
## files are installed, but X11.app/XQuartz.app is missing.
## Resorting to a rough system call of ls on the Utilities directory.

  if (identical(.Platform$OS.type, "unix")){
    if (grepl("^darwin", R.version$os)){
      # if (!capabilities("X11"))
      if (!any(c("XQuartz.app", "X11.app") %in% system2(command = "ls",  args =
        "/Applications/Utilities/", stdout = TRUE))){
        warning("no X11 available, setting 'type' to \"shiny\"")
        type <- "shiny"
      }
    }
  }

## If no section variables have been specified, just try and pick the first
## predictor out of the model.

  S <- if(is.null(S)){
    if (!inherits(varnamestry, "try-error")){
      varnamestry$predictors[1L]
    } else {
      setdiff(colnames(data), response)[1L]
    }
  } else if (is.integer(S)){
    colnames(data)[S]
  } else S

## Hierarchy for specifying C
##   1. If user supplies list, use that exactly (maybe chop length).
##   2. If user supplies vector, use that but order/group it.
##   3. If user supplies nothing, try to extract from model, then order.
##   4. Else bung in everything from the data that isn't 'response' or 'S' and
##        then try to order that and show the top 20.

  if (is.list(C)){
    C <- C[1:min(length(C), 20L)]
  } else if (is.vector(C)){
    C <- arrangeC(data[, setdiff(C, S), drop = FALSE], method = Corder)
  } else if (!inherits(varnamestry, "try-error")){
    possibleC <- unique(unlist(lapply(lapply(model, getvarnames), `[[`, 2)))
    C <- arrangeC(data[, setdiff(possibleC, S), drop = FALSE], method = Corder)
  } else {
    C <- arrangeC(data[, !colnames(data) %in% c(S, response), drop = FALSE],
      method = Corder)
  }
  C <- C[1:min(length(C), 20L)]

  uniqC <- unique(unlist(C))

## A few checks on choices of response, S and C.

  if (any(response %in% uniqC))
    stop("cannot have 'response' variable in 'C'")
  if (any(response %in% S))
    stop("cannot have 'response' variable in 'S'")
  if (!identical(length(intersect(S, uniqC)), 0L))
    stop("cannot have variables common to both 'S' and 'C'")

## Set up col so it is a vector with length equal to nrow(data). Default pch to
## 1, or 21 for using background colour to represent observed values.

  nr.data <- nrow(data)
  col <- rep(col, nr.data)
  pch <- if (is.null(pch)){
    if (identical(length(S), 2L))
      rep(21, nr.data)
    else rep(1, nr.data)
  } else rep(pch, nr.data)

## Make the appropriate call to an internal ceplot function

  if (identical(type, "default")){
    ceplot.interactive(data = data, model = model, response = response, S = S,
      C = C, sigma = sigma, distance = distance, cex.axis = cex.axis, cex.lab =
      cex.lab, tck = tck, view3d = view3d, Corder = Corder, conf = conf,
      separate = FALSE, select.colour = select.colour, select.cex = select.cex,
      probs = probs, col = col, pch = pch, residuals = residuals)
  } else if (identical(type, "separate")){
    ceplot.interactive(data = data, model = model, response = response, S = S,
      C = C, sigma = sigma, distance = distance, cex.axis = cex.axis, cex.lab =
      cex.lab, tck = tck, view3d = view3d, Corder = Corder, conf = conf,
      separate = TRUE, select.colour = select.colour, select.cex = select.cex,
      probs = probs, col = col, pch = pch, select.type = selectortype, residuals
      = residuals)
  } else if (identical(type, "shiny")){
    ceplot.shiny(data = data, model = model, response = response, S = S, C = C,
      cex.axis = cex.axis, cex.lab = cex.lab, tck = tck, Corder = Corder)
  }
}
