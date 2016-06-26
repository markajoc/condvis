#' @title Interactive conditional expectation plot
#'
#' @description Creates an interactive conditional expectation plot, which
#'   consists of two main parts. One part is a single plot depicting a section
#'   through a fitted model surface, or conditional expectation. The other part
#'   shows small data summaries which give the current condition, which can be
#'   altered by clicking with the mouse.
#'
#' @param data A dataframe containing the data to plot
#' @param model A model object, or list of model objects
#' @param response Character name of response in \code{data}
#' @param sectionvars Character name of variable(s) from \code{data} on which to
#'   take a section, can be of length 1 or 2.
#' @param conditionvars Character names of conditioning variables from
#'   \code{data}. These are the predictors which we can set to single values in
#'   order to produce a section. Can be a list of vectors of length 1 or 2. Can
#'   be a character vector, which is then paired up using
#'   \code{\link{arrangeC}}. If \code{NULL}, an attempt will be made to extract
#'   all variable names which are not \code{response} or \code{sectionvars} from
#'   \code{model}, and these will be arranged using \code{\link{arrangeC}}.
#' @param threshold This is a threshold distance. Points further than
#'   \code{threshold} away from the current section will not be visible. Passed
#'   to \code{\link{similarityweight}}.
#' @param distance A character vector describing the type of distance measure to
#'   use, either \code{"euclidean"} (default) or \code{"maxnorm"}.
#' @param type This specifies the type of interactive plot. \code{"default"}
#'   places everything on one device. \code{"separate"} places condition
#'   selectors on one device and the section on another. (These two options
#'   require XQuartz on OS X). \code{"shiny"} produces a Shiny application.
#' @param view3d Logical; if \code{TRUE} plots a three-dimensional
#'   regression surface if possible.
#' @param Corder Character name for method of ordering conditioning variables.
#'   See \code{\link{arrangeC}}.
#' @param selectortype Type of condition selector plots to use. Must be
#'   \code{"minimal"} if \code{type} is \code{"default"}. If \code{type} is
#'   \code{"separate"}, can be \code{"pcp"} (see \code{\link{plotxc.pcp}} or
#'   \code{"full"} (see \code{\link{plotxc.full}}).
#' @param conf Logical; if \code{TRUE} plots confidence bounds (or equivalent)
#'   for models which provide this.
#' @param probs Logical; if \code{TRUE}, shows predicted class probabilities
#'   instead of just predicted classes. Only available if \code{S} specifies two
#'   numeric predictors and the model's predict method provides this.
#' @param col Colour for observed data.
#' @param pch Plot symbols for observed data.
#' @param residuals Logical; if \code{TRUE}, plots a residual versus predictor
#'   plot instead of the usual scale of raw response.
#' @param xsplotpar Plotting parameters for section visualisation as a list,
#'   passed to \code{\link{plotxs}}. Not used.
#' @param modelpar Plotting parameters for models as a list, passed to
#'   \code{\link{plotxs}}. Not used.
#' @param xcplotpar Plotting parameters for condition selector plots as a list,
#'   passed to \code{\link{plotxc}}. Can specify \code{col} for highlighting
#'   current section, \code{cex}.
#'
#' @examples
#' \dontrun{
#' ## Example 1: Multivariate regression, xs one continuous predictor
#'
#' mtcars$cyl <- as.factor(mtcars$cyl)
#'
#' library(mgcv)
#' model1 <- list(
#'     quadratic = lm(mpg ~ cyl + hp + wt + I(wt^2), data = mtcars),
#'     additive = mgcv::gam(mpg ~ cyl + hp + s(wt), data = mtcars))
#'
#' conditionvars1 <- list(c("cyl", "hp"))
#'
#' ceplot(data = mtcars, model = model1, response = "mpg", sectionvars = "wt",
#'   conditionvars = conditionvars1, threshold = 0.3, conf = T)
#'
#' ## Example 2: Binary classification, xs one categorical predictor
#'
#' mtcars$cyl <- as.factor(mtcars$cyl)
#' mtcars$am <- as.factor(mtcars$am)
#'
#' library(e1071)
#' model2 <- list(
#'   svm = svm(am ~ mpg + wt + cyl, data = mtcars, family = "binomial"),
#'   glm = glm(am ~ mpg + wt + cyl, data = mtcars, family = "binomial"))
#'
#' ceplot(data = mtcars, model = model2, sectionvars = "wt", threshold = 1,
#'   type = "shiny")
#'
#' ## Example 3: Multivariate regression, xs both continuous
#'
#' mtcars$cyl <- as.factor(mtcars$cyl)
#' mtcars$gear <- as.factor(mtcars$gear)
#'
#' library(e1071)
#' model3 <- list(svm(mpg ~ wt + qsec + cyl + hp + gear,
#'   data = mtcars, family = "binomial"))
#'
#' conditionvars3 <- list(c("cyl","gear"), "hp")
#'
#' ceplot(data = mtcars, model = model3, sectionvars = c("wt", "qsec"),
#'   threshold = 1, conditionvars = conditionvars3)
#'
#' ceplot(data = mtcars, model = model3, sectionvars = c("wt", "qsec"),
#'     threshold = 1, type = "separate", view3d = T)
#'
#' ## Example 4: Multi-class classification, xs both categorical
#'
#' mtcars$cyl <- as.factor(mtcars$cyl)
#' mtcars$vs <- as.factor(mtcars$vs)
#' mtcars$am <- as.factor(mtcars$am)
#' mtcars$gear <- as.factor(mtcars$gear)
#' mtcars$carb <- as.factor(mtcars$carb)
#'
#' library(e1071)
#' model4 <- list(svm(carb ~ ., data = mtcars, family = "binomial"))
#'
#' ceplot(data = mtcars, model = model4, sectionvars = c("cyl", "gear"),
#'   threshold = 3)
#'
#' ## Example 5: Multi-class classification, xs both continuous
#'
#' data(wine)
#' wine$Class <- as.factor(wine$Class)
#' library(e1071)
#'
#' model5 <- list(svm(Class ~ ., data = wine, probability = TRUE))
#'
#' ceplot(data = wine, model = model5, sectionvars = c("Hue", "Flavanoids"),
#'   threshold = 3, probs = TRUE)
#'
#' ceplot(data = wine, model = model5, sectionvars = c("Hue", "Flavanoids"),
#'   threshold = 3, type = "separate")
#'
#' ceplot(data = wine, model = model5, sectionvars = c("Hue", "Flavanoids"),
#'   threshold = 3, type = "separate", selectortype = "pcp")
#'
#' ## Example 6: Multi-class classification, xs with one categorical predictor,
#' ##            and one continuous predictor.
#'
#' mtcars$cyl <- as.factor(mtcars$cyl)
#' mtcars$carb <- as.factor(mtcars$carb)
#'
#' library(e1071)
#' model6 <- list(svm(cyl ~ carb + wt + hp, data = mtcars, family = "binomial"))
#'
#' ceplot(data = mtcars, model = model6, threshold = 1, sectionvars = c("carb",
#'   "wt"), conditionvars = "hp")
#' }

ceplot <-
function (data, model, response = NULL, sectionvars = NULL, conditionvars = NULL
  , threshold = NULL, distance = c("euclidean", "maxnorm"), type = c("default",
  "separate", "shiny"), view3d = FALSE, Corder = "default", selectortype =
  "minimal", conf = FALSE, probs = FALSE, col = "black", pch = NULL, residuals =
  FALSE, xsplotpar = NULL, modelpar = NULL, xcplotpar = NULL)
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

## Prepare variables

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

#  S <- if(is.null(S)){
#    if (!inherits(varnamestry, "try-error")){
#      varnamestry$predictors[1L]
#    } else {
#      setdiff(colnames(data), response)[1L]
#    }
#  } else if (is.integer(S)){
#    colnames(data)[S]
#  } else S

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
    ceplot.shiny(data = data, model = model, response = response, S = S,
      C = C, sigma = sigma, distance = distance, cex.axis = cex.axis, cex.lab =
      cex.lab, tck = tck, view3d = view3d, Corder = Corder, conf = conf,
      separate = FALSE, select.colour = select.colour, select.cex = select.cex,
      probs = probs, col = col, pch = pch, residuals = residuals)
  }
}
