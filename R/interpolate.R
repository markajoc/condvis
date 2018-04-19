#' @title Interpolate
#'
#' @description Interpolate a numeric or factor vector.
#'
#' @param x A numeric or factor vector.
#' @param ninterp The number of points to interpolate between observations. It
#'   should be an even number for sensible results on a factor/character vector.
#' @param ... Not used.

interpolate <-
function (x, ...)
{
  UseMethod("interpolate", x)
}

#' @rdname interpolate
#' @method interpolate numeric

interpolate.numeric <-
function (x, ninterp = 4L, ...)
{
  if (ninterp < 0)
    stop("'ninterp' should be >= 0")
  xdiff <- diff(x) / (ninterp + 1L)
  cumsum(c(x[1L], rep(xdiff, each = ninterp + 1L)))
}

#' @rdname interpolate
#' @method interpolate integer

interpolate.integer <- interpolate.numeric

#' @rdname interpolate
#' @method interpolate factor

interpolate.factor <-
function (x, ninterp = 4L, ...)
{
  if (ninterp < 0)
    stop("'ninterp' should be >= 0")
  if (!identical(ninterp %% 2, 0))
    warning("'ninterp' should be even for factor/character vector")
  unlist(list(rep(head(x, 1L), 1L + floor(ninterp / 2L)),
    rep(head(tail(x, -1L), -1L), each = ninterp + 1L),
    rep(tail(x, 1L), 1L + ceiling(ninterp / 2L))))
}

#' @rdname interpolate
#' @method interpolate character

interpolate.character <- interpolate.factor
