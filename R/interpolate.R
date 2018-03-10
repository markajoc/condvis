## Interpolation function.

interpolate <-
function (x, ...)
{
  UseMethod("interpolate", x)
}

interpolate.numeric <- interpolate.integer <-
function (x, ninterp = 3L)
{
  xdiff <- diff(x) / (ninterp + 1L)
  #add <- matrix(xdiff, nrow = ninterp + 1L, ncol = length(xdiff), byrow = TRUE)
  cumsum(c(x[1L], rep(xdiff, each = ninterp + 1L)))
}

## Method dispatch does not seem to be working for factor/character.

interpolate.factor <- interpolate.character <-
function (x, ninterp = 2L)
{
  if (!identical(ninterp %% 2, 0))
    warning("'ninterp' should be even for factor/character vector")
  unlist(list(rep(head(x, 1L), 1L + floor(ninterp / 2L)),
    rep(head(tail(x, -1L), -1L), each = ninterp + 1L),
    rep(tail(x, 1L), 1L + ceiling(ninterp / 2L))))
}
