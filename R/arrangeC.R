#' @title Make a list of variable pairings for condition selecting plots
#' produced by plotxc
#'
#' @description This function arranges a number of variables in pairs, ordered
#' by their bivariate relationships. The goal is to discover which variable
#' pairings are most helpful in avoiding extrapolations when exploring the data
#' space. Variable pairs with strong bivariate dependencies (not necessarily
#' linear) are chosen first. The bivariate dependency is measured using
#' \code{\link{savingby2d}}. Each variable appears in the output only once.
#'
#' @param data A dataframe
#' @param method The character name for the method to use for measuring
#'   bivariate dependency, passed to \code{\link{savingby2d}}.
#'
#' @return A list containing character vectors giving variable pairings.
#'
#' @details If \code{data} is so big as to make \code{arrangeC} very slow, a
#'   random sample of rows is used instead. The bivariate dependency measures
#'   are rough, and the ordering algorithm is a simple greedy one, so it is not
#'   worth allowing it too much time. This function exists mainly to provide a
#'   helpful default ordering/pairing for \code{\link{ceplot}}.
#'
#' @seealso \code{\link{savingby2d}}
#'
#' @examples
#' data(powerplant)
#'
#'pairings <- arrangeC(powerplant)
#'
#'dev.new(height = 2, width = 2 * length(pairings))
#'par(mfrow = c(1, length(pairings)))
#'
#'for (i in seq_along(pairings)){
#'  plotxc(powerplant[, pairings[[i]]], powerplant[1, pairings[[i]]],
#'    select.col = NA)
#'}
#'
#' @references O'Connell M, Hurley CB and Domijan K (2017). “Conditional
#'   Visualization for Statistical Models: An Introduction to the
#'   \strong{condvis} Package in R.”\emph{Journal of Statistical Software},
#'   \strong{81}(5), pp. 1-20. <URL:http://dx.doi.org/10.18637/jss.v081.i05>.

arrangeC <- function (data, method = "default")
{
  nc.data <- ncol(data)
  if (nc.data <= 2L)
    return(list(colnames(data)))
  data <- na.omit(data)
  nr.data <- nrow(data)
  if (nr.data < 5)
    stop("'data' has less than 5 non-missing rows")

## Calculate a ceiling on the number of rows of 'data' we will use. This is just
## based on a few quick tests on a personal computer, to keep the computation
## time on the order of seconds.

  n <- max(36800 - 6850 * log(nc.data), 100)
  if(nr.data > n)
    data <- data[sample(1:nr.data, n, replace = TRUE), ]

## Construct a matrix of the bivariate dependencies which will be used to order
## the variables

  saving <- matrix(nrow = nc.data, ncol = nc.data)
  colnames(saving) <- rownames(saving) <- colnames(data)
  for (i in 1:nc.data){
    for (j in i:nc.data){
      saving[i, j] <-
      saving[j, i] <- savingby2d(data[, i], data[, j], method)
    }
  }
  diag(saving) <- 1

## Simple greedy ordering of pairs

  C <- list()
  i <- 1L
  while(ncol(saving) > 2){
    pair <- which(saving == min(saving), arr.ind = TRUE)[1L, ]
    C[[i]] <- colnames(saving)[pair]
    saving <- saving[-pair, -pair, drop = FALSE]
    i <- i + 1L
  }
  C[[i]] <- colnames(saving)
  C
}
