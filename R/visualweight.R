visualweight <-
function (xc.cond, xc, sigma = NULL, distance = "euclidean", basicoutput =
    FALSE, q = NULL)
{
  if(!is.data.frame(xc))
    stop("'xc' should be a data.frame.")
  if(!is.data.frame(xc.cond) || !(all(names(xc) %in% names(xc.cond))))
    stop("'xc.cond' must be a data.frame with 1 row,",
      " and same names as 'xc'.")
  sigma <- if (is.null(sigma))
    1
  else sigma
  p <- if (identical(distance, "maxnorm")) 1 else 2
  xc.cond <- xc.cond[, colnames(xc), drop = FALSE]
    arefactors <- vapply(xc, is.factor, logical(1))
    xc.factors <- as.matrix(xc[, arefactors, drop = FALSE])
    xc.cond.factors <- as.matrix(xc.cond[, arefactors, drop = FALSE])
    xc.num <- xc[, !arefactors, drop = FALSE]
    xc.cond.num <- xc.cond[, !arefactors, drop = FALSE]
    rownames(xc.factors) <- colnames(xc.factors) <- rownames(xc.cond.factors) <-
      colnames(xc.cond.factors) <- NULL
    k <- rep(0, nrow(xc))
    if (is.null(q)){
      factormatches <- if (any(arefactors)){
        which(rowSums(xc.factors == matrix(xc.cond.factors, ncol = length(
          xc.cond.factors), nrow = nrow(xc.factors), byrow = TRUE)) == length(
          xc.cond.factors))
      } else rep(TRUE, nrow(xc))
      if (length(factormatches) < 1L)
        return(list(k = rep(0, nrow(xc)), order = integer(0), sigma = sigma,
          distance = distance))
      if (all(arefactors)){
        k[factormatches] <- 1
        return(list(k = k, order = which(k == 1), sigma = sigma, distance =
          distance))
      }
    } else {
      fmr <- factormatchratio(xc.cond.factors, xc.factors)
      d2 <- length(xc.cond) * (6 - 6 * fmr ^ 2) ^ p
      factormatches <- which(d2 < (sigma ^ p))
    }
    if (any(c("euclidean", "maxnorm") %in% distance)){
      x.scaled <- scale(xc.num)
      xcond.scaled <- (xc.cond.num - attr(x.scaled, "scaled:center")) / attr(
        x.scaled, "scaled:scale")
      d <- dist1(xcond.scaled, x.scaled[factormatches, ], inf = identical(
        distance, "maxnorm"))
      k[factormatches][d < (sigma ^ p)] <- 0.4
      k[factormatches][d < ((0.6 * sigma) ^ p)] <- 0.7
      k[factormatches][d < ((0.3 * sigma) ^ p)] <- 1
    } else stop("unrecognised distance type")
  if (basicoutput)
    return(k)
  else {
    k.order <- order(k)
    k.order.trimmed <- k.order[k[k.order] > 0]
    list(k = k, order = k.order.trimmed, sigma = sigma, distance = distance)
  }
}
