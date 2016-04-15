visualweight2 <- function (xc)
{
  arefactors <- vapply(xc, is.factor, logical(1))
  xc.factors <- data.matrix(xc[, arefactors, drop = FALSE])
  xc.num <- data.matrix(xc[, !arefactors, drop = FALSE])
  x.scaled <- scale(xc.num)
  k <- rep(0, nrow(xc))
  function (xc.cond, sigma = 1, distance = "euclidean", basicoutput = FALSE, q =
    NULL)
  {
    p <- if (identical(distance, "maxnorm")) 1 else 2
    xc.cond.factors <- data.matrix(xc.cond[, arefactors, drop = FALSE])
    xc.cond.num <- data.matrix(xc.cond[, !arefactors, drop = FALSE])
    factormatches <- if (any(arefactors)){
      which({nfactormatches <- rowSums(xc.factors == matrix(xc.cond.factors,
        ncol = length(xc.cond.factors), nrow = nrow(xc.factors), byrow = TRUE))}
        == length(xc.cond.factors))
    } else {rep(TRUE, nrow(xc))}
    if (length(factormatches) < 1L)
      return(list(k = rep(0, nrow(xc)), order = integer(0), sigma = sigma,
        distance = distance))
    if (all(arefactors)){
      k[factormatches] <- 1
      return(list(k = k, order = which(k == 1), sigma = sigma, distance =
        distance))
    }
    if (any(c("euclidean", "maxnorm") %in% distance)){
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
}
