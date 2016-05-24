visualweight2 <- function (xc)
{
  nrow.xc <- nrow(xc)
  colnames.xc <- colnames(xc)
  arefactors <- vapply(xc, is.factor, logical(1))
  xc.factors <- data.matrix(xc[, arefactors, drop = FALSE])
  xc.num <- data.matrix(xc[, !arefactors, drop = FALSE])
  rm(xc)
  x.scaled <- scale(xc.num)
  k <- rep(0, nrow.xc)
  function (xc.cond, sigma = NULL, distance = c("euclidean", "maxnorm"),
    constant = NULL)
  {
    sigma <- if (is.null(sigma))
      1
    else sigma
    distance <- match.arg(distance)
    constant <- if (is.null(constant))
      10 * sigma
    else sigma
    p <- if (identical(distance, "maxnorm")) 1 else 2
    xc.cond <- xc.cond[, colnames.xc, drop = FALSE]
    xc.cond.factors <- data.matrix(xc.cond[, arefactors, drop = FALSE])
    xc.cond.num <- data.matrix(xc.cond[, !arefactors, drop = FALSE])
    factormatches <- if (any(arefactors)){
      which((nfactormatches <- rowSums(xc.factors == matrix(xc.cond.factors,
        ncol = length(xc.cond.factors), nrow = nrow.xc, byrow = TRUE))) ==
        length(xc.cond.factors))
    } else {rep(TRUE, nrow.xc)}
    if (length(factormatches) > 0){
      if (all(arefactors)){
        k[factormatches] <- 1
      } else {
        xcond.scaled <- (xc.cond.num - attr(x.scaled, "scaled:center")) / attr(
          x.scaled, "scaled:scale")
        d <- dist1(xcond.scaled, x.scaled[factormatches, ], inf = identical(
          distance, "maxnorm")) + if (any(arefactors)) constant * (sum(
          arefactors) - nfactormatches[factormatches]) else 0
        k[factormatches] <- c(1, 0.7, 0.4, 0)[findInterval(d, c(0, (0.3 * sigma)
          ^ p, (0.6 * sigma) ^ p, sigma ^ p))]
      }
    }
    list(k = k, sigma = sigma, distance = distance)
  }
}
