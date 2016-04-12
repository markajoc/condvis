visualweight <-
function (xc.cond, xc, sigma = NULL, distance = "euclidean", basicoutput =
    FALSE)
{
  if(!is.data.frame(xc))
    stop("'xc' should be a data.frame.")
  if(!is.data.frame(xc.cond) || !(all(names(xc) %in% names(xc.cond))))
    stop("'xc.cond' must be a data.frame with 1 row,",
      " and same names as 'xc'.")
  sigma <- if (is.null(sigma))
    1
  else sigma
  xc.cond <- xc.cond[, colnames(xc), drop = FALSE]
  if (identical(distance, "daisy")){
    d <- daisy1(rbind(xc.cond, xc), stand = TRUE)
    k <- rep(0, nrow(xc))
    k[d < sigma] <- 0.4
    k[d < (0.6 * sigma)] <- 0.7
    k[d < (0.3 * sigma)] <- 1
  } else {
    arefactors <- vapply(xc, is.factor, logical(1))
    xc.factors <- xc[, arefactors, drop = FALSE]
    xc.cond.factors <- xc.cond[, arefactors, drop = FALSE]
    xc.num <- xc[, !arefactors, drop = FALSE]
    xc.cond.num <- xc.cond[, !arefactors, drop = FALSE]
    tmp <- as.matrix(xc.factors)
    tmp.cond <- as.matrix(xc.cond.factors)
    rownames(tmp) <- colnames(tmp) <- rownames(tmp.cond) <-
      colnames(tmp.cond) <- NULL
    factormatches <- if (any(arefactors)){
      rowSums(tmp == matrix(tmp.cond, ncol = length(tmp.cond), nrow = nrow(tmp),
        byrow = TRUE)) == length(tmp.cond)
    } else rep(TRUE, nrow(xc))
    k <- rep(0, nrow(xc))
    if (all(!factormatches))
      return(list(k = rep(0, nrow(xc)), order = integer(0), sigma = sigma,
        distance = distance))
    if (all(arefactors)){
      k[factormatches] <- 1
      return(list(k = k, order = which(k == 1), sigma = sigma, distance =
        distance))
    }
    if (any(c("euclidean", "maxnorm") %in% distance)){
      x.mean <- colMeans(xc.num)
      x.sd <- apply(xc.num, 2L, sd)
      x.scaled <- scale(xc.num)[factormatches, ]
      xcond.scaled <- (xc.cond.num - x.mean) / x.sd
      d <- dist1(xcond.scaled, x.scaled, inf = identical(distance, "maxnorm"))
      p <- if (identical(distance, "maxnorm")) 0 else 2
      k[factormatches][d < (sigma ^ p)] <- 0.4
      k[factormatches][d < ((0.6 * sigma) ^ p)] <- 0.7
      k[factormatches][d < ((0.3 * sigma) ^ p)] <- 1
    } else stop("unrecognised distance type")
  }
  if (basicoutput)
    return(k)
  else {
    k.order <- order(k)
    k.order.trimmed <- k.order[k[k.order] > 0]
    list(k = k, order = k.order.trimmed, sigma = sigma, distance = distance)
  }
}
