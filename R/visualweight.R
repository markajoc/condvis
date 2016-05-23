visualweight <-
function (xc.cond, xc, sigma = NULL, distance = "euclidean", constant = NULL)
{
  do.call(visualweight2(xc = xc), list(xc.cond = xc.cond, sigma = sigma,
    distance = distance, constant = constant))
}
