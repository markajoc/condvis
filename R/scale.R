scale2unit <-
function (x)
{
  x <- as.numeric(x)
  mn <- min(x)
  mx <- max(x)
  out <- (x - mn) / (mx - mn)
  out
}
