## This is a helper function for use in parallel coordinates plots in plotxc.pcp

scale2unit <-
function (x)
{
  x <- as.numeric(x)
  mn <- min(x)
  mx <- max(x)
  out <- (x - mn) / (mx - mn)
  out
}
