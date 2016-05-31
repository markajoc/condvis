## This is a diagnostic plot to be used with condtour. This is a static plot.
## Shows the deciles of the maximum visual weight given to the data by the
## current conditional tour. If too many observations are reaching maximum
## visual weight of 1, the 'sigma' used in visualweight might be too big. If
## no observations are reaching maximum visual weights of 0.3, we may not see
## any data on the sections. 

plotmaxk <-
function (maxk)
{
  seq01 <- seq(0, 1, 0.1)
  q <- quantile(maxk, probs = seq01)
  plot(seq01, q, type = "l", xlab = "proportion of data", ylab =
    "max k attained", ylim = c(0, 1))
  points(seq01, q, pch = 16)
}

#update.maxk <-
#function (object, ...)
#{
#
#}
