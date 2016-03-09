## function to calculate a reasonable mode for both numerics and factors, used
## to initialise xc.cond

mode1 <- function (x, breaks = 10)
{
  if (is.factor(x)){
    ux <- unique(x)
    out <- ux[which.max(tabulate(match(x, ux)))]
  } else if (is.numeric(x)){
    cx <- cut(x, breaks = breaks)
    ucx <- unique(cx)
    bin <- ucx[which.max(tabulate(match(cx, ucx)))]
    out <- mean(as.numeric((strsplit(gsub("]", "", gsub("\\(", "",
      as.character(bin))), ",")[[1]])))
  } else stop("mode function expects factors or numerics only")
  out
}

## best used as data.frame(lapply(data, mode1))
