arrangeC <- function (data, method = "default")
{
  nc.data <- ncol(data)
  if (nc.data <= 2L)
    return(list(colnames(data)))
  data <- na.omit(data)
  nr.data <- nrow(data)
  n <- max(36800 - 6850 * log(nc.data), 100) # approx n that is practical
  if(nr.data > n)
    data <- data[sample(1:nr.data, n, replace = TRUE), ]
  saving <- matrix(nrow = nc.data, ncol = nc.data)
  colnames(saving) <- rownames(saving) <- colnames(data)
  for (i in 1:nc.data){
    for (j in i:nc.data){
      saving[i, j] <-
      saving[j, i] <- savingby2d(data[, i], data[, j], method)
    }
  }
  diag(saving) <- 1
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
