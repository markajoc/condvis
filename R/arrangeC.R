arrangeC <- function (data, method = "default")
{
    if (ncol(data) <= 2L)
        return(list(colnames(data)))
    data <- na.omit(data)
    n <- max(36800 - 6850 * log(ncol(data)), 100) # approx n that is practical
    nr <- nrow(data)
    if(nr > n)
      data <- data[sample(1:nr, n, replace = TRUE), ]
    saving <- matrix(nrow = ncol(data), ncol = ncol(data))
    colnames(saving) <- rownames(saving) <- colnames(data)
    for (i in 1:ncol(saving)){
        for (j in i:ncol(saving)){
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
