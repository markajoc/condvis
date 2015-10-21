mydist <- 
function (x, X, p = 2, inf = FALSE)
{
    X <- as.matrix(X)
    dif <- abs(X - matrix(as.numeric(x), nrow = nrow(X), ncol = length(x), byrow = TRUE))
    if (inf)
        return(apply(dif, 1, max))
    tmp <- dif^p
    rowSums(tmp)
}
