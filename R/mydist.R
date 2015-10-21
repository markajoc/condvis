mydist <- 
function (x, X, p = 2, inf = FALSE)
{
    n <- if (is.vector(X))
        length(X)
    else nrow(X)    
    dif <- abs(as.matrix(X) - matrix(as.numeric(x), nrow = nrow(X), ncol = length(x), byrow = TRUE))
    if (inf)
        return(apply(dif, 1, max))
    tmp <- dif^p
    rowSums(tmp)
}