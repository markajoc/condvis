makenewdata <- 
function (xs, xc.cond)
{
    newdata <- cbind(xs, xc.cond[rep(1, nrow(xs)), ])
    colnames(newdata) <- c(colnames(xs), colnames(xc.cond))
    rownames(newdata) <- NULL
    return(newdata)	
}
