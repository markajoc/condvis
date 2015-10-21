names2index <-
function (C, names)
{
    if(!is.character(C) | !is.vector(C)) 
        stop("'C' should be a character vector")
    if(!is.character(names) | !is.vector(names)) 
        stop("'names' should be a character vector")
    if(any(!(unlist(C) %in% names)))
        stop("All values in 'C' should appear in 'names'")    
    vapply(C, function(x) which(x == names), numeric(1))
}
