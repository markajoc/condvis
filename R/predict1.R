predict1 <- 
function (object, ...)
{
    type <- if ("nnet" %in% class(object))
        "class"
    else "response"    
    predict(object, ..., n.trees = object$n.trees, type = type)
}