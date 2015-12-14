predict1 <- 
# predict wrapper to deal with awkward predict methods
function (object, ...)
{
    type <- if ("nnet" %in% class(object))
        "class"
    else if ("rpart" %in% class(object)) 
        "vector"
    else "response"    
    predict(object, ..., n.trees = object$n.trees, type = type)
}