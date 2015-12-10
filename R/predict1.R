predict1 <- 
function (object, ...)
{
    predict(object, ..., n.trees = object$n.trees, type = "class")
}