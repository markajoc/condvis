predict1 <-
# predict wrapper to deal with awkward predict methods
function (object, ...)
{
    type <- if (inherits(object, "nnet"))
        "class"
    else if (inherits(object, "rpart"))
            "vector"
        else "response"
    n.trees <- if (inherits(object, "gbm"))
        object$n.trees
    else NULL
    predict(object, ..., n.trees = n.trees, type = type)
}
