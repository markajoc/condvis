predict1 <-
# predict wrapper to deal with awkward predict methods
function (object, ..., ylevels = NULL)
{
    type <- if (inherits(object, "nnet"))
      if (is.null(ylevels))
        "raw"
      else "class"
    else if (inherits(object, "rpart"))
            "vector"
        else "response"
    n.trees <- if (inherits(object, "gbm"))
        object$n.trees
    else NULL
    if (inherits(object, "nnet") && !is.null(ylevels)){
      return(factor(predict(object, ..., type = type), levels = ylevels))
    }
    predict(object, ..., n.trees = n.trees, type = type)
}
