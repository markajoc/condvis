## This is a helper function to try to standardise the output from various
## predict methods. If 'ylevels' is given, it implies that we want the predicted
## class probabilities back, not just the predicted class.

predict1 <-
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
  if (inherits(object, "gbm") && !is.null(ylevels)){
    p1 <- predict(object, ..., n.trees = n.trees, type = type)
    out <- structure(factor(ylevels[apply(p1, 1, which.max)], levels = ylevels),
      probabilities = p1)
    return(out)
  }
  predict(object, ..., n.trees = n.trees, type = type)
}
