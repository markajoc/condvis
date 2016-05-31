## This is a slightly hacky function to extract confidence bounds on predictions
## from certain classes of model, and pass them on in a standard fashion. Most
## notable is the approach for models of class "custompred", which will be a
## wrapper object for some model that has no predict method, designed to behave
## like an lm object under a predict call.

confpred <-
function (model, newdata)
{
  if (identical(class(model), "lm")){
    pred <- predict(object = model, newdata = newdata, interval = "confidence",
      type = "response")
    upr <- pred[, "upr"]
    lwr <- pred[, "lwr"]
    return(cbind(lwr, upr))
  }
  if (inherits(model, "custompred")){
    pred <- predict(object = model, newdata = newdata, interval = "confidence")
    upr <- pred[, "upr"]
    lwr <- pred[, "lwr"]
    return(cbind(lwr, upr))
  }
  if (identical(class(model), c("glm", "lm"))){
    pred <- predict(object = model, newdata = newdata, type = "link", se.fit =
      TRUE)
    upr <- model$family$linkinv(pred$fit + (2 * pred$se.fit))
    lwr <- model$family$linkinv(pred$fit - (2 * pred$se.fit))
    return(cbind(lwr, upr))
    }
  if (identical(class(model), c("gam", "glm", "lm")) && "mgcv.conv" %in% names(
    model)){
    pred <- predict(object = model, newdata = newdata, type = "link", se.fit =
      TRUE)
    upr <- model$family$linkinv(pred$fit + (2 * pred$se.fit))
    lwr <- model$family$linkinv(pred$fit - (2 * pred$se.fit))
    return(cbind(lwr, upr))
  }
  NULL
}
