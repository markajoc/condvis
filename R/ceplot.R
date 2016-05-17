ceplot <-
function (data, model, response = NULL, S = NULL, C = NULL, sigma = NULL,
    distance = "euclidean", type = "default", cex.axis = NULL, cex.lab = NULL,
    tck = NULL, view3d = FALSE, Corder = "default", selectortype = "minimal",
    conf = FALSE, select.colour = "blue", select.cex = 1, probs = FALSE, col =
    "black", pch = NULL, residuals = FALSE)
{
    data <- na.omit(data)
    model <- if (!inherits(model, "list"))
        list(model)
    else model
    model.name <- if (!is.null(names(model)))
        names(model)
    else paste("model", 1:length(model), sep = "_")
    varnamestry <- try(getvarnames(model[[1]]), silent = TRUE)
    response <- if (is.null(response))
        if (class(varnamestry) != "try-error")
           varnamestry$response[1L]
        else stop("could not extract response from 'model'.")
    else if (is.integer(response)){
      colnames(data)[response]
    } else response

    S <- if(is.null(S)){
      if (!inherits(varnamestry, "try-error")){
        varnamestry$predictors[1L]
      } else {
        setdiff(colnames(data), response)[1L]
      }
    } else if (is.integer(S)){
      colnames(data)[S]
    } else S


## TODO: need new hierarchy for specifying C
##         1. If user supplies list, use that exactly (maybe chop length)
##         2. If user supplies vector, use that but order/group it
##         3. If user supplies nothing, try to extract from model, then order
##         4. Else bung in everything from the data that isn't 'response' or 'S'
##            and then try to order that and show the top 20
   if (is.list(C)){
     C <- C[1:min(length(C), 20L)]
   } else if (is.vector(C)){
     C <- arrangeC(data[, setdiff(C, S), drop = FALSE], method = Corder)
   } else if (!inherits(varnamestry, "try-error")){
     possibleC <- unique(unlist(lapply(lapply(model, getvarnames), `[[`, 2)))
     C <- arrangeC(data[, setdiff(possibleC, S),
         drop = FALSE], method = Corder)
   } else {
     C <- arrangeC(data[, !colnames(data) %in% c(S, response), drop = FALSE],
       method = Corder)
   }
     C <- C[1:min(length(C), 20L)]

    uniqC <- unique(unlist(C))
    if (any(response %in% uniqC))
        stop("cannot have 'response' variable in 'C'")
    if (any(response %in% S))
        stop("cannot have 'response' variable in 'S'")
    if (!identical(length(intersect(S, uniqC)), 0L))
        stop("cannot have variables common to both 'S' and 'C'")
    col <- rep(col, nrow(data))
    pch <- if (is.null(pch)){
      if (is.factor(data[, response]))
        rep(21, nrow(data))
      else rep(1, nrow(data))
    } else rep(pch, nrow(data))
    if (identical(type, "default")){
        ceplot.interactive(data = data, model = model, response = response,
            S = S, C = C, sigma = sigma, distance = distance, cex.axis =
            cex.axis, cex.lab = cex.lab, tck = tck, view3d = view3d, Corder =
            Corder, conf = conf, separate = FALSE, select.colour =
            select.colour, select.cex = select.cex, probs = probs, col = col,
            pch = pch, residuals = residuals)
    } else if (identical(type, "separate") && selectortype %in% c("minimal",
      "pcp", "full")){
        ceplot.interactive(data = data, model = model, response = response,
            S = S, C = C, sigma = sigma, distance = distance, cex.axis =
            cex.axis, cex.lab = cex.lab, tck = tck, view3d = view3d, Corder =
            Corder, conf = conf, separate = TRUE, select.colour = select.colour,
            select.cex = select.cex, probs = probs, col = col, pch = pch,
            select.type = selectortype, residuals = residuals)
    } else if (identical(type, "shiny")){
        ceplot.shiny(data = data, model = model, response = response, S = S,
            C = C, cex.axis = cex.axis, cex.lab = cex.lab, tck = tck,
            Corder = Corder)
    }
}
