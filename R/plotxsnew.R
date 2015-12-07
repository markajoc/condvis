plotxs1 <- 
function (xs, y, xc.cond, model, model.colour = NULL, model.lwd = NULL, 
    model.lty = NULL, model.name = NULL, yhat = NULL, mar = NULL, data.colour = 
    NULL, data.order = NULL, view3d = FALSE, theta3d = 45, phi3d = 20)
{
    if (!(ncol(xs) %in% 1:2))
        stop("xs must be a dataframe with 1 or 2 columns")
    model <- if (!is.list(model))
        list(model)
    else model    
    model.colour <- if (is.null(model.colour)){
        if (requireNamespace("RColorBrewer", quietly = TRUE))
		    RColorBrewer::brewer.pal(n = max(length(model), 3L), name = "Dark2")
		else rainbow(max(length(model), 4L))   
    } else rep(model.colour, length.out = length(model))
    model.lwd <- if (is.null(model.lwd)) 
        rep(2, length(model))
    else rep(model.lwd, length.out = length(model))
    model.lty <- if (is.null(model.lty)) 
        rep(1, length(model))
    else rep(model.lty, length.out = length(model))
    model.name <- if (is.null(model.name)) 
        vapply(model, function(x) tail(class(x), n = 1L), character(1))
    else model.name
    data.order <- if (is.null(data.order))
        1:nrow(xs)
    else data.order 
    data.colour <- if (is.null(data.colour))
        rep("gray", length(data.order))
    else data.colour  
    if (identical(ncol(xs), 1L)){
        # xs has one column
        if (is.factor(xs[, 1L])){
            # xs is a factor
            if (is.factor(y[, 1L])){
                # y is factor
                plot.type <- "ff"
            } else {
                # y is continuous
                plot.type <- "cf"
            }
        } else {
            #xs is continuous
            if (is.factor(y[, 1L])){
                # y is factor
                plot.type <- "fc"
            } else {
                # y is continuous
                plot.type <- "cc"
            }
        }   
    } else {
        # xs has two columns
        arefactorsxs <- vapply(xs, is.factor, logical(1L))
        if (all(arefactorsxs)){
            # xs are both factors
            if (is.factor(y[, 1L])){
                # y is factor
                plot.type <- "fff"
            } else {
                # y is continuous
                plot.type <- "cff"
            }
        } else {
            if (any(arefactorsxs)){
                # xs is one factor, one continuous
                if (is.factor(y[, 1L])){
                    # y is factor
                    plot.type <- "ffc"
                } else {
                    # y is continuous
                    plot.type <- "cfc"
                }
            } else {
                # xs are both continuous
                if (is.factor(y[, 1L])){
                    # y is factor
                    plot.type <- "fcc"
                } else {
                    # y is continuous
                    plot.type <- "ccc"
                    if (view3d){
                        yhat <- if (is.null(yhat))
                            lapply(model[1], predict, type = "response")
                        else yhat
                        
                    } else {
                    
                    }
                }
            }
        }
    }  
}