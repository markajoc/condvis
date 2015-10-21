shinyceplot <-
function(data, model, response = NULL, S = NULL, C = NULL, sigma = NULL,
    threshold = NULL, type = "gaussian", cex.axis = NULL, cex.lab = NULL, tck = NULL,
    view3d = FALSE, method = "default", selectortype = "minimal")
{
    if(!require("shiny"))
        stop("requires package 'shiny'")
    model <- if (!identical(class(model), "list"))
        list(model)
    else model
    model.name <- if (!is.null(names(model)))
        names(model)
    else NULL
    varnamestry <- try(getvarnames(model[[1]]), silent = TRUE)
    response <- if (is.null(response))
        if (class(varnamestry) != "try-error")
           which(colnames(data) == varnamestry$response[1])
        else stop("could not extract response from 'model'.")
    else if (is.character(response))
            which(colnames(data) == response)
        else response
    S <- if(is.null(S)){
         (1:ncol(data))[-response][1L]
        #cat(paste("\n'S' was not specified, picked", colnames(data)[S]))
        } else if (is.character(S))
            vapply(S, function(x) which(colnames(data) == x), numeric(1))
            else S
    C <- if (is.null(C))
        if (class(varnamestry) != "try-error"){
            possibleC <- unique(unlist(lapply(
                lapply(model, getvarnames), `[[`, 2)))
            arrangeC(data[, possibleC[!(possibleC %in% colnames(data)[S])],
                drop = FALSE], method = method)
        } else arrangeC(data[, -c(response, S)])
    else C
    C <- if (all(vapply(C, is.numeric, logical(1))))
        as.list(C)
    else if (all(vapply(C, is.character, logical(1))))
            lapply(C, names2index, names = colnames(data))
        else
            stop("'C' should be a vector or list (containing vectors of length",
                 " 1 or 2) with integer column indices or character variable",
                 " names from 'data'.")
    uniqC <- unique(unlist(C))
    n.selector.cols <- ceiling(length(C) / 4L)
    if (any(response %in% uniqC))
        stop("cannot have 'response' variable in 'C'")
    if (any(response %in% S))
        stop("cannot have 'response' variable in 'S'")
    if (!identical(length(unique(vapply(lapply(model, getvarnames),
        `[[`, character(1), 1))), 1L))
        stop("cannot compare models with different response variables")
    if (!identical(length(intersect(S, uniqC)), 0L))
        stop("cannot have variables common to both 'S' and 'C'")
    Xc.cond <- data[1, uniqC, drop = FALSE]
    tmp <- new.env()
    assign("Xc.cond", Xc.cond, tmp)
    Xc <- data[, uniqC, drop = FALSE]
    eval(parse(text = paste("
        ui <- fluidPage(
            fluidRow(
                column(4,
                    plotOutput('plotS', height = 400, width = 400)
                )
                , column(2,",
                    paste("plotOutput('plotC", 1:length(C), "', height = 200, width = 200, click = 'plotC", 1:length(C), "click')", sep = "", collapse = ",\n")
                ,", textOutput('text')
                )
                , column(2,
                    sliderInput('sigma', 'Weighting function parameter: ', 0.01, 5, step = 0.01, value = 1),
                    radioButtons('type', 'Weighting function type:', c('gaussian', 'spherical', 'box'))
                , offset = 1)
            )
        )
    ")))

    eval(parse(text = paste("
        server <-
        function (input, output){
            output$plotS <- renderPlot({
            Xc.cond <- get('Xc.cond', envir = tmp)", paste("
            if (!is.null(input$plotC", 1:length(C), "click$x)){
                arefactors <- unlist(lapply(data[, C[[", 1:length(C), "]], drop = FALSE], is.factor))
                if (identical(length(arefactors), 1L)){
                    if(arefactors)
                        Xc.cond[, names(data)[C[[", 1:length(C), "]]]] <- factor(levels(data[, names(data)[C[[", 1:length(C), "]]]])[which.min(abs(input$plotC", 1:length(C), "click$x - (1:length(levels(data[, names(data)[C[[", 1:length(C), "]]]])))))], levels = levels(data[, names(data)[C[[", 1:length(C), "]]]]))
                    else Xc.cond[, names(data)[C[[", 1:length(C), "]]]] <- input$plotC", 1:length(C), "click$x
                }
                if (identical(length(arefactors), 2L)){
                    if (all(arefactors)){
            
                    } else if (any(arefactors)){
                       Xc.cond[, names(data)[C[[", 1:length(C), "]]][which(arefactors)]] <- factor(levels(data[, names(data)[C[[", 1:length(C), "]]][which(arefactors)]])[which.min(abs(input$plotC", 1:length(C), "click$x - (1:length(levels(data[, names(data)[C[[", 1:length(C), "]]][which(arefactors)]])))))], levels = levels(data[, names(data)[C[[", 1:length(C), "]]][which(arefactors)]]))
                        Xc.cond[, names(data)[C[[", 1:length(C), "]]][which(!arefactors)]] <- input$plotC", 1:length(C), "click$y
                        } else {
                            Xc.cond[, names(data)[C[[", 1:length(C), "]]][1]] <- input$plotC", 1:length(C), "click$x
                            Xc.cond[, names(data)[C[[", 1:length(C), "]]][2]] <- input$plotC", 1:length(C), "click$y
                        }
            
                }
                assign('Xc.cond', Xc.cond, envir = tmp) 
            }
            ", sep = "", collapse = ""),"
                    vw <- visualweight(xc = Xc, xc.cond = get('Xc.cond', envir = tmp), sigma = input$sigma, threshold = 0.2, type = input$type)
                    k <- vw$k
                    data.colour <- rgb(1 - k, 1 - k, 1 - k)
                    data.order <- vw$order
                    plotxsobject <- plotxs(xs = data[, S, drop = FALSE],
                        y = data[, response, drop = FALSE], xc.cond = get('Xc.cond', envir = tmp), model = model,
                        model.colour = NULL, model.lwd = NULL, model.lty = NULL,
                        model.name = model.name, yhat = NULL, mar = NULL,
                        data.colour = data.colour, data.order = data.order, view3d = view3d)
            })
            ", paste("
            output$plotC", 1:length(C), " <- renderPlot({
                Xc.cond <- get('Xc.cond', envir = tmp)
            if (!is.null(input$plotC", 1:length(C), "click$x)){
                arefactors <- unlist(lapply(data[, C[[", 1:length(C), "]], drop = FALSE], is.factor))
                if (identical(length(arefactors), 1L)){
                    if(arefactors)
                        Xc.cond[, names(data)[C[[", 1:length(C), "]]]] <- factor(levels(data[, names(data)[C[[", 1:length(C), "]]]])[which.min(abs(input$plotC", 1:length(C), "click$x - (1:length(levels(data[, names(data)[C[[", 1:length(C), "]]]])))))], levels = levels(data[, names(data)[C[[", 1:length(C), "]]]]))
                    else Xc.cond[, names(data)[C[[", 1:length(C), "]]]] <- input$plotC", 1:length(C), "click$x
                }
                if (identical(length(arefactors), 2L)){
                    if (all(arefactors)){
            
                    } else if (any(arefactors)){
                       Xc.cond[, names(data)[C[[", 1:length(C), "]]][which(arefactors)]] <- factor(levels(data[, names(data)[C[[", 1:length(C), "]]][which(arefactors)]])[which.min(abs(input$plotC", 1:length(C), "click$x - (1:length(levels(data[, names(data)[C[[", 1:length(C), "]]][which(arefactors)]])))))], levels = levels(data[, names(data)[C[[", 1:length(C), "]]][which(arefactors)]]))
                        Xc.cond[, names(data)[C[[", 1:length(C), "]]][which(!arefactors)]] <- input$plotC", 1:length(C), "click$y
                        } else {
                            Xc.cond[, names(data)[C[[", 1:length(C), "]]][1]] <- input$plotC", 1:length(C), "click$x
                            Xc.cond[, names(data)[C[[", 1:length(C), "]]][2]] <- input$plotC", 1:length(C), "click$y
                        }
            
                }
                cat(unlist(lapply(Xc.cond, is.factor)))  
                cat('\n')  
                assign('Xc.cond', Xc.cond, envir = tmp) 
            }            
            
                plotxc(xc = data[, C[[", 1:length(C), "]]], xc.cond =
                   get('Xc.cond', envir = tmp)[, names(data)[C[[", 1:length(C), "]]], drop = FALSE], name = colnames(data)[C[[", 1:length(C), "]]],
                   select.colour = 'blue', select.lwd = 2, cex.axis = cex.axis,
                   cex.lab = cex.lab, tck = tck)
            })", sep = "", collapse = "\n"),"
        }
    ")))


    shinyApp(ui, server)
}