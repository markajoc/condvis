condtour <-
function(data, model, response, S, C = NULL, path = NULL, ...)
{
    if(!requireNamespace("shiny", quietly = TRUE))
        stop("requires package 'shiny'")
    else if (!exists("shinyApp")) attachNamespace("shiny") 
    if(!requireNamespace("TSP", quietly = TRUE))
        stop("requires package 'TSP'")
    Xc <- data[, unlist(C), drop = FALSE]
    path <- if (is.null(path))
        makepath(Xc, 35)$path
    else path

    ui <- fluidPage(
        titlePanel("", "Conditional tour"),
        fluidRow(
            column(width = 4,
                plotOutput("plotS", height = 400, width = 400)
                , plotOutput("diagnostic1", height = 200, width = 400)
                , plotOutput("diagnostic2", height = 300, width = 300)
            )
            , column(width = 2,
                helpText(strong("Projections of weighting function")),
                #parse(text = paste('plotOutput("plot', 1:2, '", height = 200, 
                #width = 200)', sep = "", collapse = ",\n"))
                plotOutput("plot1", height = 100, width = 180)
                ,plotOutput("plot2", height = 100, width = 180)
                ,plotOutput("plot3", height = 100, width = 180)
                ,plotOutput("plot4", height = 100, width = 180)
                , offset = 0.5)
            , if(ncol(Xc) > 4) {column(width = 2,
                plotOutput("plot5", height = 100, width = 180)
                ,plotOutput("plot6", height = 100, width = 180)
                ,plotOutput("plot7", height = 100, width = 180)
                ,plotOutput("plot8", height = 100, width = 180)
                , offset = 0.2)}
            , column(2,
                helpText(strong("Animate the tour"))
                , sliderInput("tourspeed", "Animation speed (fps): ", min = 0.5,
                    max = 3.5, value = 2)
                , uiOutput("pathslider")
                , helpText(strong("Control how much data displayed"))
                , sliderInput("sigma", "Weighting function parameter: ", 
                    min = 0, max = 3, value = 0.5, step = 0.01)
                , radioButtons("type", "Weighting function type", c("euclidean",
                    "chebyshev"), selected = "euclidean", inline = FALSE)
                , offset = 0.8)
        )
    )
    ind <- 1:ncol(Xc)
    eval(parse(text = paste('
    server <-
    function (input, output)
    {
    output$pathslider <- renderUI({
        sliderInput("pathindex", "Path index: ", min = 1, max =
        nrow(path), value = 1, step = 1, animate =
        animationOptions(interval = 1000/input$tourspeed))
    })
    reactval <- reactive({
        k.full <- matrix(nrow = nrow(Xc), ncol = nrow(path))
        order.full <- list()
        for (i in 1:nrow(path)){
            tmpr <- visualweight(xc = Xc, xc.cond = path[i, ],
                sigma = input$sigma, distance = input$type)
            k.full[, i] <- tmpr$k
            order.full[[i]] <- tmpr$order
            }
        list(k.full = k.full, order.full = order.full)
        })
        output[["plotS"]] <- renderPlot({
            if(!is.null(input$pathindex)){
            vw <- visualweight(xc = Xc, xc.cond = path[input$pathindex, ], 
                sigma = input$sigma, distance = input$type)  
            k <- vw$k                
            do.call(plotxs.shiny, list(xs = data[, S, drop = FALSE], y = 
                data[, response, drop = FALSE], xc.cond = path[input$pathindex, ],
                model = model, data.order = vw$order, data.colour = 
                rgb(1 - k, 1 - k, 1 - k)))
            }
        })
        output$diagnostic1 <- renderPlot({
            par(mar = c(4, 4, 1, 1))
            approx.n.visible <- apply(reactval()$k.full, 2, sum)
            plot(approx.n.visible, xlab = "Path index", ylab = 
                "Approx observations visible", type = "l")
            abline(v = input$pathindex)
        })
        output$diagnostic2 <- renderPlot({
            par(mar = c(4, 4, 1, 1))
            max.k.attained <- apply(reactval()$k.full, 1, max)
            quant <- quantile(max.k.attained, probs = seq(0, 1, 0.1))
            plot(seq(0, 1, 0.1), quant, type = "l")
            points(seq(0, 1, 0.1), quant, pch = 16)
        })
        ',
        paste("output$plot", ind," <- renderPlot({
            if(!is.null(input$pathindex)){
            plotxc(Xc[, ", ind, "], path[input$pathindex, ", ind, "], 
                names(Xc)[", ind, "], sigma = input$sigma, type = input$type)
            }
        })", sep = "", collapse = "\n")
    ,'}', sep = '\n'
    )))
    shinyApp(ui, server)
}