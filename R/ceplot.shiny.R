ceplot.shiny <-
function (data, model, response = NULL, S = NULL, C = NULL, sigma = NULL,
  distance = "euclidean", cex.axis = NULL, cex.lab = NULL, tck = NULL, view3d =
  FALSE, Corder = "default", conf = FALSE, separate = TRUE, select.colour =
  "blue", select.cex = 1, select.lwd = 2, select.type = "minimal", probs = FALSE
  , col = "black", pch = 1, residuals = FALSE, xc.cond = NULL)
{
  if(!requireNamespace("shiny", quietly = TRUE))
    stop("requires package 'shiny'")
  else if (!exists("shinyApp")) attachNamespace("shiny")
  uniqC <- unique(unlist(C))
  xc.cond <- if (is.null(xc.cond))
    data[1, !colnames(data) %in% c(S, response)]
  else xc.cond
  #data.frame(lapply(data[, !colnames(data) %in% c(S, response)], mode1))
  xcplots <- list()
  coords <- matrix(ncol = 4L, nrow = length(C))
  plotlegend <- length(S) == 2
  n.selector.cols <- ceiling(length(C) / 4L)
  selector.colwidth <- 2
  height <- 8
  col <- rep(col, length.out = nrow(data))
  vwfun <- .visualweight(xc = data[, uniqC, drop = FALSE])

  ui <-
  '
  library(shiny)
  load("app.Rdata")
  h <- "200px"
  basicPage(
    column(3,
      if (FALSE) {
        tabsetPanel(
          tabPanel("Contour", plotOutput("plotS", height = "100%", width = "80%"
            ), value = 1),
          tabPanel("Perspective", plotOutput("plotS2", height = "100%", width =
            "80%"), value = 2),
          id = "tab"
        )
      } else plotOutput("plotS", height = "300px", width = "300px"),
      actionButton("saveButton", "Take snapshot (pdf)"),
      actionButton("deployButton", "Deploy app to web"),
      conditionalPanel(condition = "input.tab == 2", numericInput("phi",
        "Vertical rotation: ", 20, -180, 180)),
      conditionalPanel(condition = "input.tab == 2", numericInput("theta",
        "Horizontal rotation: ", 45, -180, 180)),
      sliderInput("sigma", "Weighting function parameter: ", 0.01, 5, step =
        0.01, value = 1),
      radioButtons("type", "Weighting function type:", c("euclidean", "maxnorm")
        )
    ),
    column(2,
      plotOutput("plot1", click = "plot_click1", height = h),
      plotOutput("plot2", click = "plot_click2", height = h),
      plotOutput("plot3", click = "plot_click3", height = h),
      tableOutput("info")
    ),
    column(2,
      plotOutput("plot4", click = "plot_click4", height = h),
      plotOutput("plot5", click = "plot_click5", height = h),
      plotOutput("plot6", click = "plot_click6", height = h)
    )
  )
  '

  seqC <- seq_along(C)

  server <- function (deploy = FALSE){
  paste('
  library(condvis)
  library(shiny)
  load("app.Rdata")
  shinyServer(function (input, output)
  {
    ', paste("
    output$plot", seqC, " <- renderPlot({
      i <- ", seqC, "
      if(!is.null(input$plot_click", seqC, "$x)){
          xc.cond[, xcplots[[i]]$name] <<- condvis:::update.xcplot(xcplots[[i]],
            xclick = input$plot_click", seqC, "$x, yclick =
            input$plot_click", seqC, "$y, user = TRUE)$xc.cond.old
      }
      xcplots[[i]] <<- plotxc(xc = data[, C[[i]]], xc.cond = xc.cond[1L, C[[i]]
        ], name = colnames(data[, C[[i]], drop = FALSE]), select.colour =
        select.colour, select.cex = select.cex)
    })"
    , sep = "", collapse = "\n"), '
    output$plotS <- renderPlot({
      if (!is.null(input$plot_click1$x) || !is.null(input$plot_click2$x)){
        xc.cond <<- xc.cond
      }
      vw <<- vwfun(xc.cond = xc.cond, sigma = sigma, distance = distance)
      xsplot <<- condvis:::plotxs1(xs = data[, S, drop = FALSE], data[, response, drop =
        FALSE], xc.cond = xc.cond, model = model, col = col, weights = vw$k,
        view3d = view3d, conf = conf, probs = probs, pch = pch)

    })
    output$info <- renderTable({
      if (!is.null(input$plot_click1$x) || !is.null(input$plot_click2$x)){
        xc.cond <<- xc.cond
      }
      xc.cond <<- xc.cond
    })',
    if (!deploy){'
    observeEvent(input$saveButton, {
      n.selector.cols <- ceiling(length(C) / 4L)
      select.colwidth <- max(min(0.18 * n.selector.cols, 0.45), 0.2)
      width <- 8.5 + 2 * n.selector.cols
      pdf(file = paste0(wd, "/snapshot_", condvis:::timestamp1(), ".pdf"), width
        = width, height = 8)
      condvis:::ceplot.static(data = data, model = model, response = response, S
        = S, C = C, cex.axis = cex.axis, cex.lab = cex.lab, tck = tck, xc.cond =
        xc.cond, weights = vw$k, col  = col, select.colour = select.colour)
      dev.off()
    })
    observeEvent(input$deployButton, {
      app.path <- paste0(tempdir(), "/condvis-shinyapp-deploy")
      dir.create(app.path)
      write(ui, file = paste0(app.path, "/ui.R"))
      write(server(deploy = TRUE), file = paste0(app.path, "/server.R"))
      print(ls())
      save(list = ls(), file = paste0(app.path, "/app.Rdata"))
      rsconnect::deployApp(app.path)
    })'}, '
  })
  ')
  }

  wd <- getwd()
  app.path <- paste0(tempdir(), "/condvis-shinyapp_", timestamp1())
  dir.create(app.path)
  write(ui, file = paste0(app.path, "/ui.R"))
  write(server(), file = paste0(app.path, "/server.R"))
  print(ls())
  save(list = ls(), file = paste0(app.path, "/app.Rdata"))
  shiny::runApp(appDir = app.path)
}
