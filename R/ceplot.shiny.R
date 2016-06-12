ceplot.shiny <-
function (data, model, response = NULL, S = NULL, C = NULL, sigma = NULL,
  distance = "euclidean", cex.axis = NULL, cex.lab = NULL, tck = NULL, view3d =
  FALSE, Corder = "default", conf = FALSE, separate = TRUE, select.colour =
  "blue", select.cex = 1, select.lwd = 2, select.type = "minimal", probs = FALSE
  , col = "black", pch = 1, residuals = FALSE, xc.cond = NULL, packages = NULL)
{
  ## First check for shiny package, and stop if not installed

  if(!requireNamespace("shiny", quietly = TRUE))
    stop("requires package 'shiny'")
  else if (!exists("runApp")) attachNamespace("shiny")

  ## Set up the initial section

  xc.cond <- if (is.null(xc.cond))
    data[1, !colnames(data) %in% c(S, response)]
  else xc.cond
  #data.frame(lapply(data[, !colnames(data) %in% c(S, response)], mode1))
  uniqC <- unique(unlist(C))

  ## Set some variables and the visualweight function

  xcplots <- list()
  plotlegend <- length(S) == 2
  n.selector.cols <- ceiling(length(C) / 4L)
  selector.colwidth <- 2
  height <- 8
  col <- rep(col, length.out = nrow(data))
  plotS3d <- identical(length(S), 2L)
  seqC <- seq_along(C)
  wd <- getwd()

  vwfun <- .visualweight(xc = data[, uniqC, drop = FALSE])

  ## These are the packages required to call predict on all models in 'model'
  ## If not supplied, all packages attached to the search path at the time of
  ## calling ceplot are recorded.

  packages <- if (is.null(packages))
    rev(gsub("package:", "", grep("package:", search(), value = TRUE)))
  else packages

  ## Function to create the shiny ui.R file. 'deploy' switch removes certain
  ## elements when deploying the application.

  ui <- function (deploy = FALSE)
  {
  paste0('
  ## This ui.R file was created by condvis:::ceplot.shiny

  library(shiny)
  load("app.Rdata")
  h <- "200px"
  hS <- "400px"
  basicPage(
    column(4,
      if (plotS3d) {
        tabsetPanel(
          tabPanel("Contour", plotOutput("plotS", height = hS, width = hS
            ), value = 1),
          tabPanel("Perspective", plotOutput("plotS2", height = hS, width =
            hS), value = 2),
          id = "tab"
        )
      } else plotOutput("plotS", height = hS, width = hS),
      conditionalPanel(condition = "input.tab == 2", numericInput("phi",
        "Vertical rotation: ", 20, -180, 180)),
      conditionalPanel(condition = "input.tab == 2", numericInput("theta",
        "Horizontal rotation: ", 45, -180, 180)),
      sliderInput("sigma", "Weighting function parameter: ", 0.01, 5, step =
        0.01, value = if (is.null(sigma)) 1 else sigma),
      radioButtons("type", "Weighting function type:", c("euclidean", "maxnorm")
        ),',
      if (deploy) '#', 'actionButton("deployButton", "Deploy app to web"),
      downloadButton("download", "Download snapshot (pdf)")
    ),
    column(8,
      column(3,
        plotOutput("plot1", click = "plot_click1", height = h),
        plotOutput("plot2", click = "plot_click2", height = h),
        plotOutput("plot3", click = "plot_click3", height = h),
        tableOutput("info")
      ),
      column(3,
        plotOutput("plot4", click = "plot_click4", height = h),
        plotOutput("plot5", click = "plot_click5", height = h),
        plotOutput("plot6", click = "plot_click6", height = h)
      )
    )
  )
  ')
  }

  ## Function to create the shiny server.R file. 'deploy' switch removes certain
  ## elements when deploying the application.

  server <- function (deploy = FALSE){
  paste('
  ## This server.R file was created by condvis:::ceplot.shiny

  library(condvis)
  library(shiny)\n',
  paste(paste0("library(", packages, ")"), collapse = "\n")
  ,'
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
    })
    output$download <- downloadHandler(filename = function() { paste0(
      "condvis-download-", condvis:::timestamp1(), ".pdf")}, {
      function(file){
        n.selector.cols <- ceiling(length(C) / 4L)
        select.colwidth <- max(min(0.18 * n.selector.cols, 0.45), 0.2)
        width <- 8.5 + 2 * n.selector.cols
        pdf(file = file, width = width, height = 8)
        condvis:::ceplot.static(data = data, model = model, response = response,
          S = S, C = C, cex.axis = cex.axis, cex.lab = cex.lab, tck = tck,
          xc.cond = xc.cond, weights = vw$k, col  = col, select.colour =
          select.colour, conf = conf)
        dev.off()
      }
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
        xc.cond, weights = vw$k, col  = col, select.colour = select.colour, conf
        = conf)
      dev.off()
    })
    observeEvent(input$deployButton, {
      deploy.path <- paste0(wd, "/condvis-shinyapp-deploy")
      dir.create(deploy.path, showWarnings = FALSE)
      write(ui(deploy = TRUE), file = paste0(deploy.path, "/ui.R"))
      write(server(deploy = TRUE), file = paste0(deploy.path, "/server.R"))
      file.copy(from = paste0(app.path, "/app.Rdata"), to = paste0(deploy.path,
        "/app.Rdata"), overwrite = TRUE)
      if (!requireNamespace("rsconnect", quietly = TRUE))
        stop("requires package \'rsconnect\'")
      else if (!exists("deployApp")) attachNamespace("rsconnect")
      rsconnect::deployApp(deploy.path)
    })'}, '
  })
  ')
  }

  ## Create a temporary directory to store the application files (including a
  ## snapshot of the objects in this environment in "app.Rdata") and run the
  ## application

  app.path <- paste0(tempdir(), "/condvis-shinyapp-temp")
  dir.create(app.path, showWarnings = FALSE)
  write(ui(), file = paste0(app.path, "/ui.R"))
  write(server(), file = paste0(app.path, "/server.R"))
  save(list = ls(), file = paste0(app.path, "/app.Rdata"))
  shiny::runApp(appDir = app.path)
}
