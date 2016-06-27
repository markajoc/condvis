ceplot.shiny <-
function (data, model, response = NULL, S = NULL, C = NULL, sigma = NULL, lambda
  = NULL, distance = "euclidean", cex.axis = NULL, cex.lab = NULL, tck = NULL,
  view3d = FALSE, Corder = "default", conf = FALSE, separate = TRUE,
  select.colour = "blue", select.cex = 1, select.lwd = 2, select.type =
  "minimal", probs = FALSE, col = "black", pch = 1, residuals = FALSE, xc.cond =
  NULL, packages = NULL)
{
  ## Check for shiny package, and stop if not installed

  if(!requireNamespace("shiny", quietly = TRUE))
    stop("requires package 'shiny'")
  else if (!exists("runApp")) attachNamespace("shiny")

  ## Set up the initial section

  xc.cond <- if (is.null(xc.cond))
    data[1, !colnames(data) %in% c(S, response)]
  else xc.cond
  #data.frame(lapply(data[, !colnames(data) %in% c(S, response)], mode1))

  ## Set some variables and the similarityweight function

  uniqC <- unique(unlist(C))
  xcplots <- list()
  plotlegend <- length(S) == 2
  n.selector.cols <- ceiling(length(C) / 4L)
  selector.colwidth <- 2
  height <- 8
  col <- rep(col, length.out = nrow(data))
  need3d <- identical(length(S), 2L) && all(vapply(data[, S, drop = FALSE],
    is.numeric, logical(1L))) && is.numeric(data[, response[1]])
  seqC <- seq_along(C)
  lenC <- length(C)
  wd <- getwd()

  vwfun <- .similarityweight(xc = data[, uniqC, drop = FALSE])

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
  h <- "170px"
  hS <- "350px"
  basicPage(
    column(4,
      if (need3d) {
        tabsetPanel(
          tabPanel("Contour",
            plotOutput("plotS", height = hS, width = hS),
            value = 1),
          tabPanel("Perspective",
            plotOutput("plotS3D", height = hS, width = hS),
            value = 2),
          id = "tab"
        )
      } else plotOutput("plotS", height = hS, width = hS),
      conditionalPanel(condition = "input.tab == 2",
        numericInput("phi", "Vertical rotation: ", 20, -180, 180),
        numericInput("theta", "Horizontal rotation: ", 45, -180, 180)),
      sliderInput("threshold", "Distance threshold: ", 0.01, 5, step =
        0.01, value = if (is.null(sigma)) 1 else sigma),
      radioButtons("distance", "Distance function type:", c("euclidean",
        "maxnorm")),
      hr(),
      downloadButton("download", "Download snapshot (pdf)")',
      if (!deploy) ',\n      actionButton("openDeploy", "Deploy app"),
      br(),
      conditionalPanel(condition = "input.openDeploy",
      radioButtons("deployLocation", "", c("to web via rsconnect",
        "to working directory")),
        textInput("appName", label = "Application name (valid directory name)"),
        actionButton("deployButton", "Deploy app")
      )','
    ),
    ', if (identical(length(S), 2L)) 'column(1,
      plotOutput("legend", height = hS, width = "100px")
    ),','
    column(7,
      fluidRow(helpText("Condition selectors")),
      column(4,
        plotOutput("plot1", click = "plot_click1", height = h, width = h),
        plotOutput("plot2", click = "plot_click2", height = h, width = h),
        plotOutput("plot3", click = "plot_click3", height = h, width = h),
        tableOutput("info")
      )
      ', if (lenC > 3){',column(4,
        plotOutput("plot4", click = "plot_click4", height = h, width = h),
        plotOutput("plot5", click = "plot_click5", height = h, width = h),
        plotOutput("plot6", click = "plot_click6", height = h, width = h)
      )'},
      if (lenC > 6){',column(4,
        plotOutput("plot7", click = "plot_click4", height = h, width = h),
        plotOutput("plot8", click = "plot_click5", height = h, width = h),
        plotOutput("plot9", click = "plot_click6", height = h, width = h)
      )'}, '
    )
  )
  ')
  }

  ## Function to create the shiny server.R file. 'deploy' switch removes certain
  ## elements when deploying the application.

  server <- function (deploy = FALSE){
  paste(
'  ## This server.R file was created by condvis:::ceplot.shiny

  library(condvis)
  library(shiny)

  ## Include the packages required for the application. If these have not been
  ## specified, the package list will be inferred from the search path at the
  ## time ceplot was called.
  \n ',
  paste(paste0("library(", packages, ")"), collapse = "\n  ")
  ,'

  ## Load the objects that were in the environment of the ceplot call.

  load("app.Rdata")

  ## Shiny server

  shinyServer(function (input, output)
  {
    ## Reactive value for the current condition/section

    rv <- reactiveValues(xc.cond = xc.cond)

    ## Event listeners for mouseclicks on plots
    ',
    paste('
    observeEvent({input$plot_click', seqC,'}, {
      rv$xc.cond[, xcplots[[', seqC,']]$name] <<- condvis:::update.xcplot(
        xcplots[[', seqC,']], xclick = input$plot_click', seqC,'$x, yclick =
        input$plot_click', seqC,'$y, user = TRUE, draw = FALSE)$xc.cond.old
    })
    ', sep = '', collapse = '\n'),
    '
    ## Do condition selector plots.
    ',
    paste("
    output$plot", seqC, " <- renderPlot({
      i <- ", seqC, "
      xcplots[[i]] <<- plotxc(xc = data[, C[[i]]], xc.cond = rv$xc.cond[1L,
        C[[i]]], name = colnames(data[, C[[i]], drop = FALSE]), select.colour =
        select.colour, select.cex = select.cex)
    })"
    , sep = "", collapse = "\n"), '

    ## Next do the section visualisation.

    vw <- NULL
    output$plotS <- renderPlot({
      vw <<- vwfun(xc.cond = rv$xc.cond, sigma = input$threshold, distance =
        input$distance, lambda = lambda)
      xsplot <<- condvis:::plotxs(xs = data[, S, drop = FALSE], data[, response
        , drop = FALSE], xc.cond = rv$xc.cond, model = model, col = col, weights
        = vw$k, view3d = FALSE, conf = conf, probs = probs, pch = pch)
    })

    ## Section visualisation for 3-D perspective mesh.

    output$plotS3D <- renderPlot({
      vw <<- vwfun(xc.cond = rv$xc.cond, sigma = input$threshold, distance =
        input$distance, lambda = lambda)
      xsplot <<- condvis:::plotxs(xs = data[, S, drop = FALSE], data[, response
        , drop = FALSE], xc.cond = rv$xc.cond, model = model, col = col,
        weights = vw$k, view3d = TRUE, conf = conf, probs = probs, pch = pch)
    })

    ## Legend for section

    output$legend <- renderPlot({
      condvis:::xslegend(y = data[, response[1]], name = response[1])
    })

    ## Give a basic table showing the section/condition values

    output$info <- renderTable({
      structure(rv$xc.cond, row.names = "section")
    })

    ## Allow the user to download a snapshot of the current visualisation

    output$download <- downloadHandler(filename = function() { paste0(
      "condvis-download-", condvis:::timestamp1(), ".pdf")}, {
      function(file){
        n.selector.cols <- ceiling(length(C) / 4L)
        select.colwidth <- max(min(0.18 * n.selector.cols, 0.45), 0.2)
        width <- 8.5 + 2 * n.selector.cols
        pdf(file = file, width = width, height = 8)
        condvis:::ceplot.static(data = data, model = model, response = response,
          S = S, C = C, cex.axis = cex.axis, cex.lab = cex.lab, tck = tck,
          xc.cond = rv$xc.cond, weights = vw$k, col  = col, select.colour =
          select.colour, select.cex = select.cex, conf = conf, probs = probs)
        dev.off()
      }
    })',
    if (!deploy){'

    ## Code after here relates to deploying the current application, and will
    ## not be present in a deployed application.

    observeEvent(input$deployButton, {
      folderpath <- if (input$deployLocation == "to working directory")
        wd
      else tempdir()
      appname <- if (input$appName == "")
        "condvis-shinyapp"
      else input$appName
      deploy.path <- paste0(folderpath, "/", appname)
      dir.create(deploy.path, showWarnings = FALSE)
      write(ui(deploy = TRUE), file = paste0(deploy.path, "/ui.R"))
      write(server(deploy = TRUE), file = paste0(deploy.path, "/server.R"))
      file.copy(from = paste0(app.path, "/app.Rdata"), to = paste0(deploy.path,
        "/app.Rdata"), overwrite = TRUE)
      if (input$deployLocation == "to web via rsconnect"){
        if (!requireNamespace("rsconnect", quietly = TRUE))
          stop("requires package \'rsconnect\'")
        else if (!exists("deployApp")) attachNamespace("rsconnect")
        rsconnect::deployApp(deploy.path)
      }
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
