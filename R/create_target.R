#' Freehand drawing
#'
#' Opens up a dialogue that lets you draw your data.
#'
#' @param data Optional `data.frame` with x and y values that can used as
#' background to guide your drawing.
#'
#' @return
#' A `data.frame` with the x and y values of your data.
#'
#' @export
#' @importFrom graphics par points plot
draw_data <- function(data = NULL) {
  shiny.available <- requireNamespace("shiny", quietly = TRUE)
  minuUI.available <- requireNamespace("miniUI", quietly = TRUE)

  if (!shiny.available & !minuUI.available) {
    stop("draw_data needs packages 'shiny' and 'miniUI'. ",
         "Install them with 'install.packages(c(\"shiny\", \"miniUI\"))'")
  }

  if (!shiny.available) {
    stop("draw_data needs package'shiny'. ",
         "Install it with 'install.packages(\"shiny\")'")
  }

  if (!minuUI.available) {
    stop("draw_data needs package 'miniUI'. ",
         "Install it with 'install.packages(\"miniUI\")'")
  }

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Draw your target figure"),
    miniUI::miniContentPanel(
      shiny::plotOutput("plot", width = "100%", height = "90%",
                 hover = shiny::hoverOpts(id = "hover", delay = 100,
                                   delayType = "throttle", clip = TRUE, nullOutside = TRUE),
                 click = "click",
                 dblclick = "dblclick"),
      miniUI::miniButtonBlock(
        shiny::actionButton("reset_prev", "Clear last group"),
        shiny::actionButton("reset_all", "Clear all"),
        border = "bottom"
      )
    )
  )

  server <- function(input, output, session) {
    target = shiny::reactiveValues(x = NULL, y = NULL, group = NULL)
    drawing = shiny::reactiveVal(FALSE)
    current_group <- shiny::reactiveVal(1)

    shiny::observeEvent(input$dblclick, handlerExpr = {
      if (drawing()) {
        this_group <- current_group()
        # target$group <- c(target$group, rep.int(this_group, n))
        next_group <- this_group + 1
        current_group(next_group)
      }
      temp <- drawing()
      drawing(!temp)
    })

    shiny::observeEvent(input$click, handlerExpr = {
      target$x <- c(target$x, input$click$x)
      target$y <- c(target$y, input$click$y)
      target$group <- c(target$group, current_group())

      this_group <- current_group()
      # target$group <- c(target$group, rep.int(this_group, n))
      next_group <- this_group + 1
      current_group(next_group)
    })

    shiny::observeEvent(input$hover, {
      if (drawing()) {
        target$x <- c(target$x, input$hover$x)
        target$y <- c(target$y, input$hover$y)
        target$group <- c(target$group, current_group())

      }})

    shiny::observeEvent(input$reset_all, handlerExpr = {
      target$x <- NULL
      target$y <- NULL
      target$group <- NULL
      current_group(1)
    })

    shiny::observeEvent(input$reset_prev, handlerExpr = {
      if (length(target$x) > 0) {
        this_group <- current_group()
        prev_group <- this_group - 1
        keep <- target$group != (prev_group)
        target$x <- target$x[keep]
        target$y <- target$y[keep]
        target$group <- target$group[keep]
        current_group(prev_group)
      }
    })

    output$plot <- shiny::renderPlot({
      par(mar = c(1,1,1,1))
      if (!is.null(data)) {
        plot(x = data$x, y = data$y, ylab="", xlab="", pch = 20, cex = 0.7,
             main = "Single click to add points, double click to draw lines")
      } else {
        plot(1, type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 1),
             main = "Single click to add points, double click to draw lines")
      }

      points(target$x, target$y, pch = 19, col = "red")
    })

    shiny::observeEvent(input$done, {
      returnValue <- data.frame(x = target$x,
                                y = target$y)
      shiny::stopApp(returnValue)
    })

    # observeEvent(input$cancel, {
    #   stopApp(invisible(NULL))
    # })
  }

  shiny::runGadget(ui, server, stopOnCancel = TRUE, viewer = shiny::dialogViewer(""))
}
