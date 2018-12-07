

draw_data <- function(data = NULL,
                         target = NULL) {
  library(shiny)
  library(miniUI)
  ui <- miniPage(
    gadgetTitleBar("Draw your target figure"),
    miniContentPanel(
      plotOutput("plot", width = "100%", height = "100%",
                 hover = hoverOpts(id = "hover", delay = 100,
                                   delayType = "throttle", clip = TRUE, nullOutside = TRUE),
                 click = "click"),
      miniButtonBlock(
        actionButton("reset_prev", "Clear last group"),
        actionButton("reset_all", "Clear all")
      )
    )
  )

  server <- function(input, output, session) {
    if (is.null(target)) {
      target = reactiveValues(x = NULL, y = NULL, group = NULL)
    } else {
      target = reactiveValues(x = target$x, y = target$y, group = 1)
    }

    drawing = reactiveVal(FALSE)
    current_group <- reactiveVal(1)

    observeEvent(input$click, handlerExpr = {
      if (drawing()) {
        this_group <- current_group()
        # target$group <- c(target$group, rep.int(this_group, n))
        next_group <- this_group + 1
        current_group(next_group)
      }
      temp <- drawing()
      drawing(!temp)
    })

    observeEvent(input$hover, {
      if (drawing()) {
        target$x <- c(target$x, input$hover$x)
        target$y <- c(target$y, input$hover$y)
        target$group <- c(target$group, current_group())

      }})

    observeEvent(input$reset_all, handlerExpr = {
      target$x <- NULL
      target$y <- NULL
      target$group <- NULL
      current_group(1)
    })

    observeEvent(input$reset_prev, handlerExpr = {
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

    output$plot <- renderPlot({
      if (!is.null(data)) {
        plot(x = data$x, y = data$y, ylab="y", xlab="x")
      } else {
        plot(1, type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 1))
      }

      points(target$x, target$y)
    })

    observeEvent(input$done, {
      returnValue <- data.frame(x = target$x,
                                y = target$y)
      stopApp(returnValue)
    })

    observeEvent(input$cancel, {
      stopApp(invisible(NULL))
    })
  }

  runGadget(ui, server, stopOnCancel = FALSE)
}
