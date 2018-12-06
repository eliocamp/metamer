library(shiny)
library(miniUI)

myGadgetFunc <- function(data = data.frame(x = runif(20), y = runif(20)),
                         target = NULL) {

  ui <- miniPage(
    gadgetTitleBar("Draw your target figure"),
    miniContentPanel(
      # actionButton("reset", "reset"),
      plotOutput("plot", width = "100%", height = "100%",
                 hover = hoverOpts(id = "hover", delay = 100,
                                   delayType = "throttle", clip = TRUE, nullOutside = TRUE),
                 click = "click"),
      miniButtonBlock(
        actionButton("reset_prev", "Clear last group"),
        actionButton("reset_all", "Clear all")
      )
      # Define layout, inputs, outputs
    )
  )

  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.
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

        n <- length(target$x) - length(target$group)
        print(n)
        print(this_group)

        target$group <- c(target$group, rep.int(this_group, n))
        next_group <- this_group + 1
        current_group(next_group)

      }
      temp <- drawing()
      drawing(!temp)

    })

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

    observeEvent(input$hover, {
      if (drawing()) {
        target$x <- c(target$x, input$hover$x)
        target$y <- c(target$y, input$hover$y)
      }})

    output$plot <- renderPlot({
      plot(x = data$x, y = data$y, ylab="y", xlab="x")
      points(x = target$x, y = target$y)
    })

    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      returnValue <- data.frame(x = target$x,
                                y = target$y,
                                group = target$group)
      stopApp(returnValue)
    })
    observeEvent(input$cancel, {
      stopApp(invisible(NULL))
    })
  }

  runGadget(ui, server, stopOnCancel = FALSE)
}
