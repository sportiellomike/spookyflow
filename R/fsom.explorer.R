#' fsom.explorer
#'
#' @description
#' @param
#' @return
#' @export
#'
#' @examples

fsom.explorer <- function(input.fsom, name.X1, name.Y1, name.X2, name.Y2, name.X3, name.Y3, points, ...) {

  dat <- fsom.dat(input.fsom)
  colnames(dat)[grep("cluster", colnames(dat), ignore.case = T)] <- "cluster"
  colnames(dat)[grep("node|nodes", colnames(dat), ignore.case = T)] <- "node"

  heatmap <- fsom.heatmap(input.fsom, dat.type = "codes", fsom$dims.used, ...)

  shinyApp(
    ui = fluidPage(

      titlePanel("FlowSOM Explorer"),

      sidebarLayout(
        sidebarPanel(

          sliderInput('sampleSize', 'Sample Size', min=0, max=1000000,
                      value=points),

          selectInput('x', 'X.1', names(dat), name.X1),
          selectInput('y', 'Y.1', names(dat), name.Y1),

          selectInput('x2', 'X.2', names(dat), name.X2),
          selectInput('y2', 'Y.2', names(dat), name.Y2),

          selectInput('x3', 'X.3', names(dat), name.X3),
          selectInput('y3', 'Y.3', names(dat), name.Y3),

          selectInput('color', 'Color', c('None', names(dat))),

          if(length(grep("node", colnames(dat), ignore.case = T)) == 1) {
            selectInput('node', 'node', c('None', sort(unique(dat[, grep("node", colnames(dat), ignore.case = T)]))))
          } else {
            selectInput('node', 'node', c('None'))
          },

          if(length(grep("cluster", colnames(dat), ignore.case = T)) == 1) {
            selectInput('cluster', 'cluster', c('None', sort(unique(dat[, grep("cluster", colnames(dat), ignore.case = T)]))))
          } else {
            selectInput('cluster', 'cluster', c('None'))
          }
        ),

        mainPanel(
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), plotOutput('fcs.plot1'), plotOutput('heatmap'))
          ),
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), plotOutput('fcs.plot2'), plotOutput('fcs.plot3'))
          )
        )
      )),
    server = function(input, output) {

      dataset <- reactive({
        dat[sample(nrow(dat), input$sampleSize), ]
      })

      output$fcs.plot1 <- renderPlot({

        p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) +
          geom_point(size = .5) +
          theme(axis.title.x = element_text(size = 18),
                axis.title.y = element_text(size = 18))

        if (input$color != 'None')
          p <- p + aes_string(color=input$color) + scale_color_viridis(option = "plasma")

        if (input$cluster != 'None')
          p <- p + geom_point(data = subset(dataset(), cluster == input$cluster), shape = 21, size = 3, stroke = 1, color = "red")

        if (input$node != 'None')
          p <- p + geom_point(data = subset(dataset(), node == input$node), shape = 21, size = 3, stroke = 1, color = "blue")

        print(p)

      })

      output$fcs.plot2 <- renderPlot({

        p <- ggplot(dataset(), aes_string(x=input$x2, y=input$y2)) +
          geom_point(size = .5) +
          theme(axis.title.x = element_text(size = 18),
                axis.title.y = element_text(size = 18))

        if (input$color != 'None')
          p <- p + aes_string(color=input$color) + scale_color_viridis(option = "plasma")

        if (input$cluster != 'None')
          p <- p + geom_point(data = subset(dataset(), cluster == input$cluster), shape = 21, size = 3, stroke = 1, color = "red")

        if (input$node != 'None')
          p <- p + geom_point(data = subset(dataset(), node == input$node), shape = 21, size = 3, stroke = 1, color = "blue")

        print(p)
      })

      output$heatmap <- renderPlot({heatmap})

      output$fcs.plot3 <- renderPlot({

        p <- ggplot(dataset(), aes_string(x=input$x3, y=input$y3)) +
          geom_point(size = .5) +
          theme(axis.title.x = element_text(size = 18),
                axis.title.y = element_text(size = 18))

        if (input$color != 'None')
          p <- p + aes_string(color=input$color) + scale_color_viridis(option = "plasma")

        if (input$cluster != 'None')
          p <- p + geom_point(data = subset(dataset(), cluster == input$cluster), shape = 21, size = 3, stroke = 1, color = "red")

        if (input$node != 'None')
          p <- p + geom_point(data = subset(dataset(), node == input$node), shape = 21, size = 3, stroke = 1, color = "blue")

        print(p)
      })

    },
    options = list()
  )
}
