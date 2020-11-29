library(shiny)

restaurants = sort(c("KFC", 'LGM'))

restaurants_list = list()
for(restaurant in restaurants){
  restaurants_list[restaurant] = restaurant
}

ui <- fluidPage(
  headerPanel('Yelp review analysis and suggestions'),
  sidebarPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px; position:relative;",
               radioButtons("radio", label = h3("Brand"),
                            choices = restaurants_list, 
                            selected = 1)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)

server <- function(input, output) {
  
  # output$value <- renderPrint({ input$radio })
  
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}

shinyApp(ui = ui, server = server)