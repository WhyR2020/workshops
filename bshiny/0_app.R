library(shiny)
library(ggplot2)

ui <- fluidPage(titlePanel("Hello"),
                sidebarPanel(h3("We can put things here!")),
                mainPanel(plotOutput("example_plot")))

server <- function(input, output){
  
output[["example_plot"]] <- renderPlot({
  
  ggplot(data.frame(x = 1:10, y = 1:10), aes(x, y)) +
    geom_point() +
    labs(title = "our example plot")
  
  
})
}

shinyApp(ui = ui, server = server)