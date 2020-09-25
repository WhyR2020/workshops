library(shiny)
library(ggplot2)

ui <- fluidPage(
  br(),
  sidebarPanel("Let's see the plot!"),
  mainPanel(plotOutput("nz_increase_plot"))
)

server <- function(input, output){
  
  dat <- read.csv("materials/bd-dec19-births-deaths-natural-increase.csv")
  colnames(dat) <- c("Year", "Type", "Count")
  
  output[["nz_increase_plot"]] <- renderPlot({
    
    ggplot(dat, aes(x = Year, y = Count, color = Type)) +
      geom_point() +
      labs(title = "The natural increase in New Zealand",
           x = "Year", 
           y = "Count")
    
  })
  
}

shinyApp(ui = ui, server = server)