library(shiny)
library(ggplot2)

ui <- fluidPage(
  br(),
  sidebarPanel(verbatimTextOutput("nz_info")),
  mainPanel(plotOutput("nz_increase_plot", click = "nz_plot_click"))
)

server <- function(input, output){
  
  dat <- read.csv("materials/bd-dec19-births-deaths-natural-increase.csv")
  colnames(dat) <- c("Year", "Type", "Count")
  
  output[["nz_increase_plot"]] <- renderPlot(
    
    ggplot(dat, aes(x = Year, y = Count, color = Type)) +
      geom_point() +
      labs(title = "The natural increase in New Zealand",
           x = "Year", 
           y = "Count")
    
  )
  
  output[["nz_info"]] <- renderPrint({
    
    input[["nz_plot_click"]]
    
  })
  
}

shinyApp(ui = ui, server = server)
