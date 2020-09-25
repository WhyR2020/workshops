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
  
  rv <- reactiveValues(clicked_point = c())
  
  observeEvent(input[["nz_plot_click"]], {
    
    rv[["clicked_point"]] <- nearPoints(df = dat,
                                        coordinfo = input[["nz_plot_click"]],
                                        maxpoint = 1,
                                        threshold = 10,
                                        allRows = TRUE)
    
  })
  
  output[["nz_info"]] <- renderPrint({
    
    rv[["clicked_point"]] 
    
  })
  
}

shinyApp(ui = ui, server = server)
