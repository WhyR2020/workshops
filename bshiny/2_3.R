library(shiny)
library(ggplot2)

ui <- fluidPage(
  br(),
  sidebarPanel("Let's see out plot!",
               checkboxGroupInput(inputId = "type_choices",
                                  label = "Choices:",
                                  choices = c("Births", "Deaths", "Natural_Increase"),
                                  selected = c("Births", "Deaths", "Natural_Increase")),
               sliderInput(inputId = "x_axis_range",
                           label = "Adjust time range",
                           min = 2000, max = 2019,
                           step = 1, value = c(2000, 2019))),
  mainPanel(plotOutput("nz_increase_plot"))
)

server <- function(input, output){
  
  dat <- read.csv("materials/bd-dec19-births-deaths-natural-increase.csv")
  colnames(dat) <- c("Year", "Type", "Count")
  
  output[["nz_increase_plot"]] <- renderPlot({
    
    validate(need(input[["type_choices"]], "Please, select something on the left"))
    
    dat_tmp <- dat[dat[["Type"]] %in% input[["type_choices"]] , ]
    
    ggplot(dat_tmp, aes(x = Year, y = Count, color = Type)) +
      geom_point() +
      labs(title = "The natural increase in New Zealand",
           x = "Year", 
           y = "Count") +
      coord_cartesian(xlim = c(input[["x_axis_range"]][[1]], input[["x_axis_range"]][[2]]))
    
  })
  
}

shinyApp(ui = ui, server = server)
