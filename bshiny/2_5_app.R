library(shiny)
library(ggplot2)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("darkly"),
  br(),
  sidebarPanel("Let's see the plot!",
               checkboxGroupInput(inputId = "type_choices",
                                  label = "Choices:",
                                  choices = c("Births", "Deaths", "Natural_Increase"),
                                  selected = c("Births", "Deaths", "Natural_Increase")),
               actionButton(inputId = "apply_changes",
                            label = "Filter!"),
               sliderInput(inputId = "x_axis_range",
                           label = "Adjust time range",
                           min = 2000, max = 2019,
                           step = 1, value = c(2000, 2019)),
               sliderInput(inputId = "y_axis_range",
                           label = "Adjust value range",
                           min = 24795, max = 64341, 
                           value = c(24795, 64341))),
  mainPanel(plotOutput("nz_increase_plot"))
)

server <- function(input, output, session){
  
  dat <- reactive({ 
    
    dat_tmp <- read.csv("materials/bd-dec19-births-deaths-natural-increase.csv")
    colnames(dat_tmp) <- c("Year", "Type", "Count")
    dat_tmp
    
  })
  
  dat_tmp <- eventReactive(input[["apply_changes"]], {
    
    validate(need(input[["type_choices"]], "Please select something on the left!"))
    
    dat_tmp <- dat()[dat()[["Type"]] %in% input[["type_choices"]], ]
    
  })
  
  observe({
    
    updateSliderInput(session,
                      inputId = "y_axis_range",
                      min = min(dat_tmp()[["Count"]]),
                      max = max(dat_tmp()[["Count"]]),
                      value = c(min(dat_tmp()[["Count"]]), max(dat_tmp()[["Count"]])))
    
  })
  
  output[["nz_increase_plot"]] <- renderPlot({
    
    ggplot(dat_tmp(), aes(x = Year, y = Count, color = Type)) +
      geom_point() +
      labs(title = "The natural increase in New Zealand",
           x = "Year", 
           y = "Count") +
      coord_cartesian(xlim = c(input[["x_axis_range"]][[1]], input[["x_axis_range"]][[2]] ),
                      ylim = c(input[["y_axis_range"]][[1]], input[["y_axis_range"]][[2]] ))
    
  })
  
}

shinyApp(ui = ui, server = server)