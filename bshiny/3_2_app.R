# 1. Load a file and show it 
# 2. Add some options for the plot

library(shiny)
library(ggplot2)

ui <- fluidPage(
  sidebarPanel(h4("Welcome! I'll take care of your file."),
               fileInput(inputId = "user_file",
                         label = "Upload .csv file",
                         accept = c(".csv")),
               h4("Plot settings"),
               selectInput(inputId = "data_x",
                           label = "Choose x:",
                           choices = c("x", "y")),
               selectInput(inputId = "data_y",
                           label = "Choose y:",
                           choices = c("y", "x")),
               actionButton(inputId = "generate_plot",
                            label = "Plot it!")
  ),
  mainPanel(tabsetPanel(tabPanel("Plot",
                                 plotOutput("file_data_plot")),
                        tabPanel("Data",
                                 tableOutput("file_data"))
  ))
)

server <- function(input, output, session){
  
  dat <- reactive({
    
    validate(need(input[["user_file"]], "Please upload a file!"))
    
    read.csv(input[["user_file"]][["datapath"]])
    
    
  })
  
  output[["file_data"]] <- renderTable({
    
    dat()
    
  })
  
  observe({
    
    dat_cols <- names(dat())
    
    updateSelectInput(session,
                      inputId = "data_x",
                      choices = dat_cols,
                      selected = dat_cols[[1]])
    
    updateSelectInput(session,
                      inputId = "data_y",
                      choices = dat_cols,
                      selected = dat_cols[[2]])
  })
  
  plot_out <- eventReactive(input[["generate_plot"]], {
    
    ggplot(dat(), aes(x = dat()[[input[["data_x"]]]], y = dat()[[input[["data_y"]]]])) +
      geom_point() + 
      labs(x = input[["data_x"]],
           y = input[["data_y"]])
    
  })
  
  output[["file_data_plot"]] <- renderPlot({
    
    plot_out()
    
  })
}

shinyApp(ui, server)