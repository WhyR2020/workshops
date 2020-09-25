library(shiny)
library(ggplot2)

ui <- fluidPage(
  sidebarPanel(h4("Welcome! I'll take care of your file."),
               fileInput(inputId = "user_file",
                         label = "Upload .csv file",
                         accept = c(".csv"))),
  mainPanel(tabsetPanel(tabPanel("Data",
                                 tableOutput("file_data")),
                        tabPanel("Plot")))
)

server <- function(input, output){
  
  output[["file_data"]] <- renderTable({
    
    validate(need(input[["user_file"]], "Please upload a file!"))
    
    read.csv(input[["user_file"]][["datapath"]])
    
  })
}

shinyApp(ui, server)