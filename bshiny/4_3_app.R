library(shiny)
library(ggplot2)

ui <- fluidPage(
  br(),
  sidebarPanel(verbatimTextOutput("nz_info")),
  mainPanel(plotOutput("nz_increase_plot", click = "nz_plot_click"))
)

server <- function(input, output){
  
  
  dat <- reactive({
    
    tmp <- read.csv("materials/bd-dec19-births-deaths-natural-increase.csv")
    colnames(tmp) <- c("Year", "Type", "Count") 
    tmp
    
  })
  
  output[["nz_increase_plot"]] <- renderPlot({
    
    dat_tmp <- dat()
    dat_tmp[["selected"]] <- FALSE
    if(length(rv[["clicked_points"]]) > 0)
      dat_tmp[which(rv[["clicked_points"]][["selected_"]]), "selected"] <- TRUE
    
    ggplot(dat_tmp, aes(x = Year, y = Count, color = Type)) +
      geom_point(aes(size = selected)) +
      labs(title = "The natural increase in New Zealand",
           x = "Year", 
           y = "Count") 
    
  })
  
  output[["nz_info"]] <- renderPrint({
    
    tmp <- rv[["clicked_points"]][ rv[["clicked_points"]][["selected_"]] , ]
    
    if(is.null(tmp)){
      "Please, click on some point!"
    } else {
      paste0("In year ", tmp[["Year"]], " there was ", tmp[["Count"]], " cases of ", tmp[["Type"]], " in New Zealand.")
    }
    
    
  })
  
  rv <- reactiveValues(clicked_points = c())
  
  observeEvent(input[["nz_plot_click"]], {
    
    rv[["clicked_points"]] <- nearPoints(dat(),
                                         coordinfo = input[["nz_plot_click"]],
                                         maxpoints = 1,
                                         threshold = 10,
                                         allRows = TRUE)
    
  })
  
}


shinyApp(ui = ui, server = server)
