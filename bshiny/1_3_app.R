library(shiny)
library(ggplot2)
library(DT)
library(rmarkdown)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Start",
             includeMarkdown("materials/info2.md")
    ),
    tabPanel("Info",
             br(),
             sidebarPanel(splitLayout(h1("New Zealand"),
                                      img(src = "new-zealand-flag.jpg", height = 100)),
                          h3("New Zealand (Māori: Aotearoa [aɔˈtɛaɾɔa]) is an island country in the southwestern Pacific Ocean. It consists of two main landmasses—the North Island (Te Ika-a-Māui) and the South Island (Te Waipounamu)—and around 600 smaller islands, covering a total area of 268,021 square kilometres (103,500 sq mi). New Zealand is about 2,000 kilometres (1,200 mi) east of Australia across the Tasman Sea and 1,000 kilometres (600 mi) south of the islands of New Caledonia, Fiji, and Tonga. The country's varied topography and sharp mountain peaks, including the Southern Alps, owe much to tectonic uplift and volcanic eruptions. New Zealand's capital city is Wellington, and its most populous city is Auckland.
"),
                          h5("Source: https://en.wikipedia.org/wiki/New_Zealand")),
             mainPanel(plotOutput("nz_increase_plot"),
                       h5("Data source: https://www.stats.govt.nz/large-datasets/csv-files-for-download/"))
             
    ),
    tabPanel("Data",
             dataTableOutput("table_with_data"))
    
  )
  
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
  
  output[["table_with_data"]] <- DT::renderDataTable({
    
    dat
    
  })
  
}

shinyApp(ui = ui, server = server)
