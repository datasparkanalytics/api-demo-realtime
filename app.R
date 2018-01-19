#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  fluidRow(
    column(2, imageOutput("logo", height = "50px")),
    column(6, titlePanel("Near Real Time Footfall Demo")),
    column(4, p('Now: ',
                strong(textOutput("currentTime", inline = TRUE))))
  ),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("map", height = "400px")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$logo <- renderImage(list(src = normalizePath('logo.png')),
                             deleteFile = FALSE)

  output$currentTime <- renderText({
    reactiveTimer(1000)()
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(103.82, 1.34, 11)
  })

}

# Run the application
shinyApp(ui = ui, server = server)

