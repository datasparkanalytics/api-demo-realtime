#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(leaflet)
library(shiny)

# Define server logic
server <- function(input, output) {

  # Logo
  output$logo <- renderImage(list(src = normalizePath('logo.png')),
                             deleteFile = FALSE)

  # Current timer
  output$currentTime <- renderText({
    reactiveTimer(1000)()
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  })

  # Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(103.82, 1.34, 11)
  })

}
