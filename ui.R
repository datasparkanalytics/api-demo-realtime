#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dygraphs)
library(leaflet)
library(shiny)

# Define UI for application
ui <- fluidPage(

  # Header
  fluidRow(
    column(2, imageOutput("logo", height = "50px")),
    column(6, titlePanel("Near Real Time Footfall Demo")),
    column(4, p('Now: ',
                strong(textOutput("currentTime", inline = TRUE))))
  ),

  # Sidebar
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
      leafletOutput("map", height = "450px"),
      dygraphOutput("timeseries.chart", height = "200px")
    )
  )
)
