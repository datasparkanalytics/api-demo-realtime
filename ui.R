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
library(shinydashboard)

# Define UI for application
ui <- dashboardPage(

  dashboardHeader(title = "Near Real Time Footfall Demo", titleWidth = 400),

  dashboardSidebar(
    p(imageOutput("logo", height = "50px", inline = TRUE), align = "center")
  ),

  dashboardBody(
    # Status Bar
    fluidRow(
      valueBox("Now", subtitle = textOutput("currentTime"),
               icon = icon("clock-o"), width = 6, color = "light-blue"),
      valueBox("Latest", subtitle = "TBD",
               icon = icon("hourglass-2"), width = 6, color = "teal")
    ),

    # Map
    fluidRow(
      box(width = 12,
        leafletOutput("map", height = "350px")
      )
    ),

    # Time Series
    fluidRow(
      box(width = 12,
        dygraphOutput("timeseries.chart", height = "100px")
      )
    )

  ),

  skin = "blue"
)
