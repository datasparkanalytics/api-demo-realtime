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

  # App Title
  dashboardHeader(title = "Near Real Time Footfall Demo", titleWidth = 400),

  dashboardSidebar(
    # Logo
    p(img(src = "logo.png", width = "155px", height = "100px"),
      align = "center"),
    # ROI Layer selector
    selectInput("layer", "Layer",
                c("Planning Region" = "planning-region",
                  "Planning Area" = "planning-area",
                  "Sub Zone" = "sub-zone"),
                selected = "sub-zone"),
    # Status selector
    radioButtons("status", "Status",
                 c("All" = "All",
                   "Stay" = "Stay",
                   "Transit" = "Transit",
                   "Pause" = "Pause",
                   "Unknown" = "Unknown"),
                 selected = "All"),
    # Time series display selector
    radioButtons("timeseries.stacked", "Stack Time Series?",
                 c("Yes" = "Yes", "No" = "No"), selected = "No"),
    # Selected ROI indicator
    div(class = "shiny-input-container",
        tag("label", list("Selected ROI", class="control-label")),
        uiOutput("selected.roi", class="shiny-input-container")
        )
  ),

  dashboardBody(
    # Status Bar
    fluidRow(
      # Current time display
      valueBox("Now", subtitle = textOutput("current.time"),
               icon = icon("clock-o"), width = 6, color = "light-blue"),
      # Latest data timestamp display
      valueBox("Latest", subtitle = textOutput("latest.time"),
               icon = icon("hourglass-2"), width = 6, color = "teal")
    ),

    fluidRow(
      # Map
      box(width = 12, leafletOutput("map", height = "350px"))
    ),

    fluidRow(
      # Time series chart
      box(width = 12, dygraphOutput("timeseries.chart", height = "100px"))
    )
  ),
  skin = "blue"
)
