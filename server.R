#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(leaflet)
library(memoise)
library(rgdal)
library(shiny)


#### Parameters
roi.layer <- "sub-zone"
shp.meta <- data.frame(
  layer = c("MP14_REGION_WEB_PL", "MP14_PLNG_AREA_WEB_PL", "MP14_SUBZONE_WEB_PL"),
  govId.name = c("REGION_C", "PLN_AREA_C", "SUBZONE_C"),
  row.names = c("planning-region", "planning-area", "sub-zone"),
  stringsAsFactors = FALSE
)


#### Helpers

# Get ROI data from Shapefiles
get.roishape <- memoise(function(roiLayer) {
  readOGR("shapefiles", shp.meta[roiLayer, "layer"]) %>%
    spTransform(CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
})


#### Server Logic
server <- function(input, output) {

  # Logo
  output$logo <- renderImage(list(src = normalizePath('logo.png')),
                             deleteFile = FALSE)

  # Current timer
  output$currentTime <- renderText({
    reactiveTimer(1000)()
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  })

  # Base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(103.82, 1.34, 11)
  })

  # ROI shapes
  rois <- reactive({ get.roishape(roi.layer) })

  # ROI layer with data
  observe({
    rois <- rois()
    leafletProxy("map", data = rois) %>%
      clearGroup("rois") %>%
      addPolygons(weight = 1, fillOpacity = 0.7, group = "rois")
  })

}
