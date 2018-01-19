#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dygraphs)
library(httr)
library(leaflet)
library(memoise)
library(openssl)
library(rgdal)
library(shiny)
library(tidyjson)
library(xts)

#### API Credentials
# Store credentials in api-credentials.json, with the following format:
# {
#   "key": "YOUR_KEY",
#   "secret": "YOUR_SECRET"
# }


#### Parameters
roi.layer <- "sub-zone"
default.tz <- "Asia/Singapore"
shp.meta <- data.frame(
  layer = c("MP14_REGION_WEB_PL", "MP14_PLNG_AREA_WEB_PL", "MP14_SUBZONE_WEB_PL"),
  govId.name = c("REGION_C", "PLN_AREA_C", "SUBZONE_C"),
  row.names = c("planning-region", "planning-area", "sub-zone"),
  stringsAsFactors = FALSE
)


#### Helpers

# Current time with time zone
now.tz <- function(tz = default.tz) {
  as.POSIXct(as.POSIXlt(Sys.time(), tz = tz), tz = tz)
}

# Get ROI data from Shapefiles
get.roishape <- memoise(function(roiLayer) {
  readOGR("shapefiles", shp.meta[roiLayer, "layer"]) %>%
    spTransform(CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
})


#### Server Logic
server <- function(input, output, session) {

  # Credentials
  creds.json <- reactiveFileReader(1000, session, "api-credentials.json", read_json)

  creds.b64 <- reactive({
    cred <- creds.json() %>%
      spread_values(key = jstring("key"), secret = jstring("secret"))
    key.secret.b64 <- base64_encode(paste(cred$key, cred$secret, sep = ":"))
  })

  token <- reactive({
    token.response <- POST("https://apistore.datasparkanalytics.com/token",
                           body = "grant_type=client_credentials",
                           add_headers(Authorization = paste("Basic", creds.b64())))
    warn_for_status(token.response)
    if (token.response$status_code == 200) {
      content(token.response)$access_token
    }
    else {
      ""
    }
  })

  # Logo
  output$logo <- renderImage(list(src = normalizePath('logo.png')),
                             deleteFile = FALSE)

  # Current timer
  output$currentTime <- renderText({
    reactiveTimer(1000)()
    format(now.tz(), "%Y-%m-%d %H:%M:%S %Z")
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

  # Time series chart
  output$timeseries.chart <- renderDygraph({
    # Mock data
    datetimes <- seq.POSIXt(as.POSIXct("2015-01-01", tz=default.tz),
                            as.POSIXct("2015-01-02", tz=default.tz), by="5 min")
    values <- rnorm(length(datetimes))
    series <- xts(values, order.by = datetimes, tz=default.tz)

    dygraph(series) %>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3) %>%
      dyOptions(useDataTimezone = TRUE)
  })

}
