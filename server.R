#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
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
rt.ff.api.endpoint <- "https://apistore.datasparkanalytics.com:8243/realtimefootfall/v2/query"
token.api.endpoint <- "https://apistore.datasparkanalytics.com/token"
clock.refresh.ms <- 200
file.refresh.ms <- 1000
timestamp.refresh.ms <- 10000
default.tz <- "Asia/Singapore"
shp.meta <- data.frame(
  layer = c("MP14_REGION_WEB_PL", "MP14_PLNG_AREA_WEB_PL", "MP14_SUBZONE_WEB_PL"),
  govId.name = c("REGION_C", "PLN_AREA_C", "SUBZONE_C"),
  govName.name = c("REGION_N", "PLN_AREA_N", "SUBZONE_N"),
  levelType.name = c("planningregion", "planningarea", "subzone"),
  row.names = c("planning-region", "planning-area", "sub-zone"),
  stringsAsFactors = FALSE
)


#### Default (empty results)
default.footfall <- data.frame(timestamp = as.POSIXct(Sys.time()), roi = "N.A.", status = "N.A.",
                               count = 0L)


#### Helpers

# Log API query to stderr
log.query <- function(query.body, name = "Unknown", file = stderr()) {
  cat(as.character(as.POSIXlt(Sys.time(), tz = default.tz)), name, "query:\n", file = file)
  capture.output(str(query.body), file = file)
}

# Get ROI data from Shapefiles
get.roishape <- memoise(function(roi.layer) {
  readOGR("shapefiles", shp.meta[roi.layer, "layer"]) %>%
    spTransform(CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
})

# Get latest timestamp from API
get.footfall.timestamp <- function(token, interval = 10) {
  query.body <- list(
    location = list(locationType = "locationHierarchyLevel", levelType = "planningregion",
                    id = "CR"),
    interval = interval,
    aggregations = list(list(metric = "total_stays", type = "longSum"))
  )
  log.query(query.body, "Footfall timestamp")
  query.response <- POST(rt.ff.api.endpoint, add_headers(Authorization = paste("Bearer", token)),
                         body = query.body, encode = "json")
  warn_for_status(query.response)
  if (query.response$status_code == 200) {
    result <- content(query.response, as = "text") %>%
      as.tbl_json %>%
      gather_array %>%
      spread_values(timestamp.string = jstring("timestamp")) %>%
      mutate(timestamp = as.POSIXct(timestamp.string, format = "%Y-%m-%dT%H:%M:%S",
                                    tz = default.tz)) %>%
      select(timestamp)
    max(result$timestamp)
  } else {
    0
  }
}

# Get footfall for the given Planning Region
get.footfall <- function(planning.region, roi.layer, token, interval = 10) {
  query.body <- list(
    location = list(locationType = "locationHierarchyLevel", levelType = "planningregion",
                    id = planning.region),
    interval = interval,
    dimensionFacets = c("status", shp.meta[roi.layer, "levelType.name"]),
    aggregations = list(list(metric = "total_stays", type = "longSum"))
  )
  log.query(query.body, "Footfall")
  query.response <- POST(rt.ff.api.endpoint, add_headers(Authorization = paste("Bearer", token)),
                         body = query.body, encode = "json")
  warn_for_status(query.response)
  if (query.response$status_code == 200) {
    result <- content(query.response, as = "text") %>%
      as.tbl_json %>%
      gather_array %>%
      spread_values(
        timestamp.string = jstring("timestamp"),
        roi = jstring("event", shp.meta[roi.layer, "levelType.name"]),
        status = jstring("event", "status"),
        count = jstring("event", "longSum_total_stays")
      ) %>%
      mutate(
        timestamp = as.POSIXct(timestamp.string, format = "%Y-%m-%dT%H:%M:%S", tz = default.tz),
        count = as.integer(count)
      ) %>%
      select(timestamp, roi, status, count)
    result
  } else {
    default.footfall
  }
}


#### Server Logic
server <- function(input, output, session) {

  # Credentials
  creds.json <- reactiveFileReader(file.refresh.ms, session, "api-credentials.json", read_json)
  creds.b64 <- reactive({
    cred <- creds.json() %>%
      spread_values(key = jstring("key"), secret = jstring("secret"))
    key.secret.b64 <- base64_encode(paste(cred$key, cred$secret, sep = ":"))
  })

  token <- reactive({
    token.response <- POST(token.api.endpoint,
                           add_headers(Authorization = paste("Basic", creds.b64())),
                           body = "grant_type=client_credentials")
    warn_for_status(token.response)
    if (token.response$status_code == 200) {
      content(token.response)$access_token
    }
    else {
      ""
    }
  })

  # Logo
  output$logo <- renderImage(list(src = normalizePath('logo.png'), height = "50px"),
                             deleteFile = FALSE)

  # Current timer
  output$currentTime <- renderText({
    invalidateLater(clock.refresh.ms, session)
    now <- as.POSIXct(as.POSIXlt(Sys.time(), tz = default.tz), tz = default.tz)
    format(now, "%Y-%m-%d %H:%M:%S %Z")
  })

  # Latest timestamp
  latest.ts <- reactive({
    invalidateLater(timestamp.refresh.ms, session)
    get.footfall.timestamp(token())
  })
  output$latestTime <- renderText({
    format(latest.ts(), "%Y-%m-%d %H:%M:%S %Z")
  })

  # Base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(103.82, 1.34, 11)
  })

  # ROI shapes
  roi <- reactive({
    r <- get.roishape(input$layer)
    r@data$roi <- r@data[[ shp.meta[input$layer, "govId.name"] ]]
    r@data$roi.name <- r@data[[ shp.meta[input$layer, "govName.name"] ]]
    r
  })

  # Current Footfall data
  current.footfall <- reactivePoll(
    timestamp.refresh.ms, session, latest.ts,
    function() {
      r <- roi()@data
      planning.regions <- as.character(unique(r[[ shp.meta["planning-region", "govId.name"] ]]))
      ff <- bind_rows(lapply(planning.regions, get.footfall, input$layer, token()))
      ff[ff$timestamp == max(ff$timestamp), ]
    })

  # Footfall map data
  current.footfall.map <- reactive({
    ff <- current.footfall()
    if (input$status == "All") {
      ff %>%
        group_by(roi) %>%
        summarise(
          timestamp = max(timestamp),
          status = "All",
          count = sum(count)
        )
    } else {
      ff[ff$status == input$status, ]
    }
  })

  # Palette for shading current footfall
  palette <- reactive({
    ff <- current.footfall.map()
    if (nrow(ff) <= 1) {
      colorNumeric("OrRd", 0)
    } else {
      colorNumeric("OrRd", ff$count)
    }
  })

  output$table <- renderTable({
    current.footfall.map()
  })

  # ROI layer with data
  observe({
    r <- roi()
    ff <- current.footfall.map()
    r@data <- left_join(r@data, ff, "roi")
    leafletProxy("map", data = r) %>%
      clearGroup("rois") %>%
      addPolygons(weight = 1, fillColor = ~palette()(count), fillOpacity = 0.7, group = "rois",
                  layerId = ~roi)
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
