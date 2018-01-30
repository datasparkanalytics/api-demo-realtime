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
library(htmltools)
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
statuses <- c("Stay", "Pause", "Transit", "Unknown")
shp.meta <- data.frame(
  layer = c("MP14_REGION_WEB_PL", "MP14_PLNG_AREA_WEB_PL", "MP14_SUBZONE_WEB_PL"),
  gov.id = c("REGION_C", "PLN_AREA_C", "SUBZONE_C"),
  gov.name = c("REGION_N", "PLN_AREA_N", "SUBZONE_N"),
  roi.id = c("planningregion", "planningarea", "subzone"),
  roi.name = c("planningregion_name", "planningarea_name", "subzone_name"),
  row.names = c("planning-region", "planning-area", "sub-zone"),
  stringsAsFactors = FALSE
)


#### Default (empty results)
default.footfall <- data.frame(timestamp = as.POSIXct(Sys.time()), roi.id = "N.A.",
                               roi.name = "N.A.", status = "N.A.", count = 0L,
                               stringsAsFactors = FALSE)


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
get.footfall.timestamp <- function(token, interval = 15) {
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
get.footfall <- function(roi.id, roi.layer, token, interval = 15, filters = NA, groups = NA,
                         name = "Footfall") {
  query.body <- list(
    location = list(locationType = "locationHierarchyLevel", levelType = roi.layer, id = roi.id),
    interval = interval,
    filter = filters,
    dimensionFacets = unname(groups),
    aggregations = list(list(metric = "total_stays", type = "longSum"))
  )
  log.query(query.body, name)
  query.response <- POST(rt.ff.api.endpoint, add_headers(Authorization = paste("Bearer", token)),
                         body = query.body, encode = "json")
  warn_for_status(query.response)

  if (query.response$status_code == 200) {
    result <- content(query.response, as = "text") %>%
      as.tbl_json %>%
      gather_array %>%
      spread_values(
        timestamp.string = jstring("timestamp"),
        count = jstring("event", "longSum_total_stays")
      )
    # Additional columns from groups
    spreads.groups <- lapply(groups, function(g) jstring("event", g))
    result <- do.call(function(...) spread_values(result, ...), spreads.groups)
    result %>%
      mutate(
        timestamp = as.POSIXct(timestamp.string, format = "%Y-%m-%dT%H:%M:%S", tz = default.tz),
        count = as.integer(count)
      ) %>%
      select(-timestamp.string)
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

  # Access token
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

  # Latest footfall
  footfall.latest <- reactivePoll(
    timestamp.refresh.ms, session, latest.ts,
    function() {
      r <- roi()@data
      planning.regions <- as.character(unique(r[[ shp.meta["planning-region", "gov.id"] ]]))

      # Filters
      fields <- list()
      if (input$status != "All") {
        # Status filter, if selected
        fields <- list(type = "selector", dimension = "status", value = input$status)
      }
      # Filters, if any
      filters <- if (length(fields)) fields else NA
      # filters <- if (length(fields)) list(type = "and", fields = fields) else NA

      # Run queries
      ff <- planning.regions %>%
        lapply(get.footfall, shp.meta["planning-region", "roi.id"], token(), interval = 15,
               filters = filters,
               groups = c(roi.id = shp.meta[input$layer, "roi.id"],
                          roi.name = shp.meta[input$layer, "roi.name"]),
               name = "Latest Footfall") %>%
        bind_rows()

      # Return latest records
      ff[ff$timestamp == max(ff$timestamp), ]
    })

  # Latest footfall
  footfall.latest.old <- reactive({
    ff <- footfall()
    ff <- ff[ff$timestamp == max(ff$timestamp), ]
    if (input$status == "All") {
      ff %>%
        group_by(roi.id, roi.name) %>%
        summarise(
          timestamp = max(timestamp),
          status = "All",
          count = sum(count)
        ) %>%
        ungroup()
    } else {
      ff[ff$status == input$status, ]
    }
  })

  # Footfall 24h trend
  # footfall.24h <- reactivePoll(
  #   timestamp.refresh.ms, session, latest.ts,
  #   function() {
  #   ff <- footfall()
  #   ff %>%
  #     group_by(status, timestamp) %>%
  #     summarise(
  #       count = sum(count)
  #     ) %>%
  #     ungroup() %>%
  #     arrange(status, timestamp)
  # })

  # Logo
  output$logo <- renderImage(list(src = normalizePath('logo.png'), height = "50px"),
                             deleteFile = FALSE)

  # Base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(103.82, 1.34, 11)
  })

  # ROI shapes
  roi <- reactive({
    r <- get.roishape(input$layer)
    r@data$roi.id <- as.character(r@data[[ shp.meta[input$layer, "gov.id"] ]])
    r@data$roi.name <- as.character(r@data[[ shp.meta[input$layer, "gov.name"] ]])
    r
  })

  # Palette for shading current footfall
  pal <- reactive({
    ff <- footfall.latest()
    if (nrow(ff) <= 1) {
      colorBin("OrRd", 0, bins = 1)
    } else {
      colorBin("OrRd", ff$count, bins = 7)
    }
  })

  # ROI layer with data
  observe({
    r <- roi()
    ff <- footfall.latest()
    r@data <- left_join(r@data, ff, c("roi.id", "roi.name"))
    labels <- sprintf("<strong>%s (%s)</strong><br/>%d",
                      r@data$roi.name, r@data$roi.id, r@data$count) %>%
      lapply(HTML)
    leafletProxy("map", data = r) %>%
      clearGroup("rois") %>%
      addPolygons(weight = 1, color = "royalblue", fillColor = ~pal()(count), fillOpacity = 0.7,
                  group = "rois", layerId = ~roi.id,
                  highlight = highlightOptions(weight = 5, color = "royalblue4", bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal"), direction = "auto")
      )
  })

  # Time series chart
  # output$timeseries.chart <- renderDygraph({
  #   ff <- footfall.24h()
  #
  #   series <- statuses %>%
  #     lapply(function(s) {
  #       f <- ff %>% filter(status == s)
  #       xts(f$count, order.by = f$timestamp, tz = default.tz)
  #     }) %>%
  #     cbind
  #
  #   cat(str(series), file = stderr())
  #
  #   dygraph(series) %>%
  #     dyHighlight(highlightSeriesBackgroundAlpha = 0.3) %>%
  #     dyOptions(useDataTimezone = TRUE)
  # })

}
