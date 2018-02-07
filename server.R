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
library(reshape2)
library(rgdal)
library(shiny)
library(tidyjson)
library(xts)


# API Credentials --------------------------------------------------------------

# Store credentials in api-credentials.json, with the following format:
# {
#   "key": "YOUR_KEY",
#   "secret": "YOUR_SECRET"
# }


# Parameters -------------------------------------------------------------------

# Realtime Footfall API endpoint
rt.ff.api.endpoint <-
  "https://apistore.datasparkanalytics.com:8243/realtimefootfall/v2/query"
# Token API endpoint
token.api.endpoint <- "https://apistore.datasparkanalytics.com/token"
# Token refresh interval in milliseconds
token.refresh <- 60 * 60 * 1000
# Clock refresh interval in milliseconds
clock.refresh <- 200
# File inputs refresh interval in milliseconds
file.refresh <- 1000
# API latest timestamp refresh interval in milliseconds
timestamp.refresh <- 10 * 1000
# Timestamp of data
default.tz <- "Asia/Singapore"
# ROI and shapefile information
shp.meta <- data.frame(
  # Shapefile layer names
  layer = c("MP14_REGION_WEB_PL", "MP14_PLNG_AREA_WEB_PL",
            "MP14_SUBZONE_WEB_PL"),
  # Field names for ROI ID in Shapefiles
  shp.id = c("REGION_C", "PLN_AREA_C", "SUBZONE_C"),
  # Field names for ROI Name in Shapefiles
  shp.name = c("REGION_N", "PLN_AREA_N", "SUBZONE_N"),
  # Field names for ROI ID in API
  api.id = c("planningregion", "planningarea", "subzone"),
  # Field names for ROI Name in API
  api.name = c("planningregion_name", "planningarea_name", "subzone_name"),
  # Row names for this data frame
  row.names = c("planning-region", "planning-area", "sub-zone"),
  stringsAsFactors = FALSE
)


# Helpers ----------------------------------------------------------------------

# Get ROI data from Shapefiles
get.roishape <- memoise(function(roi.layer) {
  readOGR("shapefiles", shp.meta[roi.layer, "layer"]) %>%
    spTransform(CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
})

# Get latest timestamp from API
get.latest.timestamp <- function(token, period = 1440) {
  query.body <- list(
    location = list(locationType = "locationHierarchyLevel",
                    levelType = "planningarea", id = "DT"),
    period = period,
    aggregations = list(list(metric = "total_count", type = "longSum"))
  )
  resp <- POST(rt.ff.api.endpoint,
               add_headers(Authorization = paste("Bearer", token)),
               body = query.body, encode = "json", verbose())
  warn_for_status(resp, "get latest timestamp")
  if (resp$status_code == 200) {
    result <- content(resp, as = "text") %>%
      as.tbl_json %>%
      gather_array %>%
      spread_values(timestamp.string = jstring("timestamp")) %>%
      mutate(timestamp = as.POSIXct(timestamp.string,
                                    format = "%Y-%m-%dT%H:%M:%S",
                                    tz = default.tz)) %>%
      select(timestamp)
    max(result$timestamp)
  } else {
    as.POSIXct(NA)
  }
}

# Get footfall for the given ROI, with optional filters and groups
get.footfall <- function(roi.id, roi.layer, token, period = 15,
                         filters = NULL, groups = NULL) {
  query.body <- list(
    location = list(locationType = "locationHierarchyLevel",
                    levelType = roi.layer, id = roi.id),
    period = period,
    aggregations = list(list(metric = "unique_agents", type = "hyperUnique"))
  )
  if (!is.null(filters)) query.body[["filter"]] <- filters
  if(!is.null(groups)) query.body[["dimensionFacets"]] <- unname(groups)
  resp <- POST(rt.ff.api.endpoint,
               add_headers(Authorization = paste("Bearer", token)),
               body = query.body, encode = "json", verbose())
  warn_for_status(resp, "get footfall data")
  if (resp$status_code == 200) {
    result <- content(resp, as = "text") %>%
      as.tbl_json %>%
      gather_array %>%
      spread_values(timestamp.string = jstring("timestamp"),
                    count = jnumber("event", "hyperUnique_unique_agents"))
    # Additional columns from groups
    spreads.groups <- lapply(groups, function(g) jstring("event", g))
    result <- do.call(function(...) spread_values(result, ...), spreads.groups)
    # Transform and select columns
    result <- result %>%
      mutate(
        timestamp = as.POSIXct(timestamp.string, format = "%Y-%m-%dT%H:%M:%S",
                               tz = default.tz),
        count = as.integer(count)
      ) %>%
      select(-timestamp.string)
    # Columns returned: timestamp, count, and any specified groups
    result
  } else {
    # Empty data frame
    result <- list(timestamp = as.POSIXct(NA), count = as.integer(NA))
    result <- c(result, lapply(groups, function(x) as.character(NA)))
    as.data.frame(result)
  }
}


# Server Logic -----------------------------------------------------------------

server <- function(input, output, session) {

  # Credentials from api-credentials.json
  creds.json <- reactiveFileReader(file.refresh, session,
                                   "api-credentials.json", read_json)
  # Base64 encoded credential string, used when getting access token
  creds.b64 <- reactive({
    cred <- creds.json() %>%
      spread_values(key = jstring("key"), secret = jstring("secret"))
    key.secret.b64 <- base64_encode(paste(cred$key, cred$secret, sep = ":"))
  })

  # Access token
  token <- reactiveVal()
  # Poll regularly for new access token (when the existing one expires)
  observe({
    invalidateLater(token.refresh, session)
    resp <- POST(token.api.endpoint,
                 add_headers(Authorization = paste("Basic", creds.b64())),
                 body = "grant_type=client_credentials", verbose())
    warn_for_status(resp, "get access token")
    if (resp$status_code == 200) {
      # Override existing value only if the call was successful
      token(content(resp)$access_token)
    }
  })

  # Selected ROI on the map
  roi.selected <- reactiveValues()
  roi.selected$id <- NULL
  roi.selected$name <- NULL
  observeEvent(input$map_shape_click, {
    event <- input$map_shape_click
    if (!is.null(event) && event$group == "rois") {
      roi.selected$id <- event$id
      r <- roi()
      which.roi <-
        r@data[[ shp.meta[input$layer, "shp.id"] ]] == roi.selected$id
      roi.selected$name <-
        r@data[[ shp.meta[input$layer, "shp.name"] ]][ which.roi ]
    }
  })
  observeEvent(input$clear.roi, {
    roi.selected$id <- NULL
    roi.selected$name <- NULL
  })
  output$selected.roi <- renderUI({
    if (is.null(roi.selected$id)) {
      tagList(p("None"))
    } else {
      tagList(
        p(sprintf("%s (%s)", roi.selected$name, roi.selected$id)),
        actionButton("clear.roi", "Clear")
      )
    }
  })

  # Current timer
  output$current.time <- renderText({
    invalidateLater(clock.refresh, session)
    Sys.time() %>%
      as.POSIXlt(tz = default.tz) %>%
      as.POSIXct(tz = default.tz) %>%
      format("%Y-%m-%d %H:%M:%S %Z")
  })

  # Latest timestamp
  latest.ts <- reactiveVal()
  observe({
    invalidateLater(timestamp.refresh, session)
    t <- get.latest.timestamp(token())
    if (!is.na(t)) {
      # Override existing value only if the call was successful
      latest.ts(t)
    }
  })
  output$latest.time <- renderText({
    format(latest.ts(), "%Y-%m-%d %H:%M:%S %Z")
  })

  # Latest footfall
  footfall.latest <- reactivePoll(
    timestamp.refresh, session, latest.ts,
    function() {
      r <- roi()@data
      planning.regions <- r[[ shp.meta["planning-region", "shp.id"] ]] %>%
        unique() %>%
        as.character()

      # Period
      period <-
        as.integer(difftime(Sys.time(), latest.ts(), default.tz, "mins")) + 3L

      # Filters
      if (input$status != "All") {
        # Status filter, if selected
        filters <- list(type = "selector", dimension = "status",
                        value = input$status)
      } else {
        filters <- NULL
      }

      # Run queries
      planning.regions %>%
        lapply(get.footfall, shp.meta["planning-region", "api.id"], token(),
               period = period, filters = filters,
               groups = list(roi.id = shp.meta[input$layer, "api.id"],
                             roi.name = shp.meta[input$layer, "api.name"])
        ) %>%
        bind_rows() %>%
        filter(timestamp == max(timestamp))
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
    r@data$roi.id <- r@data[[ shp.meta[input$layer, "shp.id"] ]] %>%
      as.character()
    r@data$roi.name <- r@data[[ shp.meta[input$layer, "shp.name"] ]] %>%
      as.character()
    r
  })

  # Palette for shading current footfall
  pal <- reactive({
    ff <- footfall.latest()
    if (nrow(ff) <= 1) colorBin("OrRd", 0) else colorBin("OrRd", ff$count)
  })

  # ROI layer with data
  observe({
    r <- roi()
    ff <- footfall.latest()
    r@data <- left_join(r@data, ff, c("roi.id", "roi.name"))
    labels <- sprintf("<strong>%s (%s)</strong><br/>%s: %d",
                      r@data$roi.name, r@data$roi.id, input$status,
                      r@data$count) %>%
      lapply(HTML)
    leafletProxy("map", data = r) %>%
      clearGroup("rois") %>%
      addPolygons(weight = 1, color = "royalblue",
                  fillColor = ~pal()(count),fillOpacity = 0.7,
                  group = "rois", layerId = ~roi.id,
                  highlight = highlightOptions(weight = 5, color = "royalblue4",
                                               bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal"),
                    direction = "auto")
      )
  })

  # Footfall 24h trend
  footfall.24h.cache <- reactiveVal(
    data.frame(status = as.character(NA), timestamp = as.POSIXct(NA),
               count = as.integer(NA))
  )
  footfall.24h.all <- reactivePoll(
    timestamp.refresh, session, latest.ts,
    function() {
      r <- roi()@data
      planning.regions <- r[[shp.meta["planning-region", "shp.id"]]] %>%
        unique() %>%
        as.character()

      # Check if latest data is already available; if not, compute the period of
      # data to be queried
      ff <- footfall.24h.cache()
      ff.max.time <- max(ff$timestamp)
      period <- 0
      if (is.na(ff.max.time)) {
        period <- 1440
      } else if (ff.max.time < latest.ts()) {
        period <- as.integer(difftime(latest.ts(), ff.max.time, "mins")) + 3L
      }

      # Run queries if latest data needs to be retrieved.
      if (period > 0) {
        ff.new <- planning.regions %>%
          lapply(get.footfall, shp.meta["planning-region", "api.id"], token(),
                 period = period, groups = list(status = "status")) %>%
          bind_rows() %>%
          group_by(status, timestamp) %>%
          summarise(count = sum(count)) %>%
          ungroup()
        if (!is.na(ff.max.time)) {
          ff.new <- ff.new %>% filter(timestamp > ff.max.time)
        }
        ff <- bind_rows(ff, ff.new)
      }

      # Remove records more than 24h from the latest timestamp
      ff <- ff %>%
        filter(!is.na(timestamp)) %>%
        filter(timestamp >= max(timestamp) - 24*60*60)
      print(summary(ff))
      footfall.24h.cache(ff)
      ff
    }
  )

  footfall.24h <- reactivePoll(
    timestamp.refresh, session, latest.ts,
    function() {
      r <- roi()@data
      planning.regions <- r[[shp.meta["planning-region", "shp.id"]]] %>%
        unique() %>%
        as.character()
      roi.id <- roi.selected$id

      # Run queries
      if (is.null(roi.id)) {
        # No ROI selected, query over all planning regions
        ff <- footfall.24h.all()
      } else {
        # Query over selected ROI
        ff <- get.footfall(roi.id, shp.meta[input$layer, "api.id"], token(),
                           period = 1440, groups = list(status = "status"))
      }
      df <- ff %>% dcast(timestamp ~ status, value.var = "count", drop = FALSE)
      timestamps <- df$timestamp
      df$timestamp <- NULL
      xts(df, timestamps)
    })

  # Time series chart
  ts.stack <- reactive({ input$timeseries.stacked == "Yes" })
  output$timeseries.chart <- renderDygraph({
    series <- footfall.24h()
    dygraph(series) %>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.5) %>%
      dyOptions(useDataTimezone = TRUE, includeZero = TRUE, labelsKMB = TRUE,
                maxNumberWidth = 10, stackedGraph = ts.stack(),
                fillGraph = ts.stack())
  })

}
