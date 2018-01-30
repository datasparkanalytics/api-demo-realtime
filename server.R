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
statuses <- c(Stay = "Stay", Pause = "Pause", Transit = "Transit", Unknown = "Unknown")
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
  query.response <- POST(rt.ff.api.endpoint, add_headers(Authorization = paste("Bearer", token)),
                         body = query.body, encode = "json", verbose())
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
get.footfall <- function(roi.id, roi.layer, token, interval = 15, filters = NULL, groups = NULL) {
  query.body <- list(
    location = list(locationType = "locationHierarchyLevel", levelType = roi.layer, id = roi.id),
    interval = interval,
    aggregations = list(list(metric = "total_stays", type = "longSum"))
  )
  if (!is.null(filters)) query.body[["filter"]] <- filters
  if(!is.null(groups)) query.body[["dimensionFacets"]] <- unname(groups)
  query.response <- POST(rt.ff.api.endpoint, add_headers(Authorization = paste("Bearer", token)),
                         body = query.body, encode = "json", verbose())
  warn_for_status(query.response)

  if (query.response$status_code == 200) {
    result <- content(query.response, as = "text") %>%
      as.tbl_json %>%
      gather_array %>%
      spread_values(
        timestamp.string = jstring("timestamp"),
        count = jnumber("event", "longSum_total_stays")
      )
    # Additional columns from groups
    spreads.groups <- lapply(groups, function(g) jstring("event", g))
    result <- do.call(function(...) spread_values(result, ...), spreads.groups)
    result <- result %>%
      mutate(
        timestamp = as.POSIXct(timestamp.string, format = "%Y-%m-%dT%H:%M:%S", tz = default.tz),
        count = as.integer(count)
      ) %>%
      select(-timestamp.string)
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

  # Access token
  token <- reactive({
    token.response <- POST(token.api.endpoint,
                           add_headers(Authorization = paste("Basic", creds.b64())),
                           body = "grant_type=client_credentials", verbose())
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
      filters <- if (length(fields)) fields else NULL
      # filters <- if (length(fields)) list(type = "and", fields = fields) else NULL

      # Run queries
      ff <- planning.regions %>%
        lapply(get.footfall, shp.meta["planning-region", "roi.id"], token(), interval = 15,
               filters = filters,
               groups = list(roi.id = shp.meta[input$layer, "roi.id"],
                             roi.name = shp.meta[input$layer, "roi.name"])
               ) %>%
        bind_rows()

      # Return latest records
      ff[ff$timestamp == max(ff$timestamp), ]
    })

  # Logo
  output$logo <- renderImage(list(src = normalizePath('logo.png'), height = "70px"),
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
      colorBin("OrRd", 0)
    } else {
      colorBin("OrRd", ff$count)
    }
  })

  # ROI layer with data
  observe({
    r <- roi()
    ff <- footfall.latest()
    r@data <- left_join(r@data, ff, c("roi.id", "roi.name"))
    labels <- sprintf("<strong>%s (%s)</strong><br/>%s: %d",
                      r@data$roi.name, r@data$roi.id, input$status, r@data$count) %>%
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

  # Selected ROI on the map
  roi.selected <- reactiveValues()
  roi.selected$id <- NULL
  roi.selected$name <- NULL

  observeEvent(input$map_shape_click, {
    event <- input$map_shape_click
    if (! is.null(event) && event$group == "rois") {
      roi.selected$id <- event$id
      r <- roi()
      which.roi <- r@data[[ shp.meta[input$layer, "gov.id"] ]] == roi.selected$id
      roi.selected$name <- r@data[[ shp.meta[input$layer, "gov.name"] ]][ which.roi ]
    }
  })

  observeEvent(input$clear.roi, {
    roi.selected$id <- NULL
    roi.selected$name <- NULL
  })

  output$selected.roi <- renderText({
    if (!is.null(roi.selected$id)) {
      sprintf("<p><strong>Selected ROI</strong></p><p>%s (%s)</p>", roi.selected$name, roi.selected$id)
    } else {
      "<p><strong>Selected ROI</strong></p><p>None</p>"
    }
  })

  # Footfall 24h trend
  footfall.24h.all <- reactivePoll(
    timestamp.refresh.ms, session, latest.ts,
    function() {
      r <- roi()@data
      planning.regions <- as.character(unique(r[[ shp.meta["planning-region", "gov.id"] ]]))

      # Run queries
      ff <- planning.regions %>%
        lapply(get.footfall, shp.meta["planning-region", "roi.id"], token(), interval = 1440,
               groups = list(status = "status")) %>%
        bind_rows() %>%
        group_by(status, timestamp) %>%
        summarise(count = sum(count)) %>%
        ungroup()
    }
  )

  footfall.24h <- reactivePoll(
    timestamp.refresh.ms, session, latest.ts,
    function() {
      r <- roi()@data
      planning.regions <- as.character(unique(r[[ shp.meta["planning-region", "gov.id"] ]]))
      roi.id <- roi.selected$id

      # Run queries
      if (is.null(roi.id)) {
        # No ROI selected, query over all planning regions
        ff <- footfall.24h.all()
      } else {
        # Query over selected ROI
        ff <- get.footfall(roi.id, shp.meta[input$layer, "roi.id"], token(), interval = 1440,
                           groups = list(status = "status"))
      }
      df <- ff %>% dcast(timestamp ~ status, value.var = "count", drop = FALSE)
      timestamps <- df$timestamp
      df$timestamp <- NULL
      xts(df, timestamps)
    })

  # Time series chart
  ts.stack <- reactive({ if (input$timeseries.stacked == "Yes") TRUE else FALSE })

  output$timeseries.chart <- renderDygraph({
    series <- footfall.24h()
    dygraph(series) %>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.5) %>%
      dyOptions(useDataTimezone = TRUE, includeZero = TRUE, labelsKMB = TRUE, maxNumberWidth = 10,
                stackedGraph = ts.stack(), fillGraph = ts.stack())
  })

}
