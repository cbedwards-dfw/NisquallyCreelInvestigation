# Load necessary packages
library(shiny)
library(leaflet)
library(leaflet.extras)
library(here)
library(dplyr)
library(readxl)
library(sf)
library(tidyverse)
library(rclipboard)
library(htmltools)

# Sample marker data (with labels)
# marker_data <- data.frame(
#   id = 1:5,
#   name = c("A", "B", "C", "D", "E"),
#   lat = c(37.77, 37.76, 37.75, 37.74, 37.73),
#   lng = c(-122.42, -122.43, -122.44, -122.45, -122.46)
# )
# 
# ## Quick note: we use the same coordinate system as leaflet's default: EPPSG:3857, or 900913, or WGS84. But what is sf's default

marker_data <- read_csv(here("cleaned_data/test-fishing-catch.csv")) |> 
  rename(lng = longitude_decimal_degrees_wgs84,
         lat = latitude_decimal_degrees_wgs84) |> 
  filter(!is.na(lng),
         !is.na(lat)) |> 
  select(lng, lat, survey_datetime, survey_id, catch_area_code, catch_result_type_code, species = common_name, adipose_clip_status_code, individual_fish_id, target_species)


  
# Convert to sf object for spatial intersection
marker_sf <- st_as_sf(marker_data, coords = c("lng", "lat"), crs = "WGS84")

# Shiny app
ui <- fluidPage(
  rclipboardSetup(),  # Initializes clipboard functionality
  leafletOutput("map", height = "950px"),
  textOutput("fishSelected"),
  uiOutput("clip_ui"),
)

server <- function(input, output, session) {
  
  # Render Leaflet map with drawing toolbar
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() |> 
      setView(lng = -123,
              lat = 47.7,
              zoom = 8) |> 
      # addMarkers(data = marker_data, lng = ~lng, lat = ~lat) %>%
      addDrawToolbar(
        targetGroup = "drawn",
        polygonOptions = drawPolygonOptions(showArea = TRUE),
        rectangleOptions = drawRectangleOptions(),
        markerOptions = FALSE,
        editOptions = editToolbarOptions(edit = FALSE, remove = TRUE)
      )
  })
  
  # Observe drawing events and calculate intersecting markers
  observeEvent(input$map_draw_all_features, {
    # feature <- input$map_draw_new_feature
    
    features <- input$map_draw_all_features$features
    
    if (length(features) == 0) return()
    
    drawn_polygons <- lapply(features, function(feature) {
      if (feature$geometry$type == "Polygon") {
        coords <- feature$geometry$coordinates[[1]]
        lng <- sapply(coords, `[[`, 1)
        lat <- sapply(coords, `[[`, 2)
        st_polygon(list(cbind(lng, lat)))
      } else {
        NULL
      }
    })
    
    # Filter out any NULLs and create an sf object
    drawn_polygons <- drawn_polygons[!sapply(drawn_polygons, is.null)]
    if (length(drawn_polygons) == 0) return()
    
    # Combine all drawn polygons into a single MULTIPOLYGON
    drawn_sf <- st_sfc(drawn_polygons, crs = "WGS84")
    
    # Find markers within any of the drawn polygons
    selected <- marker_sf[st_within(marker_sf, drawn_sf, sparse = FALSE) |> apply(1, any), ]
    selected_df = as.data.frame(selected) |> 
      select(survey_id, survey_datetime, catch_area_code, catch_result_type_code, species, adipose_clip_status_code,
             individual_fish_id, target_species)

 
    output$download_markers <- downloadHandler(
      filename = function() {
        paste("selected_testfishing_observations-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(selected_df, file, row.names = FALSE)
      }
    )
    
    output$fishSelected <- renderText({
      # temp = 0
      # if(exists(selected_df)){
      #   temp = nrow(selected_df)
      # }
      paste("Total of ", nrow(selected_df), " fish selected.")
    })
    
    output$clip_ui <- renderUI({
      if (nrow(selected_df) > 0) {
        tagList(
          downloadButton("download_markers", "Download as CSV", class = "btn-primary")
        )
      }
    })
    # }
  })
}

shinyApp(ui, server)