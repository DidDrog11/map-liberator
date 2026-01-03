# R/mod_map_engine.R
map_engine_ui <- function(id) {
  ns <- NS(id)
  withSpinner(leafletOutput(ns("map"), height = "85vh"), type = 6)
}

map_engine_server <- function(id, controls_output) {
  moduleServer(id, function(input, output, session) {
    
    selected_ids <- reactiveVal(character(0))
    
    output$map <- renderLeaflet({
      leaflet() |> 
        addProviderTiles(providers$CartoDB.Positron) |> 
        addMapPane("context_pane", zIndex = 390) |>
        addMapPane("geom_pane", zIndex = 400)
    })
    
    # 1. DRAW LAYERS
    observeEvent(controls_output$geom_trigger(), {
      target <- controls_output$geom_data()
      context <- controls_output$context_data()
      
      # Handle Clear
      if (is.null(target)) {
        leafletProxy("map") |> clearGroup("Interaction") |> clearGroup("Adm0") |> clearGroup("Adm1") |> clearGroup("Adm2") |> removeLayersControl()
        selected_ids(character(0))
        return()
      }
      
      selected_ids(character(0))
      
      proxy <- leafletProxy("map") |> 
        clearGroup("Interaction") |> 
        clearGroup("Adm0") |> 
        clearGroup("Adm1") |> 
        clearGroup("Adm2")
      
      # Draw Context Layers
      # Adm0: Thick Black
      if (!is.null(context$Adm0)) {
        proxy |> addPolygons(data = context$Adm0, group = "Adm0", fill = FALSE, color = "black", weight = 3, opacity = 1, options = pathOptions(pane = "context_pane"))
      }
      # Adm1: Medium Grey
      if (!is.null(context$Adm1)) {
        proxy |> addPolygons(data = context$Adm1, group = "Adm1", fill = FALSE, color = "#444", weight = 2, opacity = 1, options = pathOptions(pane = "context_pane"))
      }
      # Adm2: Thin Grey (Only if we have it)
      if (!is.null(context$Adm2)) {
        proxy |> addPolygons(data = context$Adm2, group = "Adm2", fill = FALSE, color = "#777", weight = 1, opacity = 1, options = pathOptions(pane = "context_pane"))
      }
      
      # Draw Interactive Target
      proxy |> addPolygons(
        data = target, layerId = ~layerId,
        fillColor = "white", fillOpacity = 0.1, 
        color = "#000", weight = 0.5, opacity = 1,
        label = ~Tooltip, group = "Interaction",
        options = pathOptions(pane = "geom_pane"),
        highlightOptions = highlightOptions(color = "red", weight = 2, bringToFront = TRUE)
      )
      
      # Toggle Controls
      groups <- c("Interaction")
      if (!is.null(context$Adm0)) groups <- c(groups, "Adm0")
      if (!is.null(context$Adm1)) groups <- c(groups, "Adm1")
      if (!is.null(context$Adm2)) groups <- c(groups, "Adm2")
      
      proxy |> addLayersControl(overlayGroups = groups, options = layersControlOptions(collapsed = FALSE))
    })
    
    # 2. FIX ZOOM BUG
    observeEvent(controls_output$zoom_trigger(), {
      trigger <- controls_output$zoom_trigger()
      req(trigger > 0, controls_output$geom_data())
      
      data <- controls_output$geom_data()
      bb <- st_bbox(data)
      
      # Coordinates
      lng1 <- as.numeric(bb["xmin"]); lat1 <- as.numeric(bb["ymin"])
      lng2 <- as.numeric(bb["xmax"]); lat2 <- as.numeric(bb["ymax"])
      
      message(paste("Zooming to:", lat1, lng1, " - ", lat2, lng2))
      
      # FIX: Use correct Selector ID
      # session$ns("map") resolves to "modulename-map" which matches the leaflet ID
      map_id <- session$ns("map")
      
      runjs(sprintf("
        var map = HTMLWidgets.find('#%s').getMap();
        map.invalidateSize();
        map.fitBounds([[%s, %s], [%s, %s]]);
      ", map_id, lat1, lng1, lat2, lng2))
    })
    
    # 3. CLICK LOGIC
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      if (is.null(click$id) || is.null(controls_output$geom_data())) return()
      
      # Only process clicks on "Interaction" layer
      # Check if clicked ID exists in target data
      target <- controls_output$geom_data()
      if (!click$id %in% target$layerId) return()
      
      curr <- selected_ids()
      if (click$id %in% curr) {
        selected_ids(setdiff(curr, click$id)); col <- "white"; op <- 0.1
      } else {
        selected_ids(c(curr, click$id)); col <- "red"; op <- 0.6
      }
      
      poly <- target[target$layerId == click$id, ]
      
      leafletProxy("map") |> 
        addPolygons(
          data = poly, layerId = click$id,
          fillColor = col, fillOpacity = op, 
          color = "black", weight = 0.5, opacity = 1,
          label = poly$Tooltip, options = pathOptions(pane = "geom_pane"), group = "Interaction"
        )
    })
    
    observeEvent(controls_output$clear_trigger(), { selected_ids(character(0)) })
    
    return(list(current_selection = selected_ids, current_data = controls_output$geom_data))
  })
}