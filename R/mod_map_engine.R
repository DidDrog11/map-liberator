# R/mod_map_engine.R
# ------------------------------------------------------------------------------
# MODULE: Map Engine (Robust & Stateful)
# ------------------------------------------------------------------------------

# 1. UI
map_engine_ui <- function(id, height = "100%") {
  ns <- NS(id)
  
  # Pure container, no spinner wrapper (handled by app.R CSS)
  div(style = "width: 100%; height: 100%; position: relative;",
      leafletOutput(ns("map"), width = "100%", height = "100%"),
      
      div(style="position:absolute; bottom:25px; left:20px; background:rgba(255,255,255,0.8); padding:4px 8px; border-radius:4px; font-size:11px; z-index:1000; pointer-events:none;",
          textOutput(ns("perf_msg"))
      )
  )
}

# 2. SERVER
map_engine_server <- function(id, controls_output) {
  moduleServer(id, function(input, output, session) {
    
    selected_ids <- reactiveVal(character(0))
    last_render_time <- reactiveVal(NA)
    data_drawn <- reactiveVal(FALSE)
    
    # --- HELPER: DRAWING LOGIC (Visual Hierarchy) ---
    draw_geometry <- function(map_obj, target, context) {
      
      # 1. Context Layers (Background)
      # STRATEGY: Higher levels = Thicker, Darker lines.
      
      # Admin 0 (National): Thickest
      if (!is.null(context$Adm0)) {
        map_obj <- map_obj |> addPolygons(
          data = context$Adm0, group = "Reference Borders", 
          options = pathOptions(pane = "context_pane", clickable = FALSE), 
          fill = FALSE, color = "#000000", weight = 3.0, opacity = 1.0
        )
      }
      
      # Admin 1 (State): Medium
      if (!is.null(context$Adm1)) {
        map_obj <- map_obj |> addPolygons(
          data = context$Adm1, group = "Reference Borders", 
          options = pathOptions(pane = "context_pane", clickable = FALSE), 
          fill = FALSE, color = "#444444", weight = 2.0, opacity = 0.9
        )
      }
      
      # Admin 2 (District): Thin
      is_adm2_target <- "GID_2" %in% names(target)
      if (!is.null(context$Adm2) && !is_adm2_target) {
        map_obj <- map_obj |> addPolygons(
          data = context$Adm2, group = "Reference Borders", 
          options = pathOptions(pane = "context_pane", clickable = FALSE), 
          fill = FALSE, color = "#888888", weight = 1.0, opacity = 0.6
        )
      }
      
      # 2. Target Layer (Interactive)
      # If target is deep (e.g. Admin 3), make lines very thin to avoid "blackout"
      if (!is.null(target)) {
        
        # Adaptive Weight: If we have >1000 polygons, make them super thin
        n_poly <- nrow(target)
        target_weight <- if (n_poly > 1000) 0.5 else if (n_poly > 500) 0.8 else 1.2
        
        map_obj <- map_obj |> addPolygons(
          data = target,
          layerId = ~layerId,
          fillColor = "white", fillOpacity = 0.05, 
          color = "#2c3e50", weight = target_weight, opacity = 0.8,
          label = ~Tooltip,
          options = pathOptions(pane = "geom_pane"),
          highlightOptions = highlightOptions(color = "#e74c3c", weight = 3, bringToFront = TRUE, fillOpacity = 0.2),
          group = "Target Layer"
        )
      }
      return(map_obj)
    }
    
    # --- A. INITIAL RENDER ---
    output$map <- renderLeaflet({
      m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        setView(lng = 0, lat = 20, zoom = 3) |>
        addMapPane("context_pane", zIndex = 390) |>
        addMapPane("geom_pane", zIndex = 400)
      
      target_now  <- isolate(controls_output$geom_data())
      context_now <- isolate(controls_output$context_data())
      
      if (!is.null(target_now)) {
        m <- draw_geometry(m, target_now, context_now)
        bbox <- sf::st_bbox(target_now)
        m <- m |> fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
        data_drawn(TRUE)
      }
      
      return(m)
    })
    
    # --- B. UPDATE TRIGGER ---
    observeEvent(controls_output$geom_trigger(), {
      start_time <- Sys.time()
      target  <- controls_output$geom_data()
      context <- controls_output$context_data()
      
      selected_ids(character(0))
      
      p <- leafletProxy("map") |> clearShapes() |> clearControls()
      
      if (!is.null(target)) {
        p <- draw_geometry(p, target, context)
        
        groups <- c("Target Layer", "CTX")
        p <- p |> addLayersControl(overlayGroups = groups, options = layersControlOptions(collapsed = FALSE))
        
        bbox <- sf::st_bbox(target)
        p |> fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
        
        render_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        last_render_time(render_time)
        data_drawn(TRUE)
      } else {
        data_drawn(FALSE)
      }
    })
    
    # --- C. INTERACTION ---
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      target <- controls_output$geom_data()
      if (is.null(click$id) || is.null(target)) return()
      if (!click$id %in% target$layerId) return()
      
      curr <- selected_ids()
      if (click$id %in% curr) {
        selected_ids(setdiff(curr, click$id))
        col <- "#2c3e50"; fill <- "white"; op <- 0.05; w <- 0.8 # Reset to thin
      } else {
        selected_ids(c(curr, click$id))
        col <- "#e74c3c"; fill <- "#e74c3c"; op <- 0.5; w <- 3.0 # Select Thick
      }
      
      leafletProxy("map") |> 
        addPolygons(
          data = target[target$layerId == click$id, ], 
          layerId = click$id, 
          fillColor = fill, fillOpacity = op, 
          color = col, weight = w, opacity = 1.0, 
          label = ~Tooltip, options = pathOptions(pane = "geom_pane"),
          group = "Target Layer"
        )
    })
    
    observeEvent(controls_output$clear_trigger(), { selected_ids(character(0)) })
    
    # --- D. PERFORMANCE DISPLAY ---
    output$perf_msg <- renderText({
      if (!data_drawn()) return("")
      rt <- last_render_time()
      n <- nrow(isolate(controls_output$geom_data()))
      if (is.na(rt) || is.null(n)) return("")
      paste0(n, " polys | ", round(rt, 2), "s")
    })
    
    return(list(click = reactive({ input$map_shape_click }), selected = selected_ids))
  })
}