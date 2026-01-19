# app.R - FIXED VERSION
# ------------------------------------------------------------------------------
# MAP LIBERATOR v1.0 - Resize Fix
# ------------------------------------------------------------------------------

source("global.R")
source("R/mod_controls.R")
source("R/mod_map_engine.R")
source("R/mod_sidecar.R")
source("R/mod_workbench.R")
source("R/mod_state_manager.R")

ui <- fluidPage(
  useShinyjs(),
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  # CSS
  tags$head(tags$style(HTML("
    /* 1. General Styles */
    .image-container { 
      width: 100%; height: 85vh; overflow: auto; 
      border: 2px dashed #ccc; background-color: #f9f9f9; 
      display: flex; align-items: center; justify-content: center;
      cursor: grab; 
    }
    .image-container:active { cursor: grabbing; }
    .accordion-button { font-weight: bold; background-color: #f8f9fa; }
    
    /* 2. Map Resizing Animation */
    .map-wrapper {
       position: relative;
       width: 100%;
       transition: height 0.4s ease-in-out;
       border: 1px solid #ddd; /* Adds a clean border */
    }
    
    /* 3. Height States */
    #map_container.height-normal { height: 500px !important; }
    #map_container.height-large  { height: 85vh  !important; }
    
    /* 4. Expand Button */
    .map-expand-btn {
      position: absolute; 
      bottom: 25px;  
      right: 20px;   
      z-index: 1000; 
      opacity: 0.9;
      box-shadow: 0 2px 5px rgba(0,0,0,0.2);
    }
  "))),
  
  titlePanel(div(icon("map-location-dot"), " Map Liberator")),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      state_manager_ui("state"),
      hr(),
      controls_ui("ctrl"), 
      hr(),
      h4("Reference Map"),
      actionButton("toggle_ref", "Show/Hide Image", icon=icon("eye"), class="btn-info btn-sm", width="100%"),
      br(), br(),
      sidecar_controls_ui("sidecar") 
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(id = "main_tabs",
                  
                  # TAB 1: WORKSPACE
                  tabPanel("Workspace", icon = icon("map"),
                           br(),
                           fluidRow(
                             div(id = "map_col", class = "col-sm-12", 
                                 
                                 # Map Container (default: normal height)
                                 div(id = "map_container", class = "map-wrapper height-normal",
                                     
                                     # Expand/Compress Button
                                     actionButton("toggle_size", label = NULL, icon = icon("expand"), 
                                                  class = "btn-sm btn-light map-expand-btn", 
                                                  title = "Toggle Map Size"),
                                     
                                     # Map Module
                                     map_engine_ui("map", height = "100%")
                                 )
                             ),
                             
                             # Sidecar Image Panel (hidden by default)
                             shinyjs::hidden(
                               div(id = "img_col", class = "col-sm-6", 
                                   sidecar_panel_ui("sidecar") 
                               )
                             )
                           ),
                           hr(),
                           h4(icon("table"), " Data Ledger"),
                           workbench_ui("workbench")
                  ),
                  
                  # TAB 2: TUTORIAL
                  tabPanel("Tutorial", icon = icon("graduation-cap"),
                           div(style = "height: 85vh; width: 100%;",
                               tags$iframe(src = "tutorial.html", 
                                           width = "100%", height = "100%", style = "border:none;")
                           )
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # --- MODULE SETUP ---
  controls_out <- controls_server("ctrl")
  map_out <- map_engine_server("map", controls_output = controls_out)
  
  # State Management Bridge
  bridge_data <- reactiveVal(data.frame())
  state_out <- state_manager_server("state", data_to_save = bridge_data)
  
  # Workbench
  wb_out <- workbench_server("workbench", 
                             map_source = map_out, 
                             controls_output = controls_out,
                             loaded_state = state_out)
  
  observe({ req(wb_out()); bridge_data(wb_out()) })
  
  # Sidecar
  sidecar_server("sidecar")
  
  # --- MAP RESIZE LOGIC (NON-DESTRUCTIVE) ---
  observeEvent(input$toggle_size, {
    # 1. Toggle CSS classes on the container
    toggleClass(id = "map_container", class = "height-normal")
    toggleClass(id = "map_container", class = "height-large")
    
    # 2. Update button icon
    if (input$toggle_size %% 2 == 1) {
      updateActionButton(session, "toggle_size", icon = icon("compress"))
    } else {
      updateActionButton(session, "toggle_size", icon = icon("expand"))
    }
    
    # 3. Force Leaflet Resize
    runjs("
      setTimeout(function() {
        // Find ANY element with class 'leaflet-container' inside the map_col
        var $map = $('#map_col').find('.leaflet-container');
        
        if ($map.length > 0) {
           var mapInstance = HTMLWidgets.getInstance($map[0]);
           if (mapInstance) {
             var leafletObj = mapInstance.getMap();
             leafletObj.invalidateSize();
             console.log('Map resized via generic selector!');
           }
        }
      }, 350);
    ")
  })
  
  # --- SIDECAR IMAGE TOGGLE ---
  observeEvent(input$toggle_ref, {
    toggle("img_col")
    toggleClass("map_col", "col-sm-6")
    toggleClass("map_col", "col-sm-12")
    
    # Also trigger Leaflet resize when toggling image panel
    runjs("
      setTimeout(function() {
        var mapElement = document.getElementById('map-map');
        if (mapElement) {
          var leafletMap = HTMLWidgets.find(mapElement);
          if (leafletMap && leafletMap.getMap) {
            leafletMap.getMap().invalidateSize();
          }
        }
      }, 250);
    ")
  })
}

shinyApp(ui, server)