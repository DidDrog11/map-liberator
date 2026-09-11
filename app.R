# app.R - FIXED VERSION
# ------------------------------------------------------------------------------
# MAP LIBERATOR v1.0 - Resize Fix
# ------------------------------------------------------------------------------

source("global.R")
source("R/mod_schema.R")
source("R/mod_controls.R")
source("R/mod_map_engine.R")
source("R/mod_region_list.R")
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
      width: 100%; height: 50vh; min-height: 320px; overflow: auto; 
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
    #map_container.height-normal { height: 50vh; min-height: 320px; }
    #map_container.height-large  { height: 85vh; }
    
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
      h4("Source Document"),
      actionButton("toggle_ref", "Show/Hide Source", icon=icon("eye"), class="btn-info btn-sm", width="100%"),
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

                                 # Map or list: two ways to pick a region that
                                 # feed the same workbench. The list suits a
                                 # tabular source, where the name is known and
                                 # locating the polygon is a wasted step.
                                 div(style = "display: flex; align-items: center; gap: 12px; margin-bottom: 4px;",
                                     radioButtons("view_mode", NULL, inline = TRUE,
                                                  choices = c("Map" = "map", "Region list" = "list"),
                                                  selected = "map"),
                                     hint_label("", title = "Map or list",
                                                tags$p("Both views pick regions for the same ledger."),
                                                tags$p(tags$b("Map:"), " click polygons. Best when the source is itself a map."),
                                                tags$p(class = "mb-0", tags$b("Region list:"), " click a name, with a search box. ",
                                                       "Best when the source is a table. The Entered column shows how many ",
                                                       "values each region already has for the current source document."))
                                 ),

                                 # Region list (hidden until chosen)
                                 shinyjs::hidden(
                                   # Same height as the sidecar's PDF frame, so the
                                   # source and the region rows sit side by side and
                                   # the list scrolls within its own frame.
                                   div(id = "list_container",
                                       style = "height: 75vh; min-height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 6px;",
                                       region_list_ui("regions"))
                                 ),

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
  # Two reactiveVal bridges break circular dependencies between modules that
  # each both produce and consume project state:
  #   bridge_data      workbench produces the ledger; state manager saves it
  #   restored_schema  state manager loads a schema; controls restores it
  # Neither module can be constructed before the other, so the value passes
  # through a plain reactiveVal owned here instead.
  restored_schema <- reactiveVal(NULL)
  bridge_data     <- reactiveVal(data.frame())
  
  # When a document that already has ledger rows is loaded, the metadata
  # panel is refilled from those rows, so later additions match earlier ones.
  document_meta <- reactiveVal(NULL)

  controls_out <- controls_server("ctrl", restore_schema = restored_schema,
                                  restore_metadata = document_meta)
  map_out <- map_engine_server("map", controls_output = controls_out)
  
  # State Management. The schema travels with the ledger so a multi-session
  # extraction resumes against identical variable definitions.
  state_out <- state_manager_server(
    "state",
    data_to_save   = bridge_data,
    schema_to_save = reactive(list(
      schema_text  = controls_out$schema_text(),
      rules_text   = controls_out$rules_text(),
      project_name = controls_out$metadata()$project,
      blank_zero   = controls_out$blank_zero()
    ))
  )
  
  observe({ req(state_out$schema()); restored_schema(state_out$schema()) })
  
  # Sidecar. Initialised before the workbench because the workbench stamps
  # each committed row with the reference image currently on screen.
  sidecar_out <- sidecar_server("sidecar")

  observeEvent(sidecar_out()$file_name, {
    m <- metadata_from_rows(bridge_data(), sidecar_out()$file_name)
    if (!is.null(m)) document_meta(m)
  }, ignoreInit = TRUE)
  
  # Region list: the tabular alternative to the map. It reads the ledger (via
  # bridge_data) and the current image so it can mark regions already entered.
  list_out <- region_list_server("regions",
                                 geom_data  = controls_out$geom_data,
                                 entry_mode = controls_out$entry_mode,
                                 ledger     = bridge_data,
                                 image      = sidecar_out)

  # The workbench sees one region source. Clicks are forwarded as events from
  # whichever view produced them, so switching views never replays an old
  # click; the batch selection is simply that of the view on screen.
  last_click <- reactiveVal(NULL)
  observeEvent(map_out$click(),  { last_click(map_out$click()) })
  observeEvent(list_out$click(), { last_click(list_out$click()) })
  region_source <- list(
    click    = last_click,
    selected = reactive({
      if (identical(input$view_mode, "list")) list_out$selected() else map_out$selected()
    })
  )

  observeEvent(input$view_mode, {
    shinyjs::toggle("list_container", condition = input$view_mode == "list")
    shinyjs::toggle("map_container",  condition = input$view_mode == "map")
    if (input$view_mode == "map") {
      runjs("setTimeout(function() {
        var $map = $('#map_col').find('.leaflet-container');
        if ($map.length > 0) { var inst = HTMLWidgets.getInstance($map[0]); if (inst) inst.getMap().invalidateSize(); }
      }, 100);")
    }
  }, ignoreInit = TRUE)

  # Workbench
  wb_out <- workbench_server("workbench",
                             map_source = region_source,
                             controls_output = controls_out,
                             loaded_state = state_out$ledger,
                             sidecar_source = sidecar_out)

  observe({ req(wb_out()); bridge_data(wb_out()) })
  
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