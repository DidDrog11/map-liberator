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

    /* 1b. Entry form over the source document
       The entry form is filled in BY READING the source, so dimming the source
       while it is open defeats the purpose. Bootstrap's backdrop covers the
       whole page at 50% black; here it is barely there, and the source panel is
       lifted above it so it stays at full brightness.
       Stacking order: backdrop 1050 < source panel 1052 < modal 1055. */
    .modal-backdrop.show { opacity: 0.12; }

    body.modal-open #img_col {
      position: relative;
      z-index: 1052;
    }

    /* The entry form is draggable by its title bar and remembers where it was
       put (see the script below), so it starts centred and the operator decides
       where it belongs relative to the source. The entry form is the app's only
       small modal (mod_workbench passes size = 's'); the data-loading progress
       modal is medium and is left alone. If a second small modal is ever added,
       give this one its own class instead. */
    .modal-dialog.modal-sm .modal-header {
      cursor: move;
      user-select: none;
    }

    /* While dragging, stop the PDF iframe swallowing mousemove: without this
       the box sticks the moment the pointer crosses the source document. */
    body.dragging-entry-form iframe { pointer-events: none; }
    body.dragging-entry-form { user-select: none; }
    
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

  # Draggable, position-remembering entry form.
  # The form is filled in while reading the source document, so where it sits
  # relative to that document is the operator's call, not a fixed offset. The
  # position is kept in localStorage: it is a per-viewer convenience, not part
  # of the project, so it belongs in the browser rather than the ledger.
  tags$head(tags$script(HTML("
    (function () {
      var KEY  = 'mapliberator.entryFormPos';
      var EDGE = 8;                 // keep this much of the box on screen
      var drag = null;

      function read() {
        try { var p = JSON.parse(localStorage.getItem(KEY));
              return (p && isFinite(p.x) && isFinite(p.y)) ? p : {x: 0, y: 0}; }
        catch (e) { return {x: 0, y: 0}; }
      }
      function write(p) { try { localStorage.setItem(KEY, JSON.stringify(p)); } catch (e) {} }

      // Clamp so the box always stays reachable, whatever the window size was
      // when the position was saved.
      function clamp(el, x, y) {
        var r  = el.getBoundingClientRect();
        var cur = current(el);
        var left0 = r.left - cur.x, top0 = r.top - cur.y;   // untransformed origin
        var maxX = window.innerWidth  - left0 - EDGE;
        var minX = EDGE - left0 - r.width;
        var maxY = window.innerHeight - top0 - EDGE;
        var minY = EDGE - top0;                             // never above the top
        return { x: Math.min(Math.max(x, minX), maxX),
                 y: Math.min(Math.max(y, minY), maxY) };
      }

      function current(el) {
        var m = /translate\\((-?[0-9.]+)px,\\s*(-?[0-9.]+)px\\)/.exec(el.style.transform || '');
        return m ? {x: parseFloat(m[1]), y: parseFloat(m[2])} : {x: 0, y: 0};
      }
      function apply(el, p) { el.style.transform = 'translate(' + p.x + 'px, ' + p.y + 'px)'; }

      function dialog(root) { return root.querySelector('.modal-dialog.modal-sm'); }

      // Restore on open. showModal() rebuilds the dialog each time, so this has
      // to run on every shown event, not once at startup.
      //
      // Bound twice on purpose. Bootstrap 5 dispatches native events, but Shiny
      // drives the modal through jQuery, and a jQuery-triggered event is not
      // seen by a native listener. Binding both ways covers either route; the
      // handler only sets a position, so firing twice costs nothing.
      function restore(root) {
        var el = dialog(root); if (!el) return;
        el.style.transition = 'none';
        apply(el, clamp(el, read().x, read().y));
      }
      document.addEventListener('shown.bs.modal', function (e) { restore(e.target); });
      if (window.jQuery) {
        jQuery(document).on('shown.bs.modal', function (e) { restore(e.target); });
      }

      document.addEventListener('mousedown', function (e) {
        var hd = e.target.closest('.modal-dialog.modal-sm .modal-header');
        if (!hd || e.button !== 0) return;
        var el = hd.closest('.modal-dialog');
        var c  = current(el);
        drag = {el: el, sx: e.clientX, sy: e.clientY, bx: c.x, by: c.y};
        document.body.classList.add('dragging-entry-form');
        e.preventDefault();
      });

      document.addEventListener('mousemove', function (e) {
        if (!drag) return;
        apply(drag.el, clamp(drag.el, drag.bx + (e.clientX - drag.sx),
                                      drag.by + (e.clientY - drag.sy)));
      });

      document.addEventListener('mouseup', function () {
        if (!drag) return;
        write(current(drag.el));
        document.body.classList.remove('dragging-entry-form');
        drag = null;
      });

      // Double-click the title bar to put it back in the middle.
      document.addEventListener('dblclick', function (e) {
        if (!e.target.closest('.modal-dialog.modal-sm .modal-header')) return;
        var el = e.target.closest('.modal-dialog');
        apply(el, {x: 0, y: 0});
        write({x: 0, y: 0});
      });

      // A window resize can leave a saved position off screen.
      window.addEventListener('resize', function () {
        document.querySelectorAll('.modal-dialog.modal-sm').forEach(function (el) {
          var p = clamp(el, current(el).x, current(el).y);
          apply(el, p); write(p);
        });
      });
    })();
  "))),
  
  titlePanel(
    div(style = "display: flex; align-items: center; gap: 12px;",
        # Setup (country, schema, metadata) is done once per session; for the
        # rest of it the panel is 25% of the width doing nothing, which the
        # source and the region list both want. Collapsing reclaims it.
        actionButton("toggle_sidebar", NULL, icon = icon("bars"),
                     class = "btn-sm btn-light", title = "Show or hide the setup panel"),
        div(icon("map-location-dot"), " Map Liberator")
    )
  ),
  
  fluidRow(
    div(id = "sidebar_col", class = "col-sm-3",
      tags$form(class = "well", role = "complementary",
        state_manager_ui("state"),
        hr(),
        controls_ui("ctrl"), 
        hr(),
        h4("Source Document"),
        actionButton("toggle_ref", "Show/Hide Source", icon=icon("eye"), class="btn-info btn-sm", width="100%"),
        br(), br(),
        sidecar_controls_ui("sidecar") 
      )
    ),
    
    div(id = "main_col", class = "col-sm-9",
      
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

  # Sidecar first: both region views and the workbench take it. The map
  # engine reads it while being constructed, so it must already exist.
  sidecar_out <- sidecar_server("sidecar")

  map_out <- map_engine_server("map", controls_output = controls_out, image = sidecar_out)
  
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
  
  # --- SIDEBAR COLLAPSE ---
  # Widening the main column changes the map's pixel size, so Leaflet has to
  # be told, exactly as for the other two layout toggles. The delay clears
  # Bootstrap's column reflow before the size is read.
  observeEvent(input$toggle_sidebar, {
    toggle("sidebar_col")
    toggleClass("main_col", "col-sm-9")
    toggleClass("main_col", "col-sm-12")

    runjs("
      setTimeout(function() {
        var $map = $('#map_col').find('.leaflet-container');
        if ($map.length > 0) {
          var inst = HTMLWidgets.getInstance($map[0]);
          if (inst) inst.getMap().invalidateSize();
        }
      }, 250);
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