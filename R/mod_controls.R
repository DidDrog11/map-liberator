# R/mod_controls.R
# ------------------------------------------------------------------------------
# MODULE: Control Panel (Optimized)
# ------------------------------------------------------------------------------

controls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    accordion(
      open = "Geography Scope",
      accordion_panel(
        "Project Metadata",
        icon = icon("clipboard-list"),
        textInput(ns("project_name"), "Project Name", value = "My_Extraction"),
        splitLayout(
          textInput(ns("meta_year"), "Year", value = format(Sys.Date(), "%Y")),
          textInput(ns("meta_month"), "Month", placeholder = "MM (or XX)"),
          textInput(ns("meta_day"), "Day", placeholder = "DD (or XX)")
        ),
        textInput(ns("meta_source"), "Source ID", placeholder = "e.g. SitRep 42")
      ),
      accordion_panel(
        "Geography Scope",
        icon = icon("globe"),
        selectizeInput(ns("country"), "Country:", choices = NULL, options = list(placeholder = 'Type country...', maxOptions = 1000)),
        radioButtons(ns("target_level"), "Target Level (Interact):",
                     choices = c("Admin 1" = 1, "Admin 2" = 2, "Admin 3" = 3),
                     selected = 2, inline = TRUE),
        uiOutput(ns("filter_ui_admin1")),
        uiOutput(ns("filter_ui_admin2")),
        hr(),
        checkboxInput(ns("simplify_geom"), "Fast Render (Simplify Polygons)", value = TRUE)
      )
    ),
    br(),
    h5("Actions"),
    actionButton(ns("load_data"), "1. Load Data", class = "btn-primary", width = "100%", icon = icon("database")),
    div(style = "margin-top: 5px; margin-bottom: 5px;", uiOutput(ns("visualise_ui"))),
    splitLayout(
      cellWidths = c("50%", "50%"),
      actionButton(ns("zoom_map"), "3. Zoom", class = "btn-info", width = "95%", icon = icon("expand")),
      actionButton(ns("clear_map"), "Reset", class = "btn-danger", width = "95%", icon = icon("trash"))
    ),
    br(),
    actionButton(ns("add_to_project"), "Add Selection to Project", class = "btn-success", width = "100%", icon = icon("plus-circle"))
  )
}

controls_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    updateSelectizeInput(session, "country", choices = country_vec, selected = "GBR", server = TRUE)
    
    # --- 1. METADATA HELPERS ---
    get_names_only <- function(iso, level, filter_col=NULL, filter_val=NULL) {
      tryCatch({
        f_path <- file.path(LOCAL_CACHE_DIR, paste0("gadm41_", iso, "_3_pk.rds"))
        if (!file.exists(f_path)) return(NULL)
        df <- as.data.frame(readRDS(f_path)) 
        if (!is.null(filter_col) && !is.null(filter_val)) {
          if (filter_col %in% names(df)) df <- df[df[[filter_col]] %in% filter_val, ]
        }
        col_name <- paste0("NAME_", level)
        if(col_name %in% names(df)) return(sort(unique(df[[col_name]])))
        return(NULL)
      }, error = function(e) return(NULL))
    }
    
    output$filter_ui_admin1 <- renderUI({
      req(input$country)
      names <- get_names_only(input$country, 1)
      if(!is.null(names)) selectizeInput(ns("filter_a1"), "Filter Admin 1:", choices = names, multiple = TRUE)
    })
    
    output$filter_ui_admin2 <- renderUI({
      req(input$country)
      if (as.numeric(input$target_level) < 2) return(NULL)
      parent_filter <- if (!is.null(input$filter_a1)) input$filter_a1 else NULL
      names <- get_names_only(input$country, 2, "NAME_1", parent_filter)
      if(!is.null(names)) selectizeInput(ns("filter_a2"), "Filter Admin 2:", choices = names, multiple = TRUE)
    })
    
    # --- 2. THE PIPELINE ENGINE ---
    loaded_cache <- reactiveValues(target = NULL, context = list(), ready = FALSE)
    pipeline_data <- reactiveValues(raw = NULL, target = NULL)
    
    # Tracks the current step ("idle", "step1", "step2", "step3")
    engine_step <- reactiveVal("idle")
    
    # Helper to update icons via JavaScript (Instant, no flicker)
    set_status <- function(step_id, status) {
      icon_html <- switch(status,
                          "pending" = "<i class='fa fa-circle text-muted'></i>",
                          "running" = "<i class='fa fa-spinner fa-spin text-primary'></i>",
                          "done"    = "<i class='fa fa-check text-success'></i>")
      runjs(sprintf("document.getElementById('%s').innerHTML = \"%s\";", step_id, icon_html))
    }
    
    # --- TRIGGER START ---
    observeEvent(input$load_data, {
      req(input$country)
      loaded_cache$ready <- FALSE
      
      # 1. SHOW MODAL (Direct HTML = Instant Load)
      # We give each icon a specific ID (e.g. "s1_icon") so we can target it later
      showModal(modalDialog(
        title = paste("Processing", input$country),
        div(style = "margin-left: 15%; margin-top: 15px; font-size: 1.1em;",
            div(p(HTML(paste0("<span id='", ns("s1_icon"), "'><i class='fa fa-circle text-muted'></i></span> Loading Raw Borders")))),
            div(p(HTML(paste0("<span id='", ns("s2_icon"), "'><i class='fa fa-circle text-muted'></i></span> Filtering & Simplifying")))),
            div(p(HTML(paste0("<span id='", ns("s3_icon"), "'><i class='fa fa-circle text-muted'></i></span> Preparing Context Layers"))))
        ),
        footer = NULL, easyClose = FALSE
      ))
      
      # 2. Start the Chain
      engine_step("init")
    })
    
    # --- THE CHAIN ---
    observe({
      step <- engine_step()
      if(step == "idle") return()
      
      # >> INIT -> STEP 1
      if(step == "init") {
        set_status(ns("s1_icon"), "running")
        # Force a tiny wait to let the browser paint the spinner
        invalidateLater(100)
        engine_step("do_s1")
      }
      
      # >> DO STEP 1 (Load Raw)
      else if(step == "do_s1") {
        target_lvl <- as.numeric(input$target_level)
        raw <- tryCatch({
          load_gadm_locally(input$country, level = target_lvl) |> add_hierarchy_label()
        }, error = function(e) return(NULL))
        pipeline_data$raw <- raw
        
        set_status(ns("s1_icon"), "done")
        engine_step("prep_s2")
      }
      
      # >> PREP STEP 2
      else if(step == "prep_s2") {
        set_status(ns("s2_icon"), "running")
        invalidateLater(100)
        engine_step("do_s2")
      }
      
      # >> DO STEP 2 (Filter & Simplify)
      else if(step == "do_s2") {
        target_raw <- pipeline_data$raw
        if(is.null(target_raw)) { removeModal(); return() }
        
        # Filter
        if (!is.null(input$filter_a1) && length(input$filter_a1) > 0 && "NAME_1" %in% names(target_raw)) {
          target_raw <- target_raw |> filter(NAME_1 %in% input$filter_a1)
        }
        if (!is.null(input$filter_a2) && length(input$filter_a2) > 0 && "NAME_2" %in% names(target_raw)) {
          target_raw <- target_raw |> filter(NAME_2 %in% input$filter_a2)
        }
        
        if (nrow(target_raw) == 0) {
          removeModal(); showNotification("Filter resulted in 0 areas!", type = "warning"); return()
        }
        
        # Simplify
        if (input$simplify_geom) target_raw <- st_simplify(target_raw, preserveTopology = TRUE, dTolerance = 0.002)
        
        # ID
        target_lvl <- as.numeric(input$target_level)
        id_col <- paste0("GID_", target_lvl)
        if(id_col %in% names(target_raw)) target_raw$layerId <- target_raw[[id_col]] else target_raw$layerId <- as.character(1:nrow(target_raw))
        
        pipeline_data$target <- target_raw
        
        set_status(ns("s2_icon"), "done")
        engine_step("prep_s3")
      }
      
      # >> PREP STEP 3
      else if(step == "prep_s3") {
        set_status(ns("s3_icon"), "running")
        invalidateLater(100)
        engine_step("do_s3")
      }
      
      # >> DO STEP 3 (Context)
      else if(step == "do_s3") {
        target_lvl <- as.numeric(input$target_level)
        simplify <- input$simplify_geom
        context_layers <- list()
        
        try({ 
          l0 <- load_gadm_locally(input$country, level = 0)
          if(simplify) l0 <- st_simplify(l0, preserveTopology=TRUE, dTolerance=0.005)
          context_layers$Adm0 <- l0
        }, silent=TRUE)
        
        try({ 
          l1 <- load_gadm_locally(input$country, level = 1)
          if(simplify) l1 <- st_simplify(l1, preserveTopology=TRUE, dTolerance=0.003)
          context_layers$Adm1 <- l1
        }, silent=TRUE)
        
        if (target_lvl > 2) {
          try({ 
            l2 <- load_gadm_locally(input$country, level = 2)
            if(simplify) l2 <- st_simplify(l2, preserveTopology=TRUE, dTolerance=0.002)
            context_layers$Adm2 <- l2
          }, silent=TRUE)
        }
        
        loaded_cache$target <- pipeline_data$target
        loaded_cache$context <- context_layers
        loaded_cache$ready <- TRUE
        
        set_status(ns("s3_icon"), "done")
        engine_step("finish")
      }
      
      # >> FINISH
      else if(step == "finish") {
        invalidateLater(500) # Short pause to see all green ticks
        engine_step("close")
      }
      
      else if(step == "close") {
        removeModal()
        showNotification("Data Ready.", type = "message")
        engine_step("idle")
      }
    })
    
    # --- 3. VISUALISE & OUTPUTS ---
    output$visualise_ui <- renderUI({
      if (loaded_cache$ready) {
        actionButton(ns("plot_map"), "2. Visualise Map", class = "btn-warning", width = "100%", icon = icon("paint-brush"))
      } else {
        div(class="text-muted", style="text-align:center; font-style:italic;", "Load data first...")
      }
    })
    
    geometry_output <- reactiveValues(target = NULL, context = list(), trigger = 0)
    
    observeEvent(input$plot_map, {
      req(loaded_cache$ready)
      showNotification("Rendering...", type = "message")
      geometry_output$target <- loaded_cache$target
      geometry_output$context <- loaded_cache$context
      geometry_output$trigger <- geometry_output$trigger + 1
    })
    
    observeEvent(input$clear_map, {
      geometry_output$target <- NULL
      geometry_output$context <- list()
      geometry_output$trigger <- geometry_output$trigger + 1
      loaded_cache$ready <- FALSE
    })
    
    list(
      geom_data = reactive(geometry_output$target),
      context_data = reactive(geometry_output$context),
      geom_trigger = reactive(geometry_output$trigger),
      zoom_trigger = reactive(input$zoom_map),
      metadata = reactive({
        list(project = input$project_name, year = input$meta_year, month = input$meta_month, day = input$meta_day, source = input$meta_source)
      }),
      add_trigger  = reactive(input$add_to_project),
      clear_trigger = reactive(input$clear_map)
    )
  })
}