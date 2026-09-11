# R/mod_controls.R
# ------------------------------------------------------------------------------
# MODULE: Control Panel
# ------------------------------------------------------------------------------

controls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    accordion(
      open = "Project Metadata", # Default open for data entry flow
      
      # --- PANEL 1: METADATA ---
      accordion_panel(
        "Project Metadata",
        icon = icon("clipboard-list"),
        textInput(ns("project_name"), "Project Name", value = "My_Extraction"),
        # Reference period. A weekly report needs one date; a monthly bulletin
        # or a cumulative report spans two, so an optional end date follows.
        # Left blank, the row's period is the single date.
        hint_label("Reference date (or period start)",
                   "The date the values describe. For a report covering more than ",
                   "one day, give its first day here and its last day under ",
                   tags$b("Period end"), ". Use XX for an unknown month or day."),
        splitLayout(
          textInput(ns("meta_year"), "Year", value = format(Sys.Date(), "%Y")),
          textInput(ns("meta_month"), "Month", placeholder = "MM (or XX)"),
          textInput(ns("meta_day"), "Day", placeholder = "DD (or XX)")
        ),
        span(class = "text-muted", style = "font-size: 12px;", "Period end (optional)"),
        splitLayout(
          textInput(ns("meta_end_year"), NULL, placeholder = "YYYY"),
          textInput(ns("meta_end_month"), NULL, placeholder = "MM"),
          textInput(ns("meta_end_day"), NULL, placeholder = "DD")
        ),
        # Epi week is recorded separately from the calendar date. Weekly
        # surveillance reports are indexed by epidemiological week, and
        # synthesising a date from it (as earlier versions did) loses the
        # reporting period and makes week-level joins unreliable.
        splitLayout(
          numericInput(ns("meta_week"), "Epi Week", value = NA, min = 1, max = 53, step = 1),
          textInput(ns("meta_source"), "Source ID", placeholder = "e.g. SitRep 42")
        )
      ),
      
      # --- PANEL 2: GEOGRAPHY (Setup & Navigation) ---
      accordion_panel(
        "Geography Scope",
        icon = icon("globe"),
        
        # Inputs
        selectizeInput(ns("country"), "Country:", choices = NULL, options = list(placeholder = 'Type country...', maxOptions = 1000)),
        radioButtons(ns("target_level"), "Target Level (Interact):",
                     choices = c("Admin 1" = 1, "Admin 2" = 2, "Admin 3" = 3),
                     selected = 2, inline = TRUE),
        uiOutput(ns("filter_ui_admin1")),
        uiOutput(ns("filter_ui_admin2")),
        
        hr(),
        
        # Actions
        actionButton(ns("load_data"), "1. Load Data", class = "btn-primary", width = "100%", icon = icon("database")),
        div(style = "margin-top: 5px; margin-bottom: 5px;", uiOutput(ns("visualise_ui"))),
        
        splitLayout(
          cellWidths = c("50%", "50%"),
          actionButton(ns("zoom_map"), "3. Zoom", class = "btn-info", width = "95%", icon = icon("expand")),
          actionButton(ns("clear_map"), "Reset", class = "btn-danger", width = "95%", icon = icon("trash"))
        )
      ),
      
      # --- PANEL 3: DATA ENTRY (Attributes & Add) ---
      accordion_panel(
        "Data Attributes",
        icon = icon("pen-to-square"),
        
        # Two capture modes. "single" paints one variable across a batch of
        # selected regions, which suits a choropleth where many regions share
        # a value. "form" opens a validated, schema-driven form for one region
        # at a time, which is what a tabulated source requires when each region
        # carries several different values.
        radioButtons(
          ns("entry_mode"),
          hint_label(
            "Entry Mode:", title = "Which mode to use",
            tags$p(tags$b("Paint one value:"), " select several regions on the map, ",
                   "then add one variable with one value to all of them at once. ",
                   "Use this for a choropleth or presence map, where many regions share a value."),
            tags$p(tags$b("Form per region:"), " declare the variables first, then click ",
                   "a single region and fill in all its values in a form. ",
                   "Use this for a table or labelled map, where each region carries several different values."),
            tags$p(class = "mb-0", "Both modes write to the same ledger and can be mixed in one project.")
          ),
          choices = c("Paint one value onto selected regions" = "single",
                      "Fill in a form for each region"        = "form"),
          selected = "single"),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'single'", ns("entry_mode")),
          textInput(ns("var_name"),
                    hint_label("Variable Name:",
                               "The column this value is recorded under, for example ",
                               tags$code("cases"), " or ", tags$code("presence"),
                               ". Reuse the same name across batches so the values line up in the export."),
                    value = "status", placeholder = "e.g. cases, presence"),
          selectInput(ns("var_type"),
                      hint_label("Data Type:",
                                 tags$b("Binary"), ": the region has the feature (recorded as 1). ",
                                 tags$b("Numeric"), ": a count or measurement. ",
                                 tags$b("Text"), ": a category or label read from the legend."),
                      choices = c("Binary (Present)"="binary", "Numeric (Count)"="numeric", "Text"="text")),
          uiOutput(ns("var_value_ui"))
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'form'", ns("entry_mode")),
          schema_ui(ns("schema"))
        ),
        
        hr(),
        
        # Batch mode commits explicitly; form mode commits per region from
        # the modal, so the button would be ambiguous there.
        conditionalPanel(
          condition = sprintf("input['%s'] == 'single'", ns("entry_mode")),
          actionButton(ns("add_to_project"), "Add Data to Ledger", class = "btn-success", width = "100%", icon = icon("plus-circle"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'form'", ns("entry_mode")),
          div(class = "text-muted", style = "font-size: 12px; text-align: center; font-style: italic;",
              "Click a region on the map to enter its values.")
        )
      )
    )
  )
}

controls_server <- function(id, restore_schema = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    updateSelectizeInput(session, "country", choices = country_vec, selected = "GBR", server = TRUE)

    # Schema module is nested inside the control panel: the declared variable
    # set is part of the extraction setup, alongside country and admin level.
    schema_out <- schema_server("schema", restore = restore_schema)

    # The project name travels with the file too, so a resumed extraction
    # keeps stamping the same name on new rows.
    if (is.function(restore_schema)) {
      observeEvent(restore_schema(), {
        r <- restore_schema()
        req(!is.null(r), !is.null(r$project_name), nzchar(r$project_name))
        updateTextInput(session, "project_name", value = r$project_name)
      })
    }
    
    # --- DYNAMIC DATA INPUT UI ---
    output$var_value_ui <- renderUI({
      req(input$var_type)
      if (input$var_type == "numeric") {
        numericInput(ns("var_value"), "Value:", value = 0, min = 0)
      } else if (input$var_type == "binary") {
        textInput(ns("var_value"), "Value:", value = "1", placeholder = "1")
      } else {
        textInput(ns("var_value"), "Value:", value = "", placeholder = "Enter text...")
      }
    })
    
    # --- METADATA HELPERS ---
    get_names_only <- function(iso, level, filter_col=NULL, filter_val=NULL) {
      tryCatch({
        f_path <- file.path(LOCAL_CACHE_DIR, paste0("gadm41_", iso, "_3_pk.rds"))
        if (!file.exists(f_path)) f_path <- file.path(LOCAL_CACHE_DIR, paste0("gadm41_", iso, "_2_pk.rds"))
        if (!file.exists(f_path)) f_path <- file.path(LOCAL_CACHE_DIR, paste0("gadm41_", iso, "_1_pk.rds"))
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
    
    # --- PIPELINE ENGINE ---
    loaded_cache <- reactiveValues(target = NULL, context = list(), ready = FALSE)
    pipeline_data <- reactiveValues(raw = NULL, target = NULL)
    
    engine_step <- reactiveVal("idle")
    
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
      
      showModal(modalDialog(
        title = paste("Processing", input$country),
        div(style = "margin-left: 15%; margin-top: 15px; font-size: 1.1em;",
            div(p(HTML(paste0("<span id='", ns("s1_icon"), "'><i class='fa fa-circle text-muted'></i></span> Loading Raw Borders")))),
            div(p(HTML(paste0("<span id='", ns("s2_icon"), "'><i class='fa fa-circle text-muted'></i></span> Filtering & Simplifying")))),
            div(p(HTML(paste0("<span id='", ns("s3_icon"), "'><i class='fa fa-circle text-muted'></i></span> Building Context (Dynamic)"))))
        ),
        footer = NULL, easyClose = FALSE
      ))
      
      engine_step("init")
    })
    
    # --- THE CHAIN ---
    observe({
      step <- engine_step()
      if(step == "idle") return()
      
      # >> INIT -> STEP 1
      if(step == "init") {
        set_status(ns("s1_icon"), "running")
        invalidateLater(100)
        engine_step("do_s1")
      }
      
      # >> DO STEP 1 (Load Raw)
      else if(step == "do_s1") {
        target_lvl <- as.numeric(input$target_level)
        
        raw <- tryCatch({
          load_gadm_locally(input$country, level = target_lvl) |> add_hierarchy_label()
        }, error = function(e) {
          showNotification(paste("Load failed:", e$message), type = "error")
          return(NULL)
        })
        
        if(is.null(raw) || nrow(raw) == 0) {
          set_status(ns("s1_icon"), "pending")
          showNotification("No data loaded. Check country selection.", type = "error")
          removeModal()
          engine_step("idle")
          return()
        }
        
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
      
      # >> DO STEP 2 (Filter)
      else if(step == "do_s2") {
        target_raw <- pipeline_data$raw
        if(is.null(target_raw)) { removeModal(); return() }
        
        if (!is.null(input$filter_a1) && length(input$filter_a1) > 0 && "NAME_1" %in% names(target_raw)) {
          target_raw <- target_raw |> filter(NAME_1 %in% input$filter_a1)
        }
        if (!is.null(input$filter_a2) && length(input$filter_a2) > 0 && "NAME_2" %in% names(target_raw)) {
          target_raw <- target_raw |> filter(NAME_2 %in% input$filter_a2)
        }
        
        if (nrow(target_raw) == 0) {
          removeModal()
          showNotification("Filter resulted in 0 areas!", type = "warning")
          engine_step("idle")
          return()
        }
        
        target_lvl <- as.numeric(input$target_level)
        id_col <- paste0("GID_", target_lvl)
        if(id_col %in% names(target_raw)) {
          target_raw$layerId <- target_raw[[id_col]]
        } else {
          target_raw$layerId <- as.character(1:nrow(target_raw))
        }
        
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
        target_sf  <- pipeline_data$target
        context_layers <- list()
        
        # Helper: Aggregate upwards
        make_context <- function(data, level_idx) {
          cols <- c(paste0("GID_", level_idx), paste0("NAME_", level_idx))
          if (all(cols %in% names(data))) {
            return(
              data |> 
                group_by(across(all_of(cols))) |> 
                summarise(geometry = st_union(geometry), .groups = "drop")
            )
          } else {
            return(load_gadm_locally(input$country, level = level_idx))
          }
        }
        
        try({ context_layers$Adm0 <- make_context(target_sf, 0) }, silent = TRUE)
        if (target_lvl > 1) try({ context_layers$Adm1 <- make_context(target_sf, 1) }, silent = TRUE)
        if (target_lvl > 2) try({ context_layers$Adm2 <- make_context(target_sf, 2) }, silent = TRUE)
        
        loaded_cache$target <- pipeline_data$target
        loaded_cache$context <- context_layers
        loaded_cache$ready <- TRUE
        
        pipeline_data$raw <- NULL
        gc() 
        
        set_status(ns("s3_icon"), "done")
        engine_step("finish")
      }
      
      # >> FINISH
      else if(step == "finish") {
        invalidateLater(500) 
        engine_step("close")
      }
      
      else if(step == "close") {
        removeModal()
        showNotification("Data Ready.", type = "message")
        engine_step("idle")
      }
    })
    
    # --- VISUALISE UI ---
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
    
    # --- RETURNS ---
    list(
      geom_data = reactive(geometry_output$target),
      context_data = reactive(geometry_output$context),
      geom_trigger = reactive(geometry_output$trigger),
      zoom_trigger = reactive(input$zoom_map),
      metadata = reactive({
        list(
          project = input$project_name, 
          year = input$meta_year,
          month = input$meta_month,
          day = input$meta_day,
          end_year  = input$meta_end_year,
          end_month = input$meta_end_month,
          end_day   = input$meta_end_day,
          week = input$meta_week,
          source = input$meta_source,
          var_name = input$var_name,
          var_value = input$var_value
        )
      }),
      add_trigger  = reactive(input$add_to_project),
      clear_trigger = reactive(input$clear_map),

      # Form-mode contract consumed by the workbench.
      entry_mode   = reactive(input$entry_mode),
      schema       = schema_out$schema,
      rules        = schema_out$rules,
      schema_valid = schema_out$valid,

      # Raw declaration text, so the state manager can persist it verbatim.
      schema_text  = schema_out$schema_text,
      rules_text   = schema_out$rules_text,
      blank_zero   = schema_out$blank_zero
    )
  })
}