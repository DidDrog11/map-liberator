# R/mod_scope.R
scope_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("1. Define Scope"),
    
    selectizeInput(ns("country"), "Select Country:", choices = NULL, 
                   options = list(placeholder = 'Type country name...', maxOptions = 1000)),
    
    radioButtons(ns("target_level"), "Extraction Level:",
                 choices = c("Level 1 (State)" = 1, "Level 2 (District)" = 2, "Level 3 (Ward)" = 3),
                 selected = 2, inline = TRUE),
    hr(),
    uiOutput(ns("filter_ui_admin1")),
    br(),
    
    splitLayout(
      actionButton(ns("load_geometry"), "Load Map", class = "btn-success", width = "100%", icon = icon("map")),
      actionButton(ns("remove_geometry"), "Clear Map", class = "btn-warning", width = "100%", icon = icon("trash"))
    ),
    textOutput(ns("status_msg"))
  )
}

scope_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    updateSelectizeInput(session, "country", choices = country_vec, selected = "NGA", server = TRUE)
    
    # --- FIX: REMOVED CONFLICTING OBSERVER ---
    # We rely purely on renderUI to regenerate the input when country changes.
    
    # 1. Admin 1 Metadata
    r_country_l1 <- reactive({
      req(input$country)
      tryCatch({ load_gadm_locally(input$country, level = 1) }, error = function(e) return(NULL))
    })
    
    output$filter_ui_admin1 <- renderUI({
      req(r_country_l1()) # Wait for new data
      data <- r_country_l1()
      
      if("NAME_1" %in% names(data)) {
        choices <- sort(unique(data$NAME_1))
        # This creates a FRESH input every time country changes
        selectizeInput(ns("filter_a1"), "Filter by Region/State:", choices = choices, multiple = TRUE)
      } else { NULL }
    })
    
    # 2. Geometry Bundle
    geometry_output <- reactiveValues(data = NULL, trigger = 0)
    
    observeEvent(input$load_geometry, {
      req(input$country, input$target_level)
      showNotification("Loading geometry...", type = "message")
      
      lvl <- as.numeric(input$target_level)
      
      raw <- tryCatch({
        load_gadm_locally(input$country, level = lvl) |> 
          add_hierarchy_label() 
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        return(NULL)
      })
      
      req(raw)
      
      # Apply Filters
      final <- raw
      if (!is.null(input$filter_a1) && length(input$filter_a1) > 0 && "NAME_1" %in% names(final)) {
        final <- final |> filter(NAME_1 %in% input$filter_a1)
      }
      
      # Generate LayerID
      id_col_name <- paste0("GID_", lvl)
      if(id_col_name %in% names(final)) {
        final$layerId <- final[[id_col_name]]
      } else {
        final$layerId <- as.character(1:nrow(final))
      }
      
      geometry_output$data <- final
      geometry_output$trigger <- geometry_output$trigger + 1
    })
    
    observeEvent(input$remove_geometry, {
      geometry_output$data <- NULL
      geometry_output$trigger <- geometry_output$trigger + 1 
    })
    
    output$status_msg <- renderText({
      req(geometry_output$data)
      paste("Loaded:", nrow(geometry_output$data), "areas.")
    })
    
    return(geometry_output)
  })
}