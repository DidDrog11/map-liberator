# R/mod_workbench.R
# ------------------------------------------------------------------------------
# MODULE: Workbench (Data Ledger)
# ------------------------------------------------------------------------------

workbench_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "display: flex; gap: 10px; margin-bottom: 10px;",
        downloadButton(ns("download_csv"), "Download CSV", class = "btn-secondary btn-sm"),
        actionButton(ns("delete_rows"), "Delete Selected", icon = icon("trash"), class = "btn-danger btn-sm")
    ),
    DT::DTOutput(ns("ledger_table"))
  )
}

# Renamed argument 'map_output' -> 'map_source' for clarity
workbench_server <- function(id, map_source, controls_output, loaded_state) {
  moduleServer(id, function(input, output, session) {
    
    project_data <- reactiveVal(data.frame())
    
    # 1. ADD DATA
    observeEvent(controls_output$add_trigger(), {
      
      # A. Get Selection
      # map_source is now a list. We access the 'selected' reactive.
      if (!is.list(map_source) || is.null(map_source$selected)) {
        return() 
      }
      
      ids <- map_source$selected() # Get the vector of IDs (e.g. c("GID.1", "GID.5"))
      
      if (length(ids) == 0) {
        showNotification("No regions selected!", type = "warning")
        return()
      }
      
      # B. Get Metadata & Map Reference
      meta <- controls_output$metadata()
      current_map <- controls_output$geom_data()
      
      # C. Loop through IDs and create rows
      new_rows <- lapply(ids, function(id) {
        
        # Name Lookup
        region_name <- "Unknown"
        if (!is.null(current_map)) {
          record <- current_map[current_map$layerId == id, ]
          if (nrow(record) > 0) {
            if("NAME_3" %in% names(record)) region_name <- record$NAME_3
            else if("NAME_2" %in% names(record)) region_name <- record$NAME_2
            else if("NAME_1" %in% names(record)) region_name <- record$NAME_1
            else if("NAME_0" %in% names(record)) region_name <- record$NAME_0
          }
        }
        
        # Create Single Row
        data.frame(
          Timestamp = format(Sys.time(), "%H:%M:%S"),
          Project   = meta$project,
          Source    = meta$source,
          Date_Ref  = paste(meta$year, meta$month, meta$day, sep="-"),
          Region_ID = id,
          Region_Name = region_name,
          Variable  = meta$var_name,
          Value     = as.character(meta$var_value),
          stringsAsFactors = FALSE
        )
      })
      
      # D. Combine and Save
      batch_df <- dplyr::bind_rows(new_rows)
      
      current <- project_data()
      updated <- dplyr::bind_rows(current, batch_df)
      project_data(updated)
      
      showNotification(paste("Added", length(ids), "rows to ledger."), type = "message")
    })
    
    # 2. LOAD STATE
    observe({
      req(loaded_state())
      if (nrow(loaded_state()) > 0) project_data(loaded_state())
    })
    
    # 3. DELETE
    observeEvent(input$delete_rows, {
      req(input$ledger_table_rows_selected)
      current <- project_data()
      if (nrow(current) > 0) {
        updated <- current[-input$ledger_table_rows_selected, ]
        project_data(updated)
      }
    })
    
    # 4. RENDER
    output$ledger_table <- DT::renderDT({
      req(project_data())
      DT::datatable(project_data(), 
                    options = list(pageLength = 5, scrollX = TRUE),
                    rownames = FALSE,
                    selection = "multiple")
    })
    
    # 5. DOWNLOAD
    output$download_csv <- downloadHandler(
      filename = function() { paste0("map_liberator_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(project_data(), file, row.names = FALSE) }
    )
    
    return(project_data)
  })
}