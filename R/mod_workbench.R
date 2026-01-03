# R/mod_workbench.R
workbench_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "card", style="padding:15px; background-color: #fff;",
        h4("Project Summary"),
        div(style="display:flex; justify-content:space-between; align-items:center;",
            p(class="text-muted", "Review your extracted data before downloading."),
            actionButton(ns("delete_rows"), "Delete Selected", class = "btn-warning btn-sm", icon = icon("trash"))
        ),
        DTOutput(ns("project_table")),
        br(),
        downloadButton(ns("download_project"), "Download Final CSV", class = "btn-success", style="width:100%")
    )
  )
}

workbench_server <- function(id, map_output, controls_output, loaded_state = NULL) {
  moduleServer(id, function(input, output, session) {
    
    project_data <- reactiveVal(data.frame())
    
    # Load State
    if (!is.null(loaded_state)) {
      observeEvent(loaded_state(), { req(loaded_state()); project_data(loaded_state()) })
    }
    
    # --- ADD TO PROJECT LOGIC ---
    # Triggered by the Sidebar button
    observeEvent(controls_output$add_trigger(), {
      ids <- map_output$current_selection()
      data <- map_output$current_data()
      meta <- controls_output$metadata()
      
      if (length(ids) == 0) {
        showNotification("No areas selected!", type = "warning")
        return()
      }
      
      # Handle Defaults for Date
      yr <- if (nzchar(meta$year)) meta$year else "XX"
      mo <- if (nzchar(meta$month)) meta$month else "XX"
      dy <- if (nzchar(meta$day)) meta$day else "XX"
      
      selected_poly <- data |> st_drop_geometry() |> filter(layerId %in% ids)
      
      new_rows <- selected_poly |> 
        mutate(
          Project_Name = meta$project,
          Report_Year = yr,
          Report_Month = mo,
          Report_Day = dy,
          Source_ID = meta$source,
          Entry_ID = paste(layerId, yr, mo, dy, sep = "_"),
          Commit_Timestamp = Sys.time()
        )
      
      showNotification(paste("Added", nrow(new_rows), "areas to project."), type = "message")
      
      current <- project_data()
      if(nrow(current) > 0) new_rows <- new_rows[!new_rows$Entry_ID %in% current$Entry_ID, ]
      
      if(nrow(new_rows) > 0) project_data(bind_rows(current, new_rows))
    })
    
    # Delete Logic
    observeEvent(input$delete_rows, {
      req(input$project_table_rows_selected)
      project_data(project_data()[-input$project_table_rows_selected, ])
    })
    
    # Render Table
    output$project_table <- renderDT({
      project_data() |> select(any_of(c("Project_Name", "NAME_1", "NAME_2", "Report_Year", "Report_Month", "Source_ID")))
    }, options = list(pageLength = 10, dom = 'tip'))
    
    output$download_project <- downloadHandler(
      filename = function() { paste0("MapLiberator_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(project_data(), file, row.names = FALSE) }
    )
    
    return(project_data)
  })
}