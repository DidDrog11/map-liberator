# R/mod_state_manager.R
# ------------------------------------------------------------------------------
# MODULE: State Manager
# PURPOSE: Save/Load the entire project state (selected data) to/from disk.
# ------------------------------------------------------------------------------

state_manager_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Project Files"),
    div(style="display:flex; gap:5px;",
        # Load Button (File Input)
        div(style="flex-grow:1;",
            fileInput(ns("load_file"), NULL, 
                      buttonLabel = "Load Project", 
                      placeholder = "No project loaded",
                      accept = ".rds")
        ),
        # Save Button
        div(style="margin-top:0px;",
            downloadButton(ns("save_file"), "Save", class = "btn-info")
        )
    ),
    textOutput(ns("status_msg"))
  )
}

state_manager_server <- function(id, data_to_save) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Save Logic
    output$save_file <- downloadHandler(
      filename = function() {
        paste0("MapLiberator_Project_", format(Sys.time(), "%Y%m%d_%H%M"), ".rds")
      },
      content = function(file) {
        req(data_to_save())
        # We save the data frame as an RDS (R native format)
        saveRDS(data_to_save(), file)
      }
    )
    
    # 2. Load Logic
    # We read the file and return the data as a reactive
    loaded_data <- reactive({
      req(input$load_file)
      tryCatch({
        readRDS(input$load_file$datapath)
      }, error = function(e) {
        showNotification("Invalid project file.", type = "error")
        return(NULL)
      })
    })
    
    output$status_msg <- renderText({
      if (is.null(input$load_file)) return("")
      paste("Loaded:", nrow(loaded_data()), "rows.")
    })
    
    return(loaded_data)
  })
}