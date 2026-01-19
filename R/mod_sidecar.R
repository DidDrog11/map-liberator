# R/mod_sidecar.R
# ------------------------------------------------------------------------------
# MODULE: Sidecar (Image Viewer)
# ------------------------------------------------------------------------------

# 1. CONTROLS UI (Goes in Sidebar)
sidecar_controls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("img_file"), "Upload Map", accept = c("image/png", "image/jpeg"), placeholder = "No file"),
    
    # Compact sliders
    div(style="display: flex; gap: 5px;",
        div(style="flex: 1;", sliderInput(ns("img_rotate"), "Rotate", min = -180, max = 180, value = 0, step = 90, ticks = FALSE)),
        div(style="flex: 1;", sliderInput(ns("img_zoom"), "Zoom", min = 0.5, max = 4, value = 1, step = 0.1, ticks = FALSE))
    )
  )
}

# 2. PANEL UI (Goes in Main Workspace)
sidecar_panel_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("image_display"))
}

# 3. SERVER
sidecar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$image_display <- renderUI({
      # Container Style
      container_style <- "width: 100%; height: 85vh; overflow: auto; border: 2px dashed #ccc; background-color: #f9f9f9; display: flex; align-items: center; justify-content: center;"
      
      if (is.null(input$img_file)) {
        return(div(style = container_style, h5(class="text-muted", "Upload reference map in sidebar.")))
      }
      
      # Prepare Image
      base64 <- base64enc::dataURI(file = input$img_file$datapath, mime = input$img_file$type)
      transform_style <- paste0("transform: rotate(", input$img_rotate, "deg) scale(", input$img_zoom, "); transition: transform 0.1s ease; max-width: 90%;")
      
      div(style = container_style,
          tags$img(src = base64, id = "ref_image", style = transform_style)
      )
    })
  })
}