# R/mod_sidecar.R
# ------------------------------------------------------------------------------
# MODULE: Sidecar (Image Viewer)
# PURPOSE: Displays the static reference map with zoom/rotate controls.
# ------------------------------------------------------------------------------

sidecar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Controls embedded above the image (or could be in sidebar)
    div(style = "background: #eee; padding: 5px; margin-bottom: 5px; border-radius: 4px; display: flex; gap: 10px;",
        fileInput(ns("img_file"), NULL, buttonLabel = "Upload Map", placeholder = "No file", accept = c("image/png", "image/jpeg"), width = "200px"),
        div(style = "flex-grow: 1;", sliderInput(ns("img_rotate"), NULL, min = -180, max = 180, value = 0, step = 90, ticks = FALSE, width="100%")),
        div(style = "flex-grow: 1;", sliderInput(ns("img_zoom"), NULL, min = 0.5, max = 4, value = 1, step = 0.1, ticks = FALSE, width="100%"))
    ),
    uiOutput(ns("image_ui"))
  )
}

sidecar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$image_ui <- renderUI({
      # Default CSS for container
      container_style <- "width: 100%; height: 75vh; overflow: auto; border: 2px dashed #ccc; background-color: #f9f9f9; display: flex; align-items: center; justify-content: center;"
      
      if (is.null(input$img_file)) {
        return(div(style = container_style, h5("Upload a reference map to trace.")))
      }
      
      # Prepare Image
      base64 <- dataURI(file = input$img_file$datapath, mime = input$img_file$type)
      transform_style <- paste0("transform: rotate(", input$img_rotate, "deg) scale(", input$img_zoom, "); transition: transform 0.1s ease;")
      
      div(style = container_style,
          img(src = base64, id = "ref_image", style = transform_style, style="max-width:90%;")
      )
    })
  })
}