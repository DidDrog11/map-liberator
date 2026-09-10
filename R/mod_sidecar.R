# R/mod_sidecar.R
# ------------------------------------------------------------------------------
# MODULE: Sidecar (Reference Image Viewer)
#
# Displays the static source map (the thing being digitised) next to the
# interactive Leaflet canvas, and exposes provenance about which image is
# currently loaded so the workbench can stamp it onto every extracted row.
# ------------------------------------------------------------------------------

# 1. CONTROLS UI (Goes in Sidebar)
sidecar_controls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("img_file"), "Upload Map", accept = c("image/png", "image/jpeg"), placeholder = "No file"),

    # Compact sliders
    div(style = "display: flex; gap: 5px;",
        div(style = "flex: 1;", sliderInput(ns("img_rotate"), "Rotate", min = -180, max = 180, value = 0, step = 90, ticks = FALSE)),
        div(style = "flex: 1;", sliderInput(ns("img_zoom"), "Zoom", min = 0.5, max = 4, value = 1, step = 0.1, ticks = FALSE))
    )
  )
}

# 2. PANEL UI (Goes in Main Workspace)
sidecar_panel_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("image_display"))
}

# 3. SERVER
# Returns a reactive holding the current reference image's filename and the
# time it was loaded. `loaded_at` is the clock start for per-map digitisation
# timing; the workbench subtracts it from the commit time.
sidecar_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    image_meta <- reactiveVal(list(
      file_name = NA_character_,
      loaded_at = as.POSIXct(NA)
    ))

    observeEvent(input$img_file, {
      image_meta(list(
        file_name = input$img_file$name,
        loaded_at = Sys.time()
      ))
    })

    output$image_display <- renderUI({
      container_style <- paste(
        "width: 100%; height: 85vh; overflow: auto;",
        "border: 2px dashed #ccc; background-color: #f9f9f9;",
        "display: flex; align-items: center; justify-content: center;"
      )

      if (is.null(input$img_file)) {
        return(div(style = container_style, h5(class = "text-muted", "Upload reference map in sidebar.")))
      }

      # The image is inlined as a data URI rather than served from www/, so
      # uploads never touch disk beyond Shiny's temp file.
      base64 <- base64enc::dataURI(file = input$img_file$datapath, mime = input$img_file$type)

      # Alignment is done purely in CSS. The underlying pixels are untouched,
      # so rotation/zoom cost nothing and are fully reversible.
      transform_style <- paste0(
        "transform: rotate(", input$img_rotate, "deg) scale(", input$img_zoom, ");",
        " transition: transform 0.1s ease; max-width: 90%;"
      )

      div(style = container_style,
          tags$img(src = base64, id = "ref_image", style = transform_style)
      )
    })

    return(image_meta)
  })
}
