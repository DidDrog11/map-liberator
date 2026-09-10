# R/mod_sidecar.R
# ------------------------------------------------------------------------------
# MODULE: Sidecar (Reference Document Viewer)
#
# Displays the static source (the thing being digitised) next to the
# interactive Leaflet canvas, and exposes provenance about which file is
# currently loaded so the workbench can stamp it onto every extracted row.
#
# Two kinds of source:
#   image (PNG/JPEG)  inlined as a data URI, aligned with CSS rotate/scale,
#                     and drag-to-pan.
#   PDF               shown in the browser's own PDF viewer, which brings page
#                     navigation, zoom and text search for free. The file is
#                     served from a per-session resource path, not inlined,
#                     because data-URI PDFs are size-limited in some browsers.
#   Operators work from the report they were given, in either form; nothing
#   has to be converted first.
#
# TIMING
#   Loading a file starts a clock. The operator can pause it (to read an
#   unfamiliar layout, take a break, or consult a legend) and resume it; the
#   workbench records active seconds, net of pauses, on every committed row,
#   alongside the paused total so the raw elapsed time can be reconstructed.
# ------------------------------------------------------------------------------

# 1. CONTROLS UI (Goes in Sidebar)
sidecar_controls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("img_file"), "Upload reference (image or PDF)",
              accept = c("image/png", "image/jpeg", "application/pdf", ".pdf"),
              placeholder = "No file"),

    # Image alignment. Hidden for a PDF, whose viewer has its own controls.
    div(id = ns("image_controls"), style = "display: flex; gap: 5px;",
        div(style = "flex: 1;", sliderInput(ns("img_rotate"), "Rotate", min = -180, max = 180, value = 0, step = 90, ticks = FALSE)),
        div(style = "flex: 1;", sliderInput(ns("img_zoom"), "Zoom", min = 0.5, max = 4, value = 1, step = 0.1, ticks = FALSE))
    ),

    # Timing clock: starts on file load, pausable.
    div(style = "display: flex; gap: 5px; align-items: center; margin-top: 4px;",
        actionButton(ns("toggle_clock"), "Pause clock", icon = icon("pause"),
                     class = "btn-outline-secondary btn-sm", style = "flex: 1;"),
        div(style = "flex: 1; font-size: 12px;", textOutput(ns("clock_status"), inline = TRUE))
    )
  )
}

# 2. PANEL UI (Goes in Main Workspace)
sidecar_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("image_display")),
    # Drag-to-pan for images. Zoom is a CSS scale, which does not change
    # layout, so an enlarged image overflows the frame on every side and the
    # top edge can never be scrolled to. Panning moves the image by a
    # translate() that is composed with the rotate/scale set from R via two
    # CSS variables, so the sliders and the drag never overwrite each other.
    # Double-click recentres. Handlers are delegated from the document
    # because renderUI recreates the <img> on every slider change.
    tags$script(HTML(sprintf("
      (function() {
        var outId = '%s', sel = '#' + outId + ' img';
        var pan = { tx: 0, ty: 0, file: null }, drag = null;
        function apply(el) {
          el.style.setProperty('--tx', pan.tx + 'px');
          el.style.setProperty('--ty', pan.ty + 'px');
        }
        $(document).on('mousedown', sel, function(e) {
          if (e.button !== 0) return;
          drag = { el: this, x: e.clientX, y: e.clientY, tx: pan.tx, ty: pan.ty };
          this.style.transition = 'none';
          e.preventDefault();
        });
        $(document).on('mousemove', function(e) {
          if (!drag) return;
          pan.tx = drag.tx + e.clientX - drag.x;
          pan.ty = drag.ty + e.clientY - drag.y;
          apply(drag.el);
        });
        $(document).on('mouseup mouseleave', function() {
          if (!drag) return;
          drag.el.style.transition = '';
          drag = null;
        });
        $(document).on('dblclick', sel, function() {
          pan.tx = 0; pan.ty = 0; apply(this);
        });
        // Slider changes re-render the <img>; carry the pan across, but start
        // centred when a different file is loaded.
        $(document).on('shiny:value', function(e) {
          if (e.name !== outId) return;
          setTimeout(function() {
            var el = $(sel)[0]; if (!el) return;
            var f = el.getAttribute('data-file');
            if (f !== pan.file) { pan.file = f; pan.tx = 0; pan.ty = 0; }
            apply(el);
          }, 0);
        });
      })();
    ", ns("image_display"))))
  )
}

# 3. SERVER
# Returns a reactive holding a list:
#   file_name    name of the current reference file
#   loaded_at    when it was loaded (the clock start); NA before any upload
#   paused_at    start of the current pause; NA when the clock is running
#   paused_secs  total seconds of completed pauses since loaded_at
# The workbench turns these into active seconds at commit time.
sidecar_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    image_meta <- reactiveVal(list(
      file_name = NA_character_,
      loaded_at = as.POSIXct(NA)
    ))

    clock <- reactiveValues(paused_at = as.POSIXct(NA), paused_secs = 0)

    is_pdf <- reactive({
      f <- input$img_file
      !is.null(f) && (identical(f$type, "application/pdf") || grepl("\\.pdf$", f$name, ignore.case = TRUE))
    })

    # PDFs are served from a per-session directory under a random prefix,
    # so one session can never reach another's upload. Cleared on exit.
    pdf_prefix <- paste0("sidecar-", session$token)
    pdf_dir    <- file.path(tempdir(), pdf_prefix)
    dir.create(pdf_dir, showWarnings = FALSE)
    addResourcePath(pdf_prefix, pdf_dir)
    session$onSessionEnded(function() {
      removeResourcePath(pdf_prefix)
      unlink(pdf_dir, recursive = TRUE)
    })

    pdf_url <- reactive({
      req(is_pdf())
      unlink(list.files(pdf_dir, full.names = TRUE))
      target <- file.path(pdf_dir, "reference.pdf")
      file.copy(input$img_file$datapath, target, overwrite = TRUE)
      # Cache-bust so a new upload with the same served name is refetched.
      paste0(pdf_prefix, "/reference.pdf?v=", as.integer(Sys.time()))
    })

    observeEvent(input$img_file, {
      image_meta(list(
        file_name = input$img_file$name,
        loaded_at = Sys.time()
      ))
      # A new file is a new timing unit: any pause state belongs to the old one.
      clock$paused_at   <- as.POSIXct(NA)
      clock$paused_secs <- 0
      updateActionButton(session, "toggle_clock", label = "Pause clock", icon = icon("pause"))
      shinyjs::toggle("image_controls", condition = !is_pdf())
    })

    observeEvent(input$toggle_clock, {
      if (is.na(image_meta()$loaded_at)) {
        showNotification("Load a reference file first; the clock starts then.", type = "message")
        return()
      }
      if (is.na(clock$paused_at)) {
        clock$paused_at <- Sys.time()
        updateActionButton(session, "toggle_clock", label = "Resume clock", icon = icon("play"))
      } else {
        clock$paused_secs <- clock$paused_secs +
          as.numeric(difftime(Sys.time(), clock$paused_at, units = "secs"))
        clock$paused_at <- as.POSIXct(NA)
        updateActionButton(session, "toggle_clock", label = "Pause clock", icon = icon("pause"))
      }
    })

    output$clock_status <- renderText({
      if (is.na(image_meta()$loaded_at)) return("")
      if (!is.na(clock$paused_at)) return("Paused")
      if (clock$paused_secs > 0) sprintf("Running (%.0fs paused)", clock$paused_secs) else "Running"
    })

    output$image_display <- renderUI({
      container_style <- paste(
        "width: 100%; height: 50vh; min-height: 320px; overflow: auto;",
        "border: 2px dashed #ccc; background-color: #f9f9f9;",
        "display: flex; align-items: center; justify-content: center;"
      )

      if (is.null(input$img_file)) {
        return(div(style = container_style, h5(class = "text-muted", "Upload a reference image or PDF in the sidebar.")))
      }

      if (is_pdf()) {
        # Taller than the image frame: a report page needs the height, and
        # the browser viewer handles zoom and scrolling internally.
        return(div(style = "width: 100%; height: 75vh; min-height: 400px; border: 1px solid #ccc;",
                   tags$iframe(src = pdf_url(), style = "width: 100%; height: 100%; border: none;",
                               title = input$img_file$name)))
      }

      # The image is inlined as a data URI rather than served from www/, so
      # uploads never touch disk beyond Shiny's temp file.
      base64 <- base64enc::dataURI(file = input$img_file$datapath, mime = input$img_file$type)

      # Alignment is done purely in CSS. The underlying pixels are untouched,
      # so rotation/zoom cost nothing and are fully reversible. The translate
      # comes from the --tx/--ty variables that the pan script sets; they
      # default to 0 so a fresh image is centred.
      transform_style <- paste0(
        "transform: translate(var(--tx, 0px), var(--ty, 0px)) ",
        "rotate(", input$img_rotate, "deg) scale(", input$img_zoom, ");",
        " transition: transform 0.1s ease; max-width: 90%; cursor: grab; user-select: none;"
      )

      div(style = container_style,
          tags$img(src = base64, id = "ref_image", style = transform_style,
                   draggable = "false", `data-file` = input$img_file$name)
      )
    })

    return(reactive({
      c(image_meta(), list(paused_at = clock$paused_at, paused_secs = clock$paused_secs))
    }))
  })
}
