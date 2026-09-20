# R/mod_region_list.R
# ------------------------------------------------------------------------------
# MODULE: Region List
#
# A tabular alternative to the map for choosing regions. When the source is
# itself a table (the NCDC per-state table, say), the operator already has the
# region's name and locating its polygon is a wasted step. This view lists the
# loaded target layer by name, searchable, and a row click does exactly what a
# polygon click does. It also shows how many variables have been recorded for
# each region against the reference image currently loaded, so the operator
# can see at a glance which rows of the source are done.
#
# CONTRACT
#   Returns list(click, selected), the same shape as mod_map_engine, so app.R
#   can hand whichever view is active to the workbench unchanged.
#     click     list(id, nonce) on each row click; nonce makes repeat clicks
#               on the same row distinguishable
#     selected  character vector of layerIds of the currently selected rows
#               (multi-select, used by the batch entry mode)
# ------------------------------------------------------------------------------

region_list_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "text-muted", style = "font-size: 12px; margin-bottom: 6px;",
        "Click a row to enter its values (form mode) or select several rows and use ",
        "Add Data to Ledger (batch mode). Type in the search box to filter by name."),
    radioButtons(ns("order_mode"),
                 hint_label("Order:", title = "Row order",
                            tags$p(tags$b("A to Z:"), " alphabetical by name."),
                            tags$p(class = "mb-0", tags$b("Recently entered first:"), " regions in the order ",
                                   "you entered them for earlier files, most recent at the top, ",
                                   "then the rest alphabetically. Entries for the current file do ",
                                   "not move rows, so the order stays put while you work through a report.")),
                 choices = c("A to Z" = "alpha", "Recently entered first" = "recent"),
                 selected = "alpha", inline = TRUE),
    DT::DTOutput(ns("tbl"))
  )
}

# region_list_server(id, geom_data, entry_mode, ledger, image)
#   geom_data   reactive sf with layerId and NAME_* columns (the target layer)
#   entry_mode  reactive "single" | "form"
#   ledger      reactive data.frame, the workbench ledger (for the Entered column)
#   image       reactive list with $file_name, the sidecar image (may be NULL)
region_list_server <- function(id, geom_data, entry_mode, ledger = NULL, image = NULL) {
  moduleServer(id, function(input, output, session) {

    clicked <- reactiveVal(NULL)

    # One row per region: deepest available name, parent names for context,
    # and the layerId that everything else joins on.
    regions <- reactive({
      g <- geom_data()
      if (is.null(g) || nrow(g) == 0) return(NULL)
      d <- sf::st_drop_geometry(g)
      name_cols <- grep("^NAME_\\d$", names(d), value = TRUE)
      name_cols <- name_cols[order(as.integer(sub("NAME_", "", name_cols)))]
      out <- data.frame(layerId = as.character(d$layerId), stringsAsFactors = FALSE)
      out$Region <- if (length(name_cols)) as.character(d[[tail(name_cols, 1)]]) else out$layerId
      parents <- head(name_cols, -1)
      parents <- parents[parents != "NAME_0"]
      if (length(parents)) {
        out$Within <- do.call(paste, c(lapply(rev(parents), function(cn) as.character(d[[cn]])), sep = " / "))
      }
      out <- out[order(out$Region), ]

      if (identical(input$order_mode, "recent")) {
        last <- last_entered()
        key  <- last[out$layerId]
        # Most recent first; never-entered regions (NA) keep alphabetical order at the bottom.
        out <- out[order(is.na(key), -as.numeric(key), out$Region), ]
      }
      out
    })

    # Most recent commit time per region across files OTHER than the current
    # one. Excluding the current file keeps the order stable during a report:
    # a row does not jump to the top the moment it is entered.
    last_entered <- reactive({
      led <- if (is.function(ledger)) ledger() else NULL
      if (is.null(led) || nrow(led) == 0 || !all(c("Timestamp", "Region_ID") %in% names(led))) {
        return(setNames(as.POSIXct(character(0)), character(0)))
      }
      img <- if (is.function(image)) image()$file_name else NA_character_
      if (!is.na(img) && "Image_File" %in% names(led)) led <- led[!(led$Image_File %in% img), , drop = FALSE]
      if (nrow(led) == 0) return(setNames(as.POSIXct(character(0)), character(0)))
      ts <- as.POSIXct(led$Timestamp, format = "%Y-%m-%dT%H:%M:%S%z")
      agg <- tapply(ts, led$Region_ID, max)
      setNames(as.POSIXct(agg, origin = "1970-01-01"), names(agg))
    })

    # Variables recorded for each region against the current image.
    entered <- reactive({
      r <- regions(); req(r)
      n <- integer(nrow(r))
      led <- if (is.function(ledger)) ledger() else NULL
      img <- if (is.function(image)) image()$file_name else NA_character_
      if (!is.null(led) && nrow(led) > 0 && !is.na(img) && "Image_File" %in% names(led)) {
        sub <- led[led$Image_File %in% img, , drop = FALSE]
        counts <- table(sub$Region_ID)
        hit <- match(r$layerId, names(counts))
        n[!is.na(hit)] <- as.integer(counts[hit[!is.na(hit)]])
      }
      n
    })

    output$tbl <- DT::renderDT({
      r <- regions()
      if (is.null(r)) {
        return(DT::datatable(data.frame(Region = "Load and visualise a country first."),
                             rownames = FALSE, selection = "none",
                             options = list(dom = "t", ordering = FALSE)))
      }
      show <- r
      show$Entered <- ifelse(entered() > 0, paste0("✓ ", entered()), "")
      show <- show[, setdiff(names(show), "layerId"), drop = FALSE]

      DT::datatable(
        show,
        rownames  = FALSE,
        selection = "multiple",
        # All rows, no paging: the enclosing frame scrolls, so every region is
        # reachable by scrolling or by the search box without changing page.
        # No client-side initial sort: the data frame arrives in the order
        # chosen above, and row indices must map back to it.
        options   = list(paging = FALSE, dom = "ft", order = list(), scrollX = TRUE)
      )
    }, server = FALSE)

    proxy <- DT::dataTableProxy("tbl")

    # In form mode a row click is a request to open the form for that region,
    # so the selection is cleared straight away: the row must be clickable
    # again (to correct an entry) and must not accumulate into a batch.
    # In batch mode the selection is the batch and is left alone.
    observeEvent(input$tbl_rows_selected, {
      r <- regions(); req(r)
      if (identical(entry_mode(), "form")) {
        rows <- input$tbl_rows_selected
        if (length(rows) == 0) return()
        clicked(list(id = r$layerId[tail(rows, 1)], nonce = as.numeric(Sys.time())))
        DT::selectRows(proxy, NULL)
      }
    }, ignoreNULL = TRUE)

    # A batch selection belongs to the document it was made against, so a new
    # source document clears it, matching the map. The sidecar's reactive also
    # changes when the clock is paused, so the file name is compared against
    # the last one seen rather than being trusted to have changed.
    last_source_file <- reactiveVal(NA_character_)
    if (is.function(image)) {
      observeEvent(image(), {
        fn   <- image()$file_name
        prev <- isolate(last_source_file())
        if (identical(fn, prev)) return()
        last_source_file(fn)
        if (is.na(prev)) return()
        if (length(isolate(input$tbl_rows_selected)) > 0) DT::selectRows(proxy, NULL)
      }, ignoreNULL = TRUE)
    }

    selected <- reactive({
      r <- regions()
      if (is.null(r) || identical(entry_mode(), "form")) return(character(0))
      r$layerId[input$tbl_rows_selected]
    })

    list(click = clicked, selected = selected)
  })
}
