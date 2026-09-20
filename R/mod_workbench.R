# R/mod_workbench.R
# ------------------------------------------------------------------------------
# MODULE: Workbench (Data Ledger)
#
# Holds every extracted record for the session. The ledger is deliberately
# decoupled from the map view: clearing or reloading geometry does not touch
# these rows, which is what allows a single session to span many source images,
# administrative levels, or countries.
#
# ENTRY MODES (selected in mod_controls)
#   single  One variable, painted onto a batch of selected regions at once.
#           Suited to a choropleth: select every region in a colour band, commit.
#   form    A schema of variables entered per region via a validated form,
#           opened by clicking a region. Suited to a tabulated source where
#           each region carries several different values.
#   nil     The same form with no region attached, for a document that was
#           reviewed and reports nothing for any region. Recording that
#           against an arbitrary region would be true but would leave the
#           fact that it stands for the whole country implicit.
#
# Ledger schema (one row per region per variable):
#   Timestamp             ISO 8601 datetime the row was committed
#   Project, Source       operator-supplied metadata
#   Date_Ref, Epi_Week    reference date (or period start) the value describes
#   Date_End              period end, NA for a single-date report
#   Region_ID             GADM GID at the target level; the join key.
#                         NA for a document-level row (Entry_Mode "nil")
#   Region_Name           resolved administrative name (deepest available)
#   Variable, Value       what was recorded
#   Entry_Mode            "single", "form" or "nil" - how it was captured
#   Image_File            reference image on screen at commit time (provenance)
#   Secs_Since_Image_Load active time since that image was loaded, net of any
#                         clock pauses in the sidecar (efficiency)
#   Secs_Paused           total paused seconds at commit; add to the above to
#                         recover raw elapsed time
# ------------------------------------------------------------------------------

workbench_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "display: flex; gap: 10px; margin-bottom: 10px; align-items: center;",
        downloadButton(ns("download_csv"), "Download CSV", class = "btn-secondary btn-sm"),
        actionButton(ns("delete_rows"), "Delete Selected", icon = icon("trash"), class = "btn-danger btn-sm"),
        actionButton(ns("apply_meta"), "Apply Metadata", icon = icon("tags"), class = "btn-outline-secondary btn-sm",
                     title = paste("Stamp the current project name, source, dates and epi week onto the",
                                   "selected rows, or onto every row for the current source document",
                                   "if none are selected.")),
        div(style = "margin-left: auto;", textOutput(ns("quality_msg")))
    ),
    DT::DTOutput(ns("ledger_table"))
  )
}

# `sidecar_source` is optional so the module can be driven in isolation (e.g.
# from a test harness) without a reference image present.
workbench_server <- function(id, map_source, controls_output, loaded_state, sidecar_source = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    project_data <- reactiveVal(data.frame())

    # Counted, not merely prevented: the number of commits blocked by
    # validation is the evidence behind any claim that typed entry reduces
    # transcription error.
    rejections <- reactiveVal(0L)

    # Optional in the controls contract, so older callers and test mocks that
    # omit it behave as "blank is refused".
    blank_zero_on <- function() {
      is.function(controls_output$blank_zero) && isTRUE(controls_output$blank_zero())
    }

    # Region awaiting entry in form mode: list(id, name).
    pending_region <- reactiveVal(NULL)
    form_errors    <- reactiveVal(character(0))

    # --- HELPER: resolve layerIds to administrative names -----------------
    # Build the id -> name lookup ONCE per commit rather than subsetting the
    # sf object inside a per-id loop. Dropping geometry first matters: at
    # Admin 2 a national layer carries hundreds of thousands of vertices, and
    # repeatedly slicing that was the dominant cost of a large batch commit.
    resolve_region_names <- function(ids, current_map) {
      if (is.null(current_map) || nrow(current_map) == 0) {
        return(rep("Unknown", length(ids)))
      }

      attrs <- sf::st_drop_geometry(current_map)

      name_col <- NULL
      for (candidate in c("NAME_3", "NAME_2", "NAME_1", "NAME_0")) {
        if (candidate %in% names(attrs)) { name_col <- candidate; break }
      }
      if (is.null(name_col)) return(rep("Unknown", length(ids)))

      out <- attrs[[name_col]][match(ids, attrs$layerId)]
      out[is.na(out)] <- "Unknown"
      as.character(out)
    }

    # --- HELPER: image provenance ----------------------------------------
    # Active seconds = elapsed since load, minus completed pauses, minus the
    # pause in progress (if any). A sidecar that predates the clock, or a test
    # mock, may omit the pause fields; they are treated as "never paused".
    current_image <- function() {
      img <- if (is.function(sidecar_source)) sidecar_source() else NULL
      if (is.null(img) || is.na(img$loaded_at)) {
        return(list(file = if (!is.null(img)) img$file_name else NA_character_,
                    secs = NA_real_, paused = NA_real_))
      }
      now     <- Sys.time()
      elapsed <- as.numeric(difftime(now, img$loaded_at, units = "secs"))
      paused  <- if (!is.null(img$paused_secs)) img$paused_secs else 0
      if (!is.null(img$paused_at) && !is.na(img$paused_at)) {
        paused <- paused + as.numeric(difftime(now, img$paused_at, units = "secs"))
      }
      list(
        file   = img$file_name,
        secs   = round(elapsed - paused, 1),
        paused = round(paused, 1)
      )
    }

    # End of the reference period, or NA when the operator left it blank
    # (a single-date report). Older callers without the end fields get NA.
    period_end <- function(meta) {
      parts <- c(meta$end_year, meta$end_month, meta$end_day)
      if (length(parts) < 3 || all(is.null(parts)) || !any(nzchar(trimws(parts)))) return(NA_character_)
      paste(parts, collapse = "-")
    }

    # --- HELPER: assemble ledger rows -------------------------------------
    # `ids`, `variables` and `values` are recycled against each other, so this
    # serves both modes: one variable across many regions, and many variables
    # for one region.
    build_rows <- function(ids, names, variables, values, mode) {
      meta <- controls_output$metadata()
      img  <- current_image()

      data.frame(
        Timestamp             = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        Project               = meta$project,
        Source                = meta$source,
        Date_Ref              = paste(meta$year, meta$month, meta$day, sep = "-"),
        Date_End              = period_end(meta),
        Epi_Week              = if (is.null(meta$week) || is.na(meta$week)) NA_integer_ else as.integer(meta$week),
        Region_ID             = ids,
        Region_Name           = names,
        Variable              = variables,
        Value                 = as.character(values),
        Entry_Mode            = mode,
        Image_File            = img$file,
        Secs_Since_Image_Load = img$secs,
        Secs_Paused           = img$paused,
        stringsAsFactors      = FALSE
      )
    }

    append_rows <- function(df) {
      # bind_rows tolerates schema drift, so ledgers saved by older versions of
      # the app load without migration.
      project_data(dplyr::bind_rows(project_data(), df))
    }

    # --- 1. SINGLE-VARIABLE BATCH COMMIT ----------------------------------
    observeEvent(controls_output$add_trigger(), {
      if (identical(controls_output$entry_mode(), "form")) {
        showNotification("Form mode: click a region on the map to enter data.", type = "message")
        return()
      }

      if (!is.list(map_source) || is.null(map_source$selected)) return()
      ids <- map_source$selected()

      if (length(ids) == 0) {
        showNotification("No regions selected!", type = "warning")
        return()
      }

      meta <- controls_output$metadata()
      append_rows(build_rows(
        ids       = ids,
        names     = resolve_region_names(ids, controls_output$geom_data()),
        variables = meta$var_name,
        values    = meta$var_value,
        mode      = "single"
      ))

      showNotification(paste("Added", length(ids), "rows to ledger."), type = "message")
    })

    # --- HELPER: existing entries for a region against the current file ---
    # A region's values are keyed by (Region_ID, Image_File): the same state
    # legitimately appears once per report. Rows with no image match only
    # rows with no image. Returns a logical index into the ledger.
    existing_rows <- function(id) {
      d <- project_data()
      if (nrow(d) == 0) return(logical(0))
      img <- current_image()$file
      same_img <- if (is.na(img)) is.na(d$Image_File) else !is.na(d$Image_File) & d$Image_File == img
      # NA-safe: a document-level row (nothing reported for any region) has no
      # Region_ID, and `NA == NA` would make it uneditable.
      same_region <- if (is.na(id)) is.na(d$Region_ID) else !is.na(d$Region_ID) & d$Region_ID == id
      same_region & same_img
    }

    # --- 2. THE ENTRY FORM -------------------------------------------------
    # Opening a form for something that already has values for the current file
    # pre-fills it, and submitting replaces those rows. Without this a
    # correction would append a second set and leave the ledger ambiguous.
    #
    # The field set is the declared schema in form mode, or the single batch
    # variable in paint mode, so a document-level entry means the same thing in
    # both without exposing the schema builder where it is not otherwise used.
    entry_schema <- function() {
      if (identical(controls_output$entry_mode(), "form")) return(controls_output$schema())
      nm <- controls_output$metadata()$var_name
      nm <- if (is.null(nm)) "" else trimws(nm)
      if (!nzchar(nm)) return(parse_schema_text(""))
      # numeric accepts 0 and NA, which is all a nil return needs; the batch
      # variable's own type governs values entered by painting.
      parse_schema_text(paste(nm, "numeric", sep = ", "))
    }

    # `region_id` is NA for a document-level entry: a report that enumerates a
    # variable and reports nothing for any region states something true, but it
    # belongs to the document rather than to any one region.
    open_entry_form <- function(region_id, region_name, mode = "form",
                                default_numeric = "") {
      sch <- entry_schema()
      if (nrow(sch) == 0 || any(nzchar(sch$error))) {
        showNotification("Fix the variable schema before entering data.", type = "error")
        return(invisible(FALSE))
      }

      form_errors(character(0))

      prior   <- project_data()[existing_rows(region_id), , drop = FALSE]
      current <- setNames(as.character(prior$Value), prior$Variable)
      editing <- length(current) > 0
      pending_region(list(id = region_id, name = region_name,
                          editing = editing, mode = mode))

      prefill <- function(v, type) {
        if (v %in% names(current)) return(if (is.na(current[[v]])) "NA" else current[[v]])
        if (nzchar(default_numeric) && type %in% c("count", "numeric", "binary")) {
          return(default_numeric)
        }
        ""
      }

      showModal(modalDialog(
        title = tagList(icon(if (editing) "pen" else "pen-to-square"), " ", region_name,
                        if (editing) span(class = "badge bg-warning text-dark ms-2", "editing")),
        size = "s",
        easyClose = FALSE,

        div(class = "text-muted", style = "font-size: 11px; margin-bottom: 10px;",
            if (is.na(region_id)) "Recorded against the document, not against any region."
            else region_id,
            if (editing) tagList(br(), "Existing values shown; submitting replaces them.")),

        # Numeric fields use textInput rather than numericInput so that invalid
        # entries survive to validation and can be reported, instead of being
        # silently blanked by the browser.
        lapply(seq_len(nrow(sch)), function(i) {
          v <- sch[i, ]
          if (v$type == "ordinal") {
            lv  <- trimws(strsplit(v$levels, "|", fixed = TRUE)[[1]])
            sel <- prefill(v$name, v$type)
            selectInput(ns(paste0("fld_", v$name)), v$name, choices = lv,
                        selected = if (sel %in% lv) sel else NULL)
          } else {
            is_num    <- v$type %in% c("count", "numeric", "binary")
            zero_hint <- blank_zero_on() && is_num
            textInput(ns(paste0("fld_", v$name)),
                      label = paste0(v$name, " (", v$type,
                                     if (zero_hint) ", blank = 0",
                                     if (is_num) ", NA = not reported", ")"),
                      value = prefill(v$name, v$type),
                      placeholder = if (zero_hint) "0" else "")
          }
        }),

        uiOutput(ns("form_error_ui")),

        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("form_submit"), if (editing) "Update Ledger" else "Add to Ledger",
                       class = "btn-success", icon = icon(if (editing) "check" else "plus"))
        )
      ))
      invisible(TRUE)
    }

    observeEvent(map_source$click(), {
      req(identical(controls_output$entry_mode(), "form"))
      click <- map_source$click()
      req(!is.null(click$id))
      open_entry_form(click$id,
                      resolve_region_names(click$id, controls_output$geom_data())[1])
    })

    # Available in both entry modes: whether the document is read as a map or
    # as a table, "this report enumerates these variables and reports nothing"
    # is the same statement, and it has no region to attach to.
    # Optional in the controls contract, like blank_zero, so callers and test
    # mocks that predate it keep working.
    if (is.function(controls_output$nil_trigger)) {
      observeEvent(controls_output$nil_trigger(), {
        if (is.na(current_image()$file)) {
          showNotification("Load the source document first, so the entry is attributed to it.",
                           type = "warning")
          return()
        }
        open_entry_form(NA_character_, "Nothing reported", mode = "nil", default_numeric = "0")
      }, ignoreInit = TRUE)
    }

    output$form_error_ui <- renderUI({
      errs <- form_errors()
      if (length(errs) == 0) return(NULL)
      div(class = "text-danger", style = "font-size: 12px; margin-top: 8px;",
          icon("triangle-exclamation"),
          tags$ul(style = "padding-left: 18px; margin-bottom: 0;", lapply(errs, tags$li)))
    })

    # --- 3. FORM MODE: VALIDATE AND COMMIT --------------------------------
    observeEvent(input$form_submit, {
      region <- pending_region()
      req(region)

      # Same field set the form was built from, so a paint-mode nil return
      # validates against the batch variable rather than the hidden schema.
      sch   <- entry_schema()
      rules <- controls_output$rules()

      # Per-field type validation.
      values <- character(0)
      errs   <- character(0)
      for (i in seq_len(nrow(sch))) {
        v   <- sch[i, ]
        res <- validate_value(input[[paste0("fld_", v$name)]], v$type, v$levels,
                              blank_zero = blank_zero_on())
        if (res$ok) {
          values[[v$name]] <- res$value
        } else {
          errs <- c(errs, paste0(v$name, " ", res$msg))
        }
      }

      # Cross-field rules only run once every field is individually valid,
      # otherwise the operator gets a cascade of consequential errors.
      if (length(errs) == 0) {
        violated <- check_rules(values, rules, sch)
        if (length(violated) > 0) {
          errs <- c(errs, paste0("violates rule: ", violated))
        }
      }

      if (length(errs) > 0) {
        rejections(rejections() + 1L)
        form_errors(errs)
        return()
      }

      # Replace, not append: drop this region's prior rows for the current
      # file for the variables being submitted. Rows for variables no longer
      # in the schema are left alone.
      drop <- existing_rows(region$id)
      if (any(drop)) {
        d <- project_data()
        drop <- drop & d$Variable %in% names(values)
        if (any(drop)) project_data(d[!drop, , drop = FALSE])
      }

      append_rows(build_rows(
        ids       = region$id,
        names     = region$name,
        variables = names(values),
        values    = unlist(values, use.names = FALSE),
        # "form" for a region, "nil" for a document-level entry, so rows
        # stating "nothing was reported" are identifiable in the export.
        mode      = if (is.null(region$mode)) "form" else region$mode
      ))

      removeModal()
      pending_region(NULL)
      showNotification(paste0(if (isTRUE(region$editing)) "Updated " else "Added ",
                              length(values), " values for ", region$name, "."), type = "message")
    })

    # --- 4. LOAD STATE ----------------------------------------------------
    observe({
      req(loaded_state())
      if (nrow(loaded_state()) > 0) project_data(loaded_state())
    })

    # --- 4b. APPLY METADATA -----------------------------------------------
    # Metadata is stamped at commit, so rows entered before the panel was
    # filled in carry blanks. This re-stamps them from the panel: selected
    # rows if any, otherwise every row for the current source document.
    observeEvent(input$apply_meta, {
      d <- project_data()
      if (nrow(d) == 0) return()

      sel <- input$ledger_table_rows_selected
      idx <- if (length(sel) > 0) sel else {
        img <- current_image()$file
        if (is.na(img)) integer(0) else which(!is.na(d$Image_File) & d$Image_File == img)
      }
      if (length(idx) == 0) {
        showNotification("Select rows, or load the source document whose rows should be updated.",
                         type = "warning")
        return()
      }

      meta <- controls_output$metadata()
      d$Project[idx]  <- meta$project
      d$Source[idx]   <- meta$source
      d$Date_Ref[idx] <- paste(meta$year, meta$month, meta$day, sep = "-")
      if (!"Date_End" %in% names(d)) d$Date_End <- NA_character_
      d$Date_End[idx] <- period_end(meta)
      d$Epi_Week[idx] <- if (is.null(meta$week) || is.na(meta$week)) NA_integer_ else as.integer(meta$week)
      project_data(d)

      showNotification(paste("Metadata applied to", length(idx), "rows."), type = "message")
    })

    # --- 5. DELETE --------------------------------------------------------
    observeEvent(input$delete_rows, {
      req(input$ledger_table_rows_selected)
      current <- project_data()
      if (nrow(current) > 0) {
        project_data(current[-input$ledger_table_rows_selected, ])
      }
    })

    # --- 6. RENDER --------------------------------------------------------
    output$ledger_table <- DT::renderDT({
      req(project_data())
      DT::datatable(project_data(),
                    # stateSave keeps sort order, page length and page across
                    # re-renders; the table is rebuilt on every ledger change.
                    options = list(pageLength = 5, scrollX = TRUE, stateSave = TRUE),
                    # Per-column filter boxes: checking one document or one
                    # region across weeks is the normal correction workflow.
                    filter = "top",
                    rownames = FALSE,
                    selection = "multiple")
    })

    # Surfaces the two numbers a methods section needs: how much was extracted,
    # and how much bad input was stopped at the point of entry.
    output$quality_msg <- renderText({
      n <- nrow(project_data())
      r <- rejections()
      if (n == 0 && r == 0) return("")
      paste0(n, " rows | ", r, " entries rejected by validation")
    })

    # --- 7. DOWNLOAD ------------------------------------------------------
    output$download_csv <- downloadHandler(
      filename = function() { paste0("map_liberator_", Sys.Date(), ".csv") },
      content  = function(file) { write.csv(project_data(), file, row.names = FALSE) }
    )

    return(project_data)
  })
}
