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
#
# Ledger schema (one row per region per variable):
#   Timestamp             ISO 8601 datetime the row was committed
#   Project, Source       operator-supplied metadata
#   Date_Ref, Epi_Week    reference period the value describes
#   Region_ID             GADM GID at the target level; the join key
#   Region_Name           resolved administrative name (deepest available)
#   Variable, Value       what was recorded
#   Entry_Mode            "single" or "form" - how the value was captured
#   Image_File            reference image on screen at commit time (provenance)
#   Secs_Since_Image_Load elapsed time since that image was loaded (efficiency)
# ------------------------------------------------------------------------------

workbench_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "display: flex; gap: 10px; margin-bottom: 10px; align-items: center;",
        downloadButton(ns("download_csv"), "Download CSV", class = "btn-secondary btn-sm"),
        actionButton(ns("delete_rows"), "Delete Selected", icon = icon("trash"), class = "btn-danger btn-sm"),
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
    current_image <- function() {
      img <- if (is.function(sidecar_source)) sidecar_source() else NULL
      list(
        file = if (!is.null(img)) img$file_name else NA_character_,
        secs = if (!is.null(img) && !is.na(img$loaded_at)) {
          round(as.numeric(difftime(Sys.time(), img$loaded_at, units = "secs")), 1)
        } else NA_real_
      )
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
        Epi_Week              = if (is.null(meta$week) || is.na(meta$week)) NA_integer_ else as.integer(meta$week),
        Region_ID             = ids,
        Region_Name           = names,
        Variable              = variables,
        Value                 = as.character(values),
        Entry_Mode            = mode,
        Image_File            = img$file,
        Secs_Since_Image_Load = img$secs,
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

    # --- 2. FORM MODE: OPEN ON REGION CLICK -------------------------------
    observeEvent(map_source$click(), {
      req(identical(controls_output$entry_mode(), "form"))

      click <- map_source$click()
      req(!is.null(click$id))

      sch <- controls_output$schema()
      if (nrow(sch) == 0 || any(nzchar(sch$error))) {
        showNotification("Fix the variable schema before entering data.", type = "error")
        return()
      }

      region_name <- resolve_region_names(click$id, controls_output$geom_data())[1]
      pending_region(list(id = click$id, name = region_name))
      form_errors(character(0))

      showModal(modalDialog(
        title = tagList(icon("pen-to-square"), " ", region_name),
        size = "s",
        easyClose = FALSE,

        div(class = "text-muted", style = "font-size: 11px; margin-bottom: 10px;", click$id),

        # Numeric fields use textInput rather than numericInput so that invalid
        # entries survive to validation and can be reported, instead of being
        # silently blanked by the browser.
        lapply(seq_len(nrow(sch)), function(i) {
          v <- sch[i, ]
          if (v$type == "ordinal") {
            selectInput(ns(paste0("fld_", v$name)), v$name,
                        choices = trimws(strsplit(v$levels, "|", fixed = TRUE)[[1]]))
          } else {
            textInput(ns(paste0("fld_", v$name)),
                      label = paste0(v$name, " (", v$type, ")"), value = "")
          }
        }),

        uiOutput(ns("form_error_ui")),

        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("form_submit"), "Add to Ledger", class = "btn-success", icon = icon("plus"))
        )
      ))
    })

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

      sch   <- controls_output$schema()
      rules <- controls_output$rules()

      # Per-field type validation.
      values <- character(0)
      errs   <- character(0)
      for (i in seq_len(nrow(sch))) {
        v   <- sch[i, ]
        res <- validate_value(input[[paste0("fld_", v$name)]], v$type, v$levels)
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

      append_rows(build_rows(
        ids       = region$id,
        names     = region$name,
        variables = names(values),
        values    = unlist(values, use.names = FALSE),
        mode      = "form"
      ))

      removeModal()
      pending_region(NULL)
      showNotification(paste0("Added ", length(values), " values for ", region$name, "."), type = "message")
    })

    # --- 4. LOAD STATE ----------------------------------------------------
    observe({
      req(loaded_state())
      if (nrow(loaded_state()) > 0) project_data(loaded_state())
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
                    options = list(pageLength = 5, scrollX = TRUE),
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
