# R/mod_state_manager.R
# ------------------------------------------------------------------------------
# MODULE: State Manager
# PURPOSE: Save/load the whole project state - the ledger AND the extraction
#          schema that produced it - to/from a single .rds file.
#
# WHY THE SCHEMA IS SAVED
#   An extraction spanning 150+ reports runs across many sessions. If the
#   variable declarations and consistency rules have to be retyped each time,
#   they will drift, and rows committed in week 3 will not be directly
#   comparable to rows committed in week 40. Saving the schema alongside the
#   data makes a project resumable and the extraction reproducible.
#
# FILE FORMAT
#   v2 (current): a named list - format, version, saved_at, ledger, schema_text,
#                 rules_text.
#   v1 (legacy):  a bare data.frame containing only the ledger.
#   Loading detects which it is, so projects saved by earlier versions of the
#   app - including the published Lassa fever dataset - still open. Saving
#   always writes v2.
# ------------------------------------------------------------------------------

PROJECT_FORMAT  <- "map_liberator_project"
PROJECT_VERSION <- 2L

# --- FORMAT HELPERS (pure; unit-tested independently of Shiny) ---------------

# build_project_state(ledger, schema_text, rules_text)
#   Assemble the object written to disk.
build_project_state <- function(ledger, schema_text = "", rules_text = "") {
  list(
    format      = PROJECT_FORMAT,
    version     = PROJECT_VERSION,
    saved_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    ledger      = if (is.null(ledger)) data.frame() else as.data.frame(ledger),
    schema_text = if (is.null(schema_text)) "" else as.character(schema_text)[1],
    rules_text  = if (is.null(rules_text))  "" else as.character(rules_text)[1]
  )
}

# read_project_state(obj)
#   Normalise anything readRDS returns into the v2 shape.
#   Returns list(ledger, schema_text, rules_text, version, legacy) or throws.
read_project_state <- function(obj) {
  # v1: the file is just the ledger.
  if (is.data.frame(obj)) {
    return(list(ledger = obj, schema_text = "", rules_text = "",
                version = 1L, legacy = TRUE))
  }

  if (is.list(obj) && !is.null(obj$ledger) && is.data.frame(obj$ledger)) {
    return(list(
      ledger      = obj$ledger,
      schema_text = if (is.null(obj$schema_text)) "" else obj$schema_text,
      rules_text  = if (is.null(obj$rules_text))  "" else obj$rules_text,
      version     = if (is.null(obj$version)) NA_integer_ else as.integer(obj$version),
      legacy      = FALSE
    ))
  }

  stop("Not a Map Liberator project file.")
}

# --- UI -----------------------------------------------------------------------

state_manager_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Project Files"),
    div(style = "display:flex; gap:5px;",
        div(style = "flex-grow:1;",
            fileInput(ns("load_file"), NULL,
                      buttonLabel = "Load Project",
                      placeholder = "No project loaded",
                      accept = ".rds")
        ),
        div(style = "margin-top:0px;",
            downloadButton(ns("save_file"), "Save", class = "btn-info")
        )
    ),
    textOutput(ns("status_msg"))
  )
}

# --- SERVER -------------------------------------------------------------------

# state_manager_server(id, data_to_save, schema_to_save = NULL)
#   data_to_save   reactive returning the ledger data.frame
#   schema_to_save optional reactive returning list(schema_text, rules_text)
#
# Returns list(ledger = reactive, schema = reactive). `schema` yields NULL when
# the loaded file carried none, so consumers can leave their inputs untouched.
state_manager_server <- function(id, data_to_save, schema_to_save = NULL) {
  moduleServer(id, function(input, output, session) {

    # 1. SAVE
    output$save_file <- downloadHandler(
      filename = function() {
        paste0("MapLiberator_Project_", format(Sys.time(), "%Y%m%d_%H%M"), ".rds")
      },
      content = function(file) {
        sch <- if (is.function(schema_to_save)) schema_to_save() else NULL
        saveRDS(
          build_project_state(
            ledger      = data_to_save(),
            schema_text = if (is.null(sch)) "" else sch$schema_text,
            rules_text  = if (is.null(sch)) "" else sch$rules_text
          ),
          file
        )
      }
    )

    # 2. LOAD
    # Parsed once per upload; `ledger` and `schema` below are views onto it.
    loaded <- reactive({
      req(input$load_file)
      tryCatch({
        read_project_state(readRDS(input$load_file$datapath))
      }, error = function(e) {
        showNotification(paste("Could not load project:", conditionMessage(e)), type = "error")
        NULL
      })
    })

    loaded_ledger <- reactive({
      st <- loaded()
      if (is.null(st)) return(NULL)
      st$ledger
    })

    loaded_schema <- reactive({
      st <- loaded()
      if (is.null(st)) return(NULL)
      # Nothing to restore for a legacy file; returning NULL leaves the
      # operator's current schema in place rather than blanking it.
      if (!nzchar(st$schema_text) && !nzchar(st$rules_text)) return(NULL)
      list(schema_text = st$schema_text, rules_text = st$rules_text)
    })

    output$status_msg <- renderText({
      st <- loaded()
      if (is.null(st)) return("")
      paste0("Loaded: ", nrow(st$ledger), " rows",
             if (st$legacy) " (legacy format, no schema)"
             else if (nzchar(st$schema_text)) " + schema" else "")
    })

    list(ledger = loaded_ledger, schema = loaded_schema)
  })
}
