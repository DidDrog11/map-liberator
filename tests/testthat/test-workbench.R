# tests/testthat/test-workbench.R
# Ledger construction in both entry modes, provenance stamping, and the
# validation gate that stands between operator input and the ledger.

LEDGER_COLS <- c("Timestamp", "Project", "Source", "Date_Ref", "Date_End", "Epi_Week",
                 "Region_ID", "Region_Name", "Variable", "Value", "Entry_Mode",
                 "Image_File", "Secs_Since_Image_Load", "Secs_Paused")

test_that("batch mode writes one row per selected region", {
  add <- reactiveVal(0)

  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(c("NGA.28_1", "NGA.12_1")),
                           click    = reactive(NULL)),
    controls_output = mock_controls("single", add_trigger = reactive(add())),
    loaded_state    = reactive(NULL),
    sidecar_source  = mock_sidecar("wk52.png", seconds_ago = 42)
  ), {
    add(1); session$flushReact()
    d <- session$returned()

    expect_equal(nrow(d), 2)
    expect_setequal(names(d), LEDGER_COLS)
    expect_equal(d$Region_ID, c("NGA.28_1", "NGA.12_1"))
    expect_equal(d$Region_Name, c("Ondo", "Edo"))       # resolved from geometry
    expect_true(all(d$Variable == "cases_detected"))
    expect_true(all(d$Entry_Mode == "single"))
  })
})

test_that("committed rows carry epi week and image provenance", {
  add <- reactiveVal(0)

  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive("NGA.6_1"), click = reactive(NULL)),
    controls_output = mock_controls("single", add_trigger = reactive(add())),
    loaded_state    = reactive(NULL),
    sidecar_source  = mock_sidecar("epi_week_52.png", seconds_ago = 90)
  ), {
    add(1); session$flushReact()
    d <- session$returned()

    expect_equal(d$Epi_Week, 52L)
    expect_equal(d$Image_File, "epi_week_52.png")
    # Elapsed time is measured, not merely recorded as present.
    expect_gte(d$Secs_Since_Image_Load, 89)
    expect_lt(d$Secs_Since_Image_Load, 120)
    # ISO 8601 with a UTC offset, so ordering survives across sessions.
    expect_match(d$Timestamp, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}[+-]\\d{4}$")
  })
})

test_that("clock pauses are subtracted from active time and recorded", {
  add <- reactiveVal(0)

  # 90 s since load, 30 s of completed pauses, and a pause still running for 10 s.
  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive("NGA.6_1"), click = reactive(NULL)),
    controls_output = mock_controls("single", add_trigger = reactive(add())),
    loaded_state    = reactive(NULL),
    sidecar_source  = mock_sidecar("wk52.png", seconds_ago = 90,
                                   paused_secs = 30, paused_for = 10)
  ), {
    add(1); session$flushReact()
    d <- session$returned()

    expect_gte(d$Secs_Since_Image_Load, 49)
    expect_lt(d$Secs_Since_Image_Load, 60)
    expect_gte(d$Secs_Paused, 40)
    expect_lt(d$Secs_Paused, 50)
  })

  # A sidecar without pause fields (older code, simpler mocks) is "never paused".
  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive("NGA.6_1"), click = reactive(NULL)),
    controls_output = mock_controls("single", add_trigger = reactive(add())),
    loaded_state    = reactive(NULL),
    sidecar_source  = reactive(list(file_name = "old.png", loaded_at = Sys.time() - 20))
  ), {
    add(1); session$flushReact()
    d <- session$returned()
    expect_gte(d$Secs_Since_Image_Load, 19)
    expect_equal(d$Secs_Paused, 0)
  })
})

test_that("an empty selection commits nothing", {
  add <- reactiveVal(0)

  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(character(0)), click = reactive(NULL)),
    controls_output = mock_controls("single", add_trigger = reactive(add())),
    loaded_state    = reactive(NULL),
    sidecar_source  = NULL
  ), {
    add(1); session$flushReact()
    expect_equal(nrow(session$returned()), 0)
  })
})

test_that("regions absent from the layer resolve to Unknown rather than erroring", {
  add <- reactiveVal(0)

  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive("NOT.A.REAL.ID"), click = reactive(NULL)),
    controls_output = mock_controls("single", add_trigger = reactive(add())),
    loaded_state    = reactive(NULL),
    sidecar_source  = NULL
  ), {
    add(1); session$flushReact()
    expect_equal(session$returned()$Region_Name, "Unknown")
  })
})

# --- FORM MODE ----------------------------------------------------------------

form_controls <- function() {
  sch <- parse_schema_text("suspected, count\nconfirmed, count\ndeaths, count")
  mock_controls(
    entry_mode = "form",
    schema     = sch,
    rules      = parse_rules_text("confirmed <= suspected\ndeaths <= confirmed", sch)
  )
}

test_that("form mode writes one row per declared variable from a single click", {
  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(character(0)),
                           click    = reactive(list(id = "NGA.28_1"))),
    controls_output = form_controls(),
    loaded_state    = reactive(NULL),
    sidecar_source  = mock_sidecar()
  ), {
    session$flushReact()
    session$setInputs(fld_suspected = "101", fld_confirmed = "27", fld_deaths = "9")
    session$setInputs(form_submit = 1)

    d <- session$returned()
    expect_equal(nrow(d), 3)
    expect_equal(d$Variable, c("suspected", "confirmed", "deaths"))
    expect_equal(d$Value, c("101", "27", "9"))
    expect_true(all(d$Region_Name == "Ondo"))
    expect_true(all(d$Entry_Mode == "form"))
  })
})

test_that("type violations are blocked and counted", {
  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(character(0)),
                           click    = reactive(list(id = "NGA.12_1"))),
    controls_output = form_controls(),
    loaded_state    = reactive(NULL),
    sidecar_source  = NULL
  ), {
    session$flushReact()
    session$setInputs(fld_suspected = "twelve", fld_confirmed = "27", fld_deaths = "9")
    session$setInputs(form_submit = 1)

    expect_equal(nrow(session$returned()), 0)
    expect_match(output$quality_msg, "1 entries rejected")
  })
})

test_that("cross-field rule violations are blocked", {
  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(character(0)),
                           click    = reactive(list(id = "NGA.12_1"))),
    controls_output = form_controls(),
    loaded_state    = reactive(NULL),
    sidecar_source  = NULL
  ), {
    session$flushReact()
    # Individually valid counts, but confirmed exceeds suspected.
    session$setInputs(fld_suspected = "20", fld_confirmed = "27", fld_deaths = "9")
    session$setInputs(form_submit = 1)
    expect_equal(nrow(session$returned()), 0)

    # Corrected values commit, and the rejection tally is preserved.
    session$setInputs(fld_suspected = "50", fld_confirmed = "27", fld_deaths = "9")
    session$setInputs(form_submit = 2)
    expect_equal(nrow(session$returned()), 3)
    expect_match(output$quality_msg, "3 rows \\| 1 entries rejected")
  })
})

test_that("the batch commit button does nothing in form mode", {
  add <- reactiveVal(0)
  ctrl <- form_controls()
  ctrl$add_trigger <- reactive(add())

  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(c("NGA.28_1", "NGA.12_1")),
                           click    = reactive(NULL)),
    controls_output = ctrl,
    loaded_state    = reactive(NULL),
    sidecar_source  = NULL
  ), {
    add(1); session$flushReact()
    expect_equal(nrow(session$returned()), 0)
  })
})

# --- LOADING ------------------------------------------------------------------

test_that("a loaded ledger replaces the session ledger", {
  prior <- data.frame(Region_ID = "NGA.1_1", Variable = "x", Value = "1",
                      stringsAsFactors = FALSE)

  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(character(0)), click = reactive(NULL)),
    controls_output = mock_controls("single"),
    loaded_state    = reactive(prior),
    sidecar_source  = NULL
  ), {
    session$flushReact()
    expect_equal(nrow(session$returned()), 1)
    expect_equal(session$returned()$Region_ID, "NGA.1_1")
  })
})

test_that("blank numeric fields commit as 0 when the schema allows it, and are refused otherwise", {
  sch <- parse_schema_text("suspected, count\nconfirmed, count\ndeaths, count")

  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(character(0)),
                           click    = reactive(list(id = "NGA.28_1"))),
    controls_output = mock_controls("form", schema = sch, blank_zero = TRUE),
    loaded_state    = reactive(NULL),
    sidecar_source  = NULL
  ), {
    session$flushReact()
    session$setInputs(fld_suspected = "8", fld_confirmed = "", fld_deaths = "")
    session$setInputs(form_submit = 1)

    d <- session$returned()
    expect_equal(d$Value, c("8", "0", "0"))
    expect_match(output$quality_msg, "0 entries rejected")
  })

  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(character(0)),
                           click    = reactive(list(id = "NGA.28_1"))),
    controls_output = mock_controls("form", schema = sch, blank_zero = FALSE),
    loaded_state    = reactive(NULL),
    sidecar_source  = NULL
  ), {
    session$flushReact()
    session$setInputs(fld_suspected = "8", fld_confirmed = "", fld_deaths = "")
    session$setInputs(form_submit = 1)

    expect_equal(nrow(session$returned()), 0)
    expect_match(output$quality_msg, "1 entries rejected")
  })
})

test_that("NA typed into a numeric field is committed as missing, distinct from blank-as-zero", {
  sch <- parse_schema_text("suspected, count\nconfirmed, count\ndeaths, count")

  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(character(0)),
                           click    = reactive(list(id = "NGA.28_1"))),
    controls_output = mock_controls("form", schema = sch, blank_zero = TRUE),
    loaded_state    = reactive(NULL),
    sidecar_source  = NULL
  ), {
    session$flushReact()
    session$setInputs(fld_suspected = "40", fld_confirmed = "", fld_deaths = "NA")
    session$setInputs(form_submit = 1)

    d <- session$returned()
    expect_equal(d$Variable, c("suspected", "confirmed", "deaths"))
    expect_equal(d$Value[1:2], c("40", "0"))
    expect_true(is.na(d$Value[3]))
  })
})

test_that("re-entering a region for the same file replaces its rows instead of appending", {
  sch <- parse_schema_text("suspected, count\nconfirmed, count")
  click <- reactiveVal(NULL)

  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(character(0)), click = click),
    controls_output = mock_controls("form", schema = sch),
    loaded_state    = reactive(NULL),
    sidecar_source  = mock_sidecar("w01.pdf")
  ), {
    click(list(id = "NGA.28_1", nonce = 1)); session$flushReact()
    session$setInputs(fld_suspected = "10", fld_confirmed = "2")
    session$setInputs(form_submit = 1)
    expect_equal(nrow(session$returned()), 2)

    # Second click on the same region: the form is pre-filled from the ledger.
    click(list(id = "NGA.28_1", nonce = 2)); session$flushReact()
    expect_true(pending_region()$editing)
    expect_equal(unname(setNames(project_data()$Value, project_data()$Variable)["suspected"]), "10")

    session$setInputs(fld_suspected = "12", fld_confirmed = "2")
    session$setInputs(form_submit = 2)

    d <- session$returned()
    expect_equal(nrow(d), 2)
    expect_equal(d$Value[d$Variable == "suspected"], "12")

    # A different region is a fresh entry, not an edit.
    click(list(id = "NGA.12_1", nonce = 3)); session$flushReact()
    expect_false(pending_region()$editing)
  })
})

test_that("a period end is recorded when given and NA for a single-date report", {
  add <- reactiveVal(0)
  base <- list(map_source   = list(selected = reactive("NGA.6_1"), click = reactive(NULL)),
               loaded_state = reactive(NULL), sidecar_source = NULL)

  shiny::testServer(workbench_server, args = c(base, list(
    controls_output = mock_controls("single", add_trigger = reactive(add()))
  )), {
    add(1); session$flushReact()
    expect_equal(session$returned()$Date_Ref, "2025-12-28")
    expect_true(is.na(session$returned()$Date_End))
  })

  shiny::testServer(workbench_server, args = c(base, list(
    controls_output = mock_controls("single", add_trigger = reactive(add()),
                                    metadata = test_metadata(year = "2025", month = "12", day = "01",
                                                             end_year = "2025", end_month = "12", end_day = "31"))
  )), {
    add(1); session$flushReact()
    expect_equal(session$returned()$Date_Ref, "2025-12-01")
    expect_equal(session$returned()$Date_End, "2025-12-31")
  })
})

test_that("Apply Metadata re-stamps the current document's rows, or the selected rows", {
  add  <- reactiveVal(0)
  meta <- reactiveVal(test_metadata(source = "", week = NA))
  ctrl <- mock_controls("single", add_trigger = reactive(add()))
  ctrl$metadata <- meta

  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(c("NGA.28_1", "NGA.12_1")), click = reactive(NULL)),
    controls_output = ctrl,
    loaded_state    = reactive(NULL),
    sidecar_source  = mock_sidecar("w07.pdf")
  ), {
    add(1); session$flushReact()
    expect_equal(session$returned()$Source, c("", ""))
    expect_true(all(is.na(session$returned()$Epi_Week)))

    # Fill the panel in afterwards and apply to the document's rows.
    meta(test_metadata(source = "sitrep_w07", week = 7)); session$flushReact()
    session$setInputs(apply_meta = 1)
    d <- session$returned()
    expect_equal(d$Source, c("sitrep_w07", "sitrep_w07"))
    expect_equal(d$Epi_Week, c(7L, 7L))

    # With a selection, only the selected row changes.
    meta(test_metadata(source = "sitrep_w08", week = 8)); session$flushReact()
    session$setInputs(ledger_table_rows_selected = 2L)
    session$setInputs(apply_meta = 2)
    d <- session$returned()
    expect_equal(d$Source, c("sitrep_w07", "sitrep_w08"))
  })
})
