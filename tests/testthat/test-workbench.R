# tests/testthat/test-workbench.R
# Ledger construction in both entry modes, provenance stamping, and the
# validation gate that stands between operator input and the ledger.

LEDGER_COLS <- c("Timestamp", "Project", "Source", "Date_Ref", "Epi_Week",
                 "Region_ID", "Region_Name", "Variable", "Value", "Entry_Mode",
                 "Image_File", "Secs_Since_Image_Load")

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
