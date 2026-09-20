# tests/testthat/test-nil-return.R
# Recording that a document was reviewed and reports nothing for any region.

nil_controls <- function(entry_mode = "form", trigger = reactive(0), ...) {
  sch <- parse_schema_text("suspected, count\nconfirmed, count\ndeaths, count")
  mock_controls(entry_mode = entry_mode, schema = sch,
                rules = parse_rules_text("", sch),
                nil_trigger = trigger, ...)
}

test_that("a nil return records every declared variable against the document", {
  nil <- reactiveVal(0)

  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(character(0)), click = reactive(NULL)),
    controls_output = nil_controls(trigger = reactive(nil())),
    loaded_state    = reactive(NULL),
    sidecar_source  = mock_sidecar("week_09.pdf")
  ), {
    session$flushReact()

    nil(1); session$flushReact()
    session$setInputs(fld_suspected = "0", fld_confirmed = "0", fld_deaths = "0")
    session$setInputs(form_submit = 1)

    d <- session$returned()
    expect_equal(nrow(d), 3)
    expect_equal(d$Variable, c("suspected", "confirmed", "deaths"))
    expect_true(all(d$Value == "0"))

    # The defining property: no region is implicated.
    expect_true(all(is.na(d$Region_ID)))
    expect_true(all(d$Entry_Mode == "nil"))

    # Still a full ledger row: provenance and period travel with it, which is
    # what makes it evidence the document was processed.
    expect_true(all(d$Image_File == "week_09.pdf"))
    expect_equal(unique(d$Epi_Week), 52L)
  })
})

test_that("a variable the document does not break down can be recorded as NA", {
  # 2018 gives suspected cases nationally only, so a nil week is zero confirmed
  # and zero deaths but an unknown state-level suspected count.
  nil <- reactiveVal(0)

  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(character(0)), click = reactive(NULL)),
    controls_output = nil_controls(trigger = reactive(nil())),
    loaded_state    = reactive(NULL),
    sidecar_source  = mock_sidecar("week_09.pdf")
  ), {
    session$flushReact()

    nil(1); session$flushReact()
    session$setInputs(fld_suspected = "NA", fld_confirmed = "0", fld_deaths = "0")
    session$setInputs(form_submit = 1)

    d <- session$returned()
    expect_equal(nrow(d), 3)
    expect_true(is.na(d$Value[d$Variable == "suspected"]))
    expect_equal(d$Value[d$Variable == "confirmed"], "0")
  })
})

test_that("a nil return is refused until a source document is loaded", {
  # The whole point is to attribute the nil to a document; without one there
  # is nothing to attribute it to.
  nil <- reactiveVal(0)

  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(character(0)), click = reactive(NULL)),
    controls_output = nil_controls(trigger = reactive(nil())),
    loaded_state    = reactive(NULL),
    sidecar_source  = NULL
  ), {
    session$flushReact()

    nil(1); session$flushReact()
    session$setInputs(form_submit = 1)

    expect_equal(nrow(session$returned()), 0)
  })
})

test_that("a nil return is still validated", {
  nil <- reactiveVal(0)

  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(character(0)), click = reactive(NULL)),
    controls_output = nil_controls(trigger = reactive(nil())),
    loaded_state    = reactive(NULL),
    sidecar_source  = mock_sidecar("week_09.pdf")
  ), {
    session$flushReact()

    nil(1); session$flushReact()
    session$setInputs(fld_suspected = "none", fld_confirmed = "0", fld_deaths = "0")
    session$setInputs(form_submit = 1)

    expect_equal(nrow(session$returned()), 0)
    expect_match(output$quality_msg, "1 entries rejected")
  })
})

test_that("re-submitting a nil return replaces it rather than appending", {
  # The document-level row must be correctable like any other, which needs the
  # region match to be NA-safe.
  nil <- reactiveVal(0)

  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(character(0)), click = reactive(NULL)),
    controls_output = nil_controls(trigger = reactive(nil())),
    loaded_state    = reactive(NULL),
    sidecar_source  = mock_sidecar("week_09.pdf")
  ), {
    session$flushReact()

    nil(1); session$flushReact()
    session$setInputs(fld_suspected = "0", fld_confirmed = "0", fld_deaths = "0")
    session$setInputs(form_submit = 1)
    expect_equal(nrow(session$returned()), 3)

    # Reopened and corrected: one death was reported after all.
    nil(2); session$flushReact()
    session$setInputs(fld_suspected = "0", fld_confirmed = "0", fld_deaths = "1")
    session$setInputs(form_submit = 2)

    d <- session$returned()
    expect_equal(nrow(d), 3)
    expect_equal(d$Value[d$Variable == "deaths"], "1")
  })
})

test_that("a nil return does not disturb rows already entered for the document", {
  nil <- reactiveVal(0)
  ctrl <- nil_controls(trigger = reactive(nil()))

  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(character(0)),
                           click    = reactive(list(id = "NGA.6_1"))),
    controls_output = ctrl,
    loaded_state    = reactive(NULL),
    sidecar_source  = mock_sidecar("week_09.pdf")
  ), {
    session$flushReact()

    # A region entered normally.
    session$setInputs(fld_suspected = "5", fld_confirmed = "2", fld_deaths = "0")
    session$setInputs(form_submit = 1)
    expect_equal(nrow(session$returned()), 3)

    # The nil row is a separate record; it must not replace the region's rows.
    nil(1); session$flushReact()
    session$setInputs(fld_suspected = "0", fld_confirmed = "0", fld_deaths = "0")
    session$setInputs(form_submit = 2)

    d <- session$returned()
    expect_equal(nrow(d), 6)
    expect_equal(sum(is.na(d$Region_ID)), 3)
    expect_equal(sum(d$Region_ID == "NGA.6_1", na.rm = TRUE), 3)
  })
})

test_that("in paint mode the nil return uses the batch variable", {
  # Paint mode hides the schema builder, so a nil return there records the one
  # variable the operator is painting rather than a schema they cannot see.
  nil <- reactiveVal(0)
  ctrl <- mock_controls(entry_mode = "single", nil_trigger = reactive(nil()),
                        metadata = test_metadata(var_name = "cases_detected"))

  shiny::testServer(workbench_server, args = list(
    map_source      = list(selected = reactive(character(0)), click = reactive(NULL)),
    controls_output = ctrl,
    loaded_state    = reactive(NULL),
    sidecar_source  = mock_sidecar("week_09.pdf")
  ), {
    session$flushReact()

    nil(1); session$flushReact()
    session$setInputs(fld_cases_detected = "0")
    session$setInputs(form_submit = 1)

    d <- session$returned()
    expect_equal(nrow(d), 1)
    expect_equal(d$Variable, "cases_detected")
    expect_equal(d$Value, "0")
    expect_true(is.na(d$Region_ID))
    expect_equal(d$Entry_Mode, "nil")
  })
})
