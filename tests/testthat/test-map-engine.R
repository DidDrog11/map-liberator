# tests/testthat/test-map-engine.R
# Selection state on the map, and the rule that a selection belongs to the
# source document it was made against.
#
# Every test starts with session$flushReact(). An observer only records its
# reactive dependencies when it first executes, and observeEvent defers that to
# the first flush; setting an input before then leaves the click observer
# unsubscribed and the click is silently lost. This is a testServer property,
# not app behaviour.

click_shape <- function(session, id) {
  # The nonce stands in for the lat/lng Leaflet sends, making a repeat click on
  # the same polygon a distinct event.
  session$setInputs(map_shape_click = list(id = id, nonce = as.numeric(Sys.time())))
  session$flushReact()
}

test_that("clicking selects and clicking again deselects", {
  shiny::testServer(map_engine_server, args = list(
    controls_output = mock_controls("single"),
    image           = NULL
  ), {
    session$flushReact()

    click_shape(session, "NGA.28_1")
    expect_equal(session$returned$selected(), "NGA.28_1")

    click_shape(session, "NGA.12_1")
    expect_setequal(session$returned$selected(), c("NGA.28_1", "NGA.12_1"))

    click_shape(session, "NGA.28_1")
    expect_equal(session$returned$selected(), "NGA.12_1")
  })
})

test_that("a click on an id not in the layer is ignored", {
  shiny::testServer(map_engine_server, args = list(
    controls_output = mock_controls("single"),
    image           = NULL
  ), {
    session$flushReact()
    click_shape(session, "NOT.A.REAL.ID")
    expect_length(session$returned$selected(), 0)
  })
})

test_that("loading a new source document clears the selection", {
  # The rule that matters: in batch mode a selection left over from the
  # previous report would be committed under the new report's metadata, with
  # every value individually valid, so no later check could catch it.
  sc <- mock_sidecar_switchable("week_09.pdf")

  shiny::testServer(map_engine_server, args = list(
    controls_output = mock_controls("single"),
    image           = sc$reactive
  ), {
    session$flushReact()

    click_shape(session, "NGA.6_1")       # Bauchi, recorded for week 09
    expect_equal(session$returned$selected(), "NGA.6_1")

    sc$load("week_10.pdf")                 # operator uploads the next report
    session$flushReact()

    expect_length(session$returned$selected(), 0)
  })
})

test_that("pausing the clock does not clear the selection", {
  # The sidecar's reactive also changes when the clock is paused. Watching it
  # naively would wipe a batch mid-entry, so the file name is compared against
  # the last one seen.
  sc <- mock_sidecar_switchable("week_09.pdf")

  shiny::testServer(map_engine_server, args = list(
    controls_output = mock_controls("single"),
    image           = sc$reactive
  ), {
    session$flushReact()

    click_shape(session, "NGA.6_1")
    click_shape(session, "NGA.28_1")
    expect_length(session$returned$selected(), 2)

    sc$pause(30)
    session$flushReact()

    expect_length(session$returned$selected(), 2)
  })
})

test_that("the first document loaded does not trigger a clear", {
  # Nothing can belong to a previous document when there was none, so a
  # selection made before any file is loaded survives the first load.
  sc <- mock_sidecar_switchable("week_09.pdf")

  shiny::testServer(map_engine_server, args = list(
    controls_output = mock_controls("single"),
    image           = sc$reactive
  ), {
    session$flushReact()
    click_shape(session, "NGA.6_1")
    expect_equal(session$returned$selected(), "NGA.6_1")
  })
})

test_that("the Reset control clears the selection", {
  clear <- reactiveVal(0)
  ctrl  <- mock_controls("single")
  ctrl$clear_trigger <- reactive(clear())

  shiny::testServer(map_engine_server, args = list(
    controls_output = ctrl,
    image           = NULL
  ), {
    session$flushReact()

    click_shape(session, "NGA.6_1")
    expect_length(session$returned$selected(), 1)

    clear(1); session$flushReact()
    expect_length(session$returned$selected(), 0)
  })
})

test_that("the map still works when no sidecar is attached", {
  # `image` is optional so the module can be driven without a reference file.
  shiny::testServer(map_engine_server, args = list(
    controls_output = mock_controls("single"),
    image           = NULL
  ), {
    session$flushReact()
    click_shape(session, "NGA.6_1")
    expect_equal(session$returned$selected(), "NGA.6_1")
  })
})
