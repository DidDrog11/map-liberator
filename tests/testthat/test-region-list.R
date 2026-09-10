# tests/testthat/test-region-list.R
# The list view must honour the same contract as the map: a click in form mode
# is a request to open the form; a multi-select in batch mode is the batch.

test_that("a row click in form mode emits the region's layerId and clears the selection", {
  shiny::testServer(region_list_server, args = list(
    geom_data  = reactive(test_geometry()),
    entry_mode = reactive("form")
  ), {
    session$flushReact()
    # Rows are sorted by name: Bauchi, Edo, Ondo.
    session$setInputs(tbl_rows_selected = 3L)
    expect_equal(session$returned$click()$id, "NGA.28_1")
    expect_equal(session$returned$selected(), character(0))

    # Clicking the same row again must register as a new click.
    first <- session$returned$click()$nonce
    session$setInputs(tbl_rows_selected = NULL)
    session$setInputs(tbl_rows_selected = 3L)
    expect_gte(session$returned$click()$nonce, first)
  })
})

test_that("a multi-select in batch mode is the batch and emits no click", {
  shiny::testServer(region_list_server, args = list(
    geom_data  = reactive(test_geometry()),
    entry_mode = reactive("single")
  ), {
    session$flushReact()
    session$setInputs(tbl_rows_selected = c(1L, 2L))
    expect_setequal(session$returned$selected(), c("NGA.6_1", "NGA.12_1"))
    expect_null(session$returned$click())
  })
})

test_that("the Entered column counts variables recorded against the current image only", {
  led <- data.frame(
    Region_ID  = c("NGA.28_1", "NGA.28_1", "NGA.12_1"),
    Image_File = c("w07.png", "w07.png", "w08.png"),
    stringsAsFactors = FALSE
  )
  shiny::testServer(region_list_server, args = list(
    geom_data  = reactive(test_geometry()),
    entry_mode = reactive("form"),
    ledger     = reactive(led),
    image      = reactive(list(file_name = "w07.png"))
  ), {
    session$flushReact()
    # Order is Bauchi, Edo, Ondo: only Ondo has rows for w07.png.
    expect_equal(entered(), c(0L, 0L, 2L))
  })
})

test_that("an empty layer renders a placeholder rather than erroring", {
  shiny::testServer(region_list_server, args = list(
    geom_data  = reactive(NULL),
    entry_mode = reactive("form")
  ), {
    session$flushReact()
    expect_null(regions())
    expect_equal(session$returned$selected(), character(0))
  })
})

test_that("recently-entered ordering follows earlier files and ignores the current one", {
  led <- data.frame(
    Region_ID  = c("NGA.12_1", "NGA.28_1", "NGA.6_1"),
    Image_File = c("w05.pdf",  "w05.pdf",  "w06.pdf"),
    Timestamp  = c("2026-09-10T10:00:00+0200", "2026-09-10T10:05:00+0200", "2026-09-10T11:00:00+0200"),
    stringsAsFactors = FALSE
  )
  shiny::testServer(region_list_server, args = list(
    geom_data  = reactive(test_geometry()),
    entry_mode = reactive("form"),
    ledger     = reactive(led),
    image      = reactive(list(file_name = "w06.pdf"))
  ), {
    session$setInputs(order_mode = "alpha")
    expect_equal(regions()$Region, c("Bauchi", "Edo", "Ondo"))

    # Bauchi's only entry is for the current file, so it is treated as never
    # entered; Ondo was entered after Edo in w05, so it comes first.
    session$setInputs(order_mode = "recent")
    expect_equal(regions()$Region, c("Ondo", "Edo", "Bauchi"))

    # Row indices still map to the displayed order.
    session$setInputs(tbl_rows_selected = 1L)
    expect_equal(session$returned$click()$id, "NGA.28_1")
  })
})
