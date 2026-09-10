# tests/testthat/test-state-manager.R
# Project file format: schema travels with the ledger, and files written by
# earlier versions of the app still open.

sample_ledger <- function(n = 3) {
  data.frame(
    Timestamp   = rep("2026-08-27T10:00:00+0100", n),
    Region_ID   = paste0("NGA.", seq_len(n), "_1"),
    Region_Name = paste0("Region", seq_len(n)),
    Variable    = rep("confirmed", n),
    Value       = as.character(seq_len(n)),
    stringsAsFactors = FALSE
  )
}

SCHEMA_TXT <- "suspected, count\nconfirmed, count\ndeaths, count"
RULES_TXT  <- "confirmed <= suspected"

test_that("a saved project round-trips through disk with its schema", {
  path <- withr::local_tempfile(fileext = ".rds")

  saveRDS(build_project_state(sample_ledger(), SCHEMA_TXT, RULES_TXT), path)
  st <- read_project_state(readRDS(path))

  expect_false(st$legacy)
  expect_equal(st$version, PROJECT_VERSION)
  expect_equal(nrow(st$ledger), 3)
  expect_equal(st$schema_text, SCHEMA_TXT)
  expect_equal(st$rules_text, RULES_TXT)

  # The restored declarations must parse back to the same schema.
  expect_equal(parse_schema_text(st$schema_text)$name,
               c("suspected", "confirmed", "deaths"))
})

test_that("the saved object records format and version", {
  st <- build_project_state(sample_ledger(), SCHEMA_TXT, RULES_TXT)
  expect_equal(st$format, PROJECT_FORMAT)
  expect_equal(st$version, PROJECT_VERSION)
  expect_match(st$saved_at, "^\\d{4}-\\d{2}-\\d{2}T")
})

test_that("legacy projects - a bare data frame - still load", {
  # This is the format of every project saved before schema persistence,
  # including the published Lassa fever dataset.
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(sample_ledger(5), path)

  st <- read_project_state(readRDS(path))

  expect_true(st$legacy)
  expect_equal(st$version, 1L)
  expect_equal(nrow(st$ledger), 5)
  expect_equal(st$schema_text, "")
  expect_equal(st$rules_text, "")
})

test_that("a project with no schema declared is not treated as legacy", {
  st <- read_project_state(build_project_state(sample_ledger(), "", ""))
  expect_false(st$legacy)
  expect_equal(st$schema_text, "")
})

test_that("an empty ledger survives a round trip", {
  st <- read_project_state(build_project_state(data.frame(), SCHEMA_TXT, ""))
  expect_equal(nrow(st$ledger), 0)
  expect_equal(st$schema_text, SCHEMA_TXT)
})

test_that("NULL schema arguments become empty strings, never NULL", {
  st <- build_project_state(sample_ledger(), NULL, NULL)
  expect_identical(st$schema_text, "")
  expect_identical(st$rules_text, "")
})

test_that("unrecognised files are rejected with a clear error", {
  expect_error(read_project_state(list(foo = 1)), "Not a Map Liberator project")
  expect_error(read_project_state("just a string"), "Not a Map Liberator project")
  expect_error(read_project_state(NULL), "Not a Map Liberator project")
  # A list whose ledger is not a data frame is not a project either.
  expect_error(read_project_state(list(ledger = "nope")), "Not a Map Liberator project")
})

# --- MODULE BEHAVIOUR ---------------------------------------------------------

test_that("loading exposes the ledger and schema separately", {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(build_project_state(sample_ledger(), SCHEMA_TXT, RULES_TXT), path)

  shiny::testServer(state_manager_server, args = list(
    data_to_save   = reactive(data.frame()),
    schema_to_save = reactive(list(schema_text = "", rules_text = ""))
  ), {
    session$setInputs(load_file = data.frame(
      name = "p.rds", size = 1, type = "", datapath = path,
      stringsAsFactors = FALSE
    ))

    out <- session$returned
    expect_equal(nrow(out$ledger()), 3)
    expect_equal(out$schema()$schema_text, SCHEMA_TXT)
    expect_match(output$status_msg, "3 rows \\+ schema")
  })
})

test_that("a legacy file yields a NULL schema so current declarations survive", {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(sample_ledger(2), path)

  shiny::testServer(state_manager_server, args = list(
    data_to_save   = reactive(data.frame()),
    schema_to_save = reactive(list(schema_text = "", rules_text = ""))
  ), {
    session$setInputs(load_file = data.frame(
      name = "old.rds", size = 1, type = "", datapath = path,
      stringsAsFactors = FALSE
    ))

    out <- session$returned
    expect_equal(nrow(out$ledger()), 2)
    expect_null(out$schema())
    expect_match(output$status_msg, "legacy format")
  })
})

test_that("a corrupt file surfaces an error instead of crashing the session", {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(nonsense = TRUE), path)

  shiny::testServer(state_manager_server, args = list(
    data_to_save   = reactive(data.frame()),
    schema_to_save = reactive(list(schema_text = "", rules_text = ""))
  ), {
    session$setInputs(load_file = data.frame(
      name = "bad.rds", size = 1, type = "", datapath = path,
      stringsAsFactors = FALSE
    ))

    out <- session$returned
    expect_null(out$ledger())
    expect_null(out$schema())
    expect_equal(output$status_msg, "")
  })
})
