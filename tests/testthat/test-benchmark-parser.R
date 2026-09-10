# tests/testthat/test-benchmark-parser.R
# The NCDC page-1 parser, checked against the Epi Week 52, 2025 report bundled
# in www/ for the tutorial. These figures are the ground truth an extraction is
# scored against, so a silent regression here would silently invalidate the
# accuracy assessment.

skip_if_no_fixture <- function() {
  testthat::skip_if_not_installed("pdftools")
  testthat::skip_if_not(file.exists(sitrep_fixture()), "NCDC sitrep fixture not present")
}

test_that("the week 52 2025 report parses without flags", {
  skip_if_no_fixture()
  b <- parse_ncdc_sitrep(sitrep_fixture())

  expect_equal(nrow(b), 1)
  expect_equal(b$parse_flags, "")
  expect_equal(b$epi_week, 52L)
  expect_equal(b$year, 2025L)
  expect_equal(b$n_pages, 7L)
})

test_that("current-week figures match the printed Table 1", {
  skip_if_no_fixture()
  b <- parse_ncdc_sitrep(sitrep_fixture())

  expect_equal(b$wk_suspected, 101L)
  expect_equal(b$wk_confirmed, 27L)
  expect_equal(b$wk_probable, 0L)
  expect_equal(b$wk_deaths, 9L)
  expect_equal(b$wk_cfr, 33.3)
  expect_equal(b$wk_states, 5L)
  # The count a Figure 3 digitisation has to reproduce.
  expect_equal(b$wk_lgas, 12L)
})

test_that("cumulative and prior-year rows are read in the right order", {
  skip_if_no_fixture()
  b <- parse_ncdc_sitrep(sitrep_fixture())

  expect_equal(b$cum_suspected, 9389L)
  expect_equal(b$cum_confirmed, 1148L)
  expect_equal(b$cum_deaths, 215L)
  expect_equal(b$cum_states, 22L)
  expect_equal(b$cum_lgas, 107L)

  expect_equal(b$prev_suspected, 10098L)
  expect_equal(b$prev_confirmed, 1309L)
  expect_equal(b$prev_deaths, 214L)
  expect_equal(b$prev_states, 28L)
  expect_equal(b$prev_lgas, 139L)
})

test_that("named states are recovered and agree with the tabulated count", {
  skip_if_no_fixture()
  b <- parse_ncdc_sitrep(sitrep_fixture())

  named <- strsplit(b$wk_states_named, "|", fixed = TRUE)[[1]]
  expect_setequal(named, c("Bauchi", "Ondo", "Ebonyi", "Taraba", "Nasarawa"))
  # The parser's own consistency check: names must match the State(s): cell.
  expect_equal(length(named), b$wk_states)
})

test_that("CFR is consistent with the deaths and confirmed counts", {
  skip_if_no_fixture()
  b <- parse_ncdc_sitrep(sitrep_fixture())
  # NCDC computes weekly CFR as deaths / confirmed for the same period.
  expect_equal(round(100 * b$wk_deaths / b$wk_confirmed, 1), b$wk_cfr)
})

test_that("a corpus run keeps unreadable reports as flagged rows", {
  skip_if_no_fixture()

  bad <- withr::local_tempfile(fileext = ".pdf")
  writeLines("not a pdf", bad)

  out <- parse_ncdc_corpus(c(sitrep_fixture(), bad))

  expect_equal(nrow(out), 2)               # coverage stays honest
  expect_equal(out$parse_flags[1], "")
  expect_match(out$parse_flags[2], "read_error")
})

test_that("a missing file is an error, not a silent empty result", {
  expect_error(parse_ncdc_sitrep("does_not_exist.pdf"))
})
