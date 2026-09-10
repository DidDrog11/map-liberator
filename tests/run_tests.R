# tests/run_tests.R
# ------------------------------------------------------------------------------
# Test entry point.
#
# Map Liberator is a Shiny application rather than an R package, so there is no
# `R CMD check` to hang the suite off. Run it from the project root with:
#
#     Rscript tests/run_tests.R
#
# or interactively with testthat::test_dir("tests/testthat").
#
# helper-app.R sources global.R and every module before any test file runs, and
# resolves the project root relative to the test directory, so the tests do not
# depend on where testthat starts.
# ------------------------------------------------------------------------------

for (pkg in c("testthat", "withr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("%s is required to run the test suite: install.packages('%s')", pkg, pkg))
  }
}

test_path <- if (dir.exists("tests/testthat")) {
  "tests/testthat"                       # run from the project root
} else if (dir.exists("testthat")) {
  "testthat"                             # run from tests/
} else {
  stop("Cannot locate tests/testthat - run this from the project root.")
}

testthat::test_dir(test_path, reporter = "summary", stop_on_failure = TRUE)
