# tests/testthat/test-controls.R
# Pure helpers of the control panel. The panel's server needs GADM files and
# is not exercised here.

test_that("metadata_from_rows recovers the panel values a document's rows were stamped with", {
  led <- data.frame(
    Project = c("lassa", "lassa"), Source = c("w07", "w07"),
    Date_Ref = c("2020-02-15", "2020-02-15"), Date_End = c(NA, "2020-02-21"),
    Epi_Week = c(7L, 7L), Image_File = c("w07.pdf", "w07.pdf"),
    stringsAsFactors = FALSE
  )
  m <- metadata_from_rows(led, "w07.pdf")
  expect_equal(m$project, "lassa")
  expect_equal(m$source, "w07")
  expect_equal(c(m$year, m$month, m$day), c("2020", "02", "15"))
  expect_equal(c(m$end_year, m$end_month, m$end_day), c("2020", "02", "21"))  # last row wins
  expect_equal(m$week, 7L)
})

test_that("metadata_from_rows is NULL for an unknown document, an empty ledger, or no file", {
  led <- data.frame(Project = "p", Source = "s", Date_Ref = "2020-01-01",
                    Epi_Week = 1L, Image_File = "a.pdf", stringsAsFactors = FALSE)
  expect_null(metadata_from_rows(led, "b.pdf"))
  expect_null(metadata_from_rows(data.frame(), "a.pdf"))
  expect_null(metadata_from_rows(led, NA_character_))
  expect_null(metadata_from_rows(NULL, "a.pdf"))
})

test_that("metadata_from_rows tolerates rows without a period end or with XX date parts", {
  led <- data.frame(Project = "p", Source = "s", Date_Ref = "2020-XX-XX",
                    Epi_Week = NA, Image_File = "a.pdf", stringsAsFactors = FALSE)
  m <- metadata_from_rows(led, "a.pdf")
  expect_equal(c(m$year, m$month, m$day), c("2020", "XX", "XX"))
  expect_equal(c(m$end_year, m$end_month, m$end_day), c("", "", ""))
  expect_true(is.na(m$week))
})
