# tests/testthat/test-sidecar.R
# The digitisation clock: starts on image load, pauses and resumes, and resets
# when a new image is loaded.

# The render path base64-encodes the upload, so it needs a file that exists.
fake_png <- function() {
  f <- withr::local_tempfile(fileext = ".png", .local_envir = parent.frame())
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), f)
  f
}

test_that("the clock starts on upload and pause/resume accumulate paused time", {
  shiny::testServer(sidecar_server, {
    expect_true(is.na(session$returned()$loaded_at))

    session$setInputs(img_file = list(name = "wk52.png", datapath = fake_png(), type = "image/png"))
    m <- session$returned()
    expect_equal(m$file_name, "wk52.png")
    expect_false(is.na(m$loaded_at))
    expect_true(is.na(m$paused_at))
    expect_equal(m$paused_secs, 0)

    session$setInputs(toggle_clock = 1)
    expect_false(is.na(session$returned()$paused_at))

    session$setInputs(toggle_clock = 2)
    m <- session$returned()
    expect_true(is.na(m$paused_at))
    expect_gte(m$paused_secs, 0)
    expect_lt(m$paused_secs, 5)
  })
})

test_that("loading a new image resets the clock", {
  shiny::testServer(sidecar_server, {
    session$setInputs(img_file = list(name = "a.png", datapath = fake_png(), type = "image/png"))
    session$setInputs(toggle_clock = 1)                  # paused
    session$setInputs(img_file = list(name = "b.png", datapath = fake_png(), type = "image/png"))
    m <- session$returned()
    expect_equal(m$file_name, "b.png")
    expect_true(is.na(m$paused_at))
    expect_equal(m$paused_secs, 0)
  })
})

test_that("pausing before any image is loaded is a no-op", {
  shiny::testServer(sidecar_server, {
    session$setInputs(toggle_clock = 1)
    expect_true(is.na(session$returned()$paused_at))
  })
})

test_that("a PDF upload is served from a session-scoped resource path and hides the image controls", {
  shiny::testServer(sidecar_server, {
    pdf <- withr::local_tempfile(fileext = ".pdf")
    writeLines("%PDF-1.4 stub", pdf)
    session$setInputs(img_file = list(name = "sitrep_w07.pdf", datapath = pdf, type = "application/pdf"))
    expect_true(is_pdf())
    expect_equal(session$returned()$file_name, "sitrep_w07.pdf")
    expect_false(is.na(session$returned()$loaded_at))
    expect_true(pdf_prefix %in% names(shiny::resourcePaths()))
    expect_match(pdf_url(), paste0("^", pdf_prefix, "/reference.pdf"))
    expect_true(file.exists(file.path(pdf_dir, "reference.pdf")))
  })
})
