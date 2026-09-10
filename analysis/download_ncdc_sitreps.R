# analysis/download_ncdc_sitreps.R
# ------------------------------------------------------------------------------
# PURPOSE
#   Fetch every NCDC Lassa fever situation report listed on the NCDC website,
#   rename each by report year and epidemiological week, and write a manifest
#   so the extraction exercise can be resumed and audited.
#
# SOURCE
#   https://ncdc.gov.ng/diseases/sitreps/?cat=5&name=An%20update%20of%20Lassa%20fever%20outbreak%20in%20Nigeria
#   The page is one HTML table. Each row holds an index, a title ("... for
#   Week N"), a commented-out publication date, and a download link whose
#   `download` attribute carries the report date and week as _ddmmyy_WW.pdf.
#   That attribute is the naming source: the visible week and date are
#   occasionally corrupt (a "Week 613566758" row, a date in 2007).
#
# OUTPUT
#   data/sitreps/lassa_sitrep_<YYYY>_w<WW>_<yyyymmdd>.pdf   one per report
#   data/sitreps/manifest.csv                               one row per listing entry
#
#   The PDFs are gitignored; the manifest is not. Re-running skips files that
#   already exist, so an interrupted download resumes where it stopped.
#
# SCOPE
#   Analysis code. Not sourced by app.R; rvest/httr2/pdftools are not app
#   dependencies.
#
# USAGE (from the project root)
#   source("analysis/download_ncdc_sitreps.R")
#   manifest <- download_ncdc_sitreps()          # fetch listing + download
#   manifest <- verify_ncdc_sitreps(manifest)    # read each PDF, check week
# ------------------------------------------------------------------------------

LISTING_URL <- paste0(
  "https://ncdc.gov.ng/diseases/sitreps/?cat=5&name=",
  "An%20update%20of%20Lassa%20fever%20outbreak%20in%20Nigeria")
SITREP_DIR  <- file.path("data", "sitreps")
MANIFEST    <- file.path(SITREP_DIR, "manifest.csv")
USER_AGENT  <- "map-liberator sitrep archive (research use)"

# --- LISTING ------------------------------------------------------------------

# One row per <tr> on the listing page. Dates and weeks are parsed from the
# download attribute; the visible title and the commented date are kept for
# cross-checking but not trusted for naming.
parse_ncdc_listing <- function(html) {
  rows <- rvest::html_elements(html, "tr")
  has_link <- vapply(rows, function(tr) {
    !is.na(rvest::html_attr(rvest::html_element(tr, "a[href*='sitreps/']"), "href"))
  }, logical(1))
  rows <- rows[has_link]

  out <- lapply(rows, function(tr) {
    a    <- rvest::html_element(tr, "a[href*='sitreps/']")
    txt  <- trimws(rvest::html_text2(rvest::html_elements(tr, "td")))
    href <- rvest::html_attr(a, "href")
    dl   <- rvest::html_attr(a, "download")
    # The publication date is inside an HTML comment, invisible to html_text.
    raw  <- as.character(tr)
    cm   <- regmatches(raw, regexpr("<!--<td>([^<]*)</td>-->", raw))
    listed_date <- if (length(cm)) gsub("<!--<td>|</td>-->", "", cm) else NA_character_

    data.frame(
      listing_index = suppressWarnings(as.integer(txt[1])),
      listed_title  = if (length(txt) >= 2) txt[2] else NA_character_,
      listed_date   = listed_date,
      download_name = dl,
      url           = xml2::url_absolute(href, "https://ncdc.gov.ng/"),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, out)

  # _ddmmyy_WW.pdf -> report date and epi week.
  m <- regmatches(df$download_name,
                  regexec("_(\\d{2})(\\d{2})(\\d{2})_(\\d{1,2})\\.pdf$", df$download_name))
  df$report_date <- as.Date(vapply(m, function(g) {
    if (length(g) < 5) return(NA_character_)
    sprintf("20%s-%s-%s", g[4], g[3], g[2])
  }, character(1)))
  df$epi_week_listed <- vapply(m, function(g) {
    if (length(g) < 5) NA_integer_ else as.integer(g[5])
  }, integer(1))

  # Fall back to the visible title/date when the download attribute is unusable.
  miss <- is.na(df$report_date)
  if (any(miss)) {
    wk <- suppressWarnings(as.integer(sub(".*Week\\s+(\\d+).*", "\\1", df$listed_title[miss])))
    dt <- suppressWarnings(as.Date(df$listed_date[miss], format = "%d %B %Y"))
    df$epi_week_listed[miss] <- wk
    df$report_date[miss]     <- dt
  }
  df$report_year <- as.integer(format(df$report_date, "%Y"))

  df$hash <- sub("\\.pdf$", "", basename(df$url))
  df$file <- ifelse(
    !is.na(df$report_date) & !is.na(df$epi_week_listed) & df$epi_week_listed <= 53,
    sprintf("lassa_sitrep_%d_w%02d_%s.pdf", df$report_year, df$epi_week_listed,
            format(df$report_date, "%Y%m%d")),
    sprintf("lassa_sitrep_unknown_%s.pdf", df$hash)
  )
  # Two listing rows can describe the same week (re-issued reports); keep both
  # by suffixing the hash on the later duplicate.
  dup <- duplicated(df$file)
  df$file[dup] <- sub("\\.pdf$", paste0("_", substr(df$hash[dup], 1, 6), ".pdf"), df$file[dup])
  df$duplicate_of_week <- dup
  df[order(df$report_date, df$epi_week_listed, na.last = TRUE), ]
}

fetch_ncdc_listing <- function(url = LISTING_URL) {
  resp <- httr2::request(url) |>
    httr2::req_user_agent(USER_AGENT) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()
  rvest::read_html(httr2::resp_body_string(resp))
}

# --- DOWNLOAD -----------------------------------------------------------------

download_ncdc_sitreps <- function(dir = SITREP_DIR, pause = 0.5, quiet = FALSE) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  manifest <- parse_ncdc_listing(fetch_ncdc_listing())
  manifest$path       <- file.path(dir, manifest$file)
  manifest$downloaded <- file.exists(manifest$path)
  manifest$bytes      <- ifelse(manifest$downloaded, file.size(manifest$path), NA_real_)
  manifest$error      <- NA_character_

  todo <- which(!manifest$downloaded)
  if (!quiet) message(nrow(manifest), " reports listed; ", length(todo), " to download.")

  for (i in todo) {
    ok <- tryCatch({
      httr2::request(manifest$url[i]) |>
        httr2::req_user_agent(USER_AGENT) |>
        httr2::req_retry(max_tries = 3, backoff = function(n) 2^n) |>
        httr2::req_perform(path = manifest$path[i])
      TRUE
    }, error = function(e) { manifest$error[i] <<- conditionMessage(e); FALSE })

    if (ok) {
      manifest$downloaded[i] <- TRUE
      manifest$bytes[i]      <- file.size(manifest$path[i])
    } else if (file.exists(manifest$path[i])) {
      unlink(manifest$path[i])  # never leave a partial file to be "resumed"
    }
    if (!quiet && (match(i, todo) %% 25 == 0 || i == max(todo)))
      message(sprintf("  %d / %d", sum(manifest$downloaded), nrow(manifest)))
    Sys.sleep(pause)
  }

  write.csv(manifest[, setdiff(names(manifest), "path")], MANIFEST, row.names = FALSE)
  invisible(manifest)
}

# --- VERIFY -------------------------------------------------------------------

# Open every downloaded PDF and read the epi week printed on page 1, so a
# listing that mislabels a report is caught before extraction starts. Also
# records page count and whether page 1 yielded any text at all: a scanned
# report has no text layer and will need every figure read by eye.
#
# Also records which page carries the per-state table (the page whose text
# mentions "Table 3"), since that table is a raster image and has to be
# rendered to PNG for the app's sidecar. Layout drift means the page number
# is not fixed across years.
verify_ncdc_sitreps <- function(manifest, dir = SITREP_DIR) {
  if (!"path" %in% names(manifest)) manifest$path <- file.path(dir, manifest$file)
  manifest$pages        <- NA_integer_
  manifest$has_text     <- NA
  manifest$epi_week_pdf <- NA_integer_
  manifest$year_pdf     <- NA_integer_
  manifest$page_table3  <- NA_integer_
  manifest$caption_table3 <- NA_character_

  for (i in which(manifest$downloaded)) {
    info <- tryCatch(pdftools::pdf_info(manifest$path[i]), error = function(e) NULL)
    if (is.null(info)) { manifest$error[i] <- "unreadable pdf"; next }
    manifest$pages[i] <- info$pages
    txt <- tryCatch(pdftools::pdf_text(manifest$path[i]), error = function(e) character(0))
    p1  <- if (length(txt)) txt[1] else ""
    manifest$has_text[i] <- nchar(trimws(p1)) > 50
    # The week must be followed by a non-digit: broken text layers run
    # "Epi Week: 2 2022" together as "22022", which must yield NA, not 22.
    wk <- regmatches(p1, regexpr("Epi\\s*Week:?\\s*(\\d{1,2})(?![\\d])", p1, perl = TRUE))
    yr <- regmatches(p1, regexpr("\\b20[12]\\d\\b", p1, perl = TRUE))
    if (length(wk)) manifest$epi_week_pdf[i] <- as.integer(gsub("\\D", "", wk))
    if (length(yr)) manifest$year_pdf[i]     <- as.integer(yr)
    # Caption lines start with "Table 3"; the page-1 highlights also say
    # "(Table 3)" in running text, which must not count.
    caps <- lapply(txt, function(pg) {
      l <- strsplit(pg, "\n")[[1]]
      trimws(l[grepl("^\\s*Table\\s*3\\s*[.:]", l, perl = TRUE)])
    })
    t3 <- which(lengths(caps) > 0)
    if (length(t3)) {
      manifest$page_table3[i]    <- t3[1]
      manifest$caption_table3[i] <- caps[[t3[1]]][1]
    } else if (isTRUE(manifest$report_year[i] >= 2020) && info$pages >= 6) {
      # Many 2021-2022 reports have a broken text layer (glyphs split across
      # lines), so the caption cannot be matched even though the table is
      # there. In the 6+ page layout it has always been on page 4; take that
      # and flag it so the operator knows to confirm on screen.
      manifest$page_table3[i]    <- 4L
      manifest$caption_table3[i] <- "(assumed: page 4, caption not readable)"
    }
  }
  manifest$week_mismatch <- !is.na(manifest$epi_week_pdf) &
    manifest$epi_week_pdf != manifest$epi_week_listed

  # The listing repeats some reports under a second link (the corrupt
  # "Week 613566758" row is the 2020 week 3 report again). Identical content
  # is flagged so the operator extracts each report once.
  manifest$md5 <- NA_character_
  ok <- which(manifest$downloaded)
  manifest$md5[ok] <- unname(tools::md5sum(manifest$path[ok]))
  first <- match(manifest$md5, manifest$md5)
  manifest$duplicate_content_of <- ifelse(!is.na(manifest$md5) & first != seq_len(nrow(manifest)),
                                          manifest$file[first], NA_character_)

  write.csv(manifest[, setdiff(names(manifest), "path")], MANIFEST, row.names = FALSE)
  invisible(manifest)
}

# --- COMPOSITE ----------------------------------------------------------------

# One image per report for the sidecar: the Table 3 page on top and page 1
# beneath it, so Table 1 (national totals, named states) is on screen without
# loading a second image, which would restart the timing clock and change the
# provenance stamp. Pages share a size, so the bitmaps stack directly.
render_ncdc_extract <- function(manifest, dir = SITREP_DIR, dpi = 150, write_manifest = TRUE) {
  if (!"path" %in% names(manifest)) manifest$path <- file.path(dir, manifest$file)
  if (!"page_table3" %in% names(manifest)) stop("run verify_ncdc_sitreps() first")
  png_dir <- file.path(dir, "png")
  dir.create(png_dir, recursive = TRUE, showWarnings = FALSE)

  manifest$extract_png <- ifelse(is.na(manifest$page_table3), NA_character_,
                                 file.path(png_dir, sub("\\.pdf$", "_extract.png", manifest$file)))
  todo <- which(!is.na(manifest$extract_png) & !file.exists(manifest$extract_png))
  for (i in todo) {
    tryCatch({
      top <- pdftools::pdf_render_page(manifest$path[i], page = manifest$page_table3[i], dpi = dpi)
      bot <- pdftools::pdf_render_page(manifest$path[i], page = 1L, dpi = dpi)
      # pdf_render_page returns channels x width x height; png wants height x width x channels.
      to_arr <- function(b) aperm(array(as.integer(b), dim(b)), c(3, 2, 1)) / 255
      a_top <- to_arr(top); a_bot <- to_arr(bot)
      w <- min(dim(a_top)[2], dim(a_bot)[2])
      # A thin dark rule between the pages so the join is visible.
      rule <- array(0.6, dim = c(6, w, dim(a_top)[3]))
      img <- abind_rows(a_top[, seq_len(w), , drop = FALSE], rule, a_bot[, seq_len(w), , drop = FALSE])
      png::writePNG(img, manifest$extract_png[i])
    }, error = function(e) manifest$error[i] <<- paste("extract:", conditionMessage(e)))
  }
  if (write_manifest) write.csv(manifest[, setdiff(names(manifest), "path")], MANIFEST, row.names = FALSE)
  invisible(manifest)
}

# Stack arrays vertically (rows), matching width and channels. Avoids a
# dependency on abind for one operation.
abind_rows <- function(...) {
  parts <- list(...)
  h <- sum(vapply(parts, function(p) dim(p)[1], integer(1)))
  w <- dim(parts[[1]])[2]; ch <- dim(parts[[1]])[3]
  out <- array(0, dim = c(h, w, ch))
  at <- 0
  for (p in parts) { n <- dim(p)[1]; out[at + seq_len(n), , ] <- p; at <- at + n }
  out
}

# --- RENDER -------------------------------------------------------------------

# Render the Table 3 page of each report to PNG so it can be loaded into the
# app's sidecar (which accepts images, not PDFs). Files are named after the
# PDF so `Image_File` in the ledger identifies the report year and week.
# Reports with no detected Table 3 page are skipped and listed in the result.
render_ncdc_table3 <- function(manifest, dir = SITREP_DIR, dpi = 150) {
  if (!"path" %in% names(manifest)) manifest$path <- file.path(dir, manifest$file)
  if (!"page_table3" %in% names(manifest)) stop("run verify_ncdc_sitreps() first")
  png_dir <- file.path(dir, "png")
  dir.create(png_dir, recursive = TRUE, showWarnings = FALSE)

  manifest$png <- ifelse(is.na(manifest$page_table3), NA_character_,
                         file.path(png_dir, sub("\\.pdf$", "_table3.png", manifest$file)))
  todo <- which(!is.na(manifest$png) & !file.exists(manifest$png))
  for (i in todo) {
    tryCatch(
      pdftools::pdf_convert(manifest$path[i], format = "png", pages = manifest$page_table3[i],
                            dpi = dpi, filenames = manifest$png[i], verbose = FALSE),
      error = function(e) manifest$error[i] <<- paste("render:", conditionMessage(e))
    )
  }
  write.csv(manifest[, setdiff(names(manifest), "path")], MANIFEST, row.names = FALSE)
  invisible(manifest)
}
