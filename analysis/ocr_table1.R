# analysis/ocr_table1.R
# ------------------------------------------------------------------------------
# PURPOSE
#   Read the Table 1 rows (current week, cumulative this year, cumulative
#   prior year) from page 1 of an NCDC sitrep whose text layer is unusable,
#   by OCR. Around 60 reports from 2021 to mid 2022 have glyphs split across
#   lines in the PDF text, so pdftools returns nothing the benchmark parser
#   can use; the page itself renders perfectly.
#
# METHOD
#   Render page 1 at 300 dpi, locate the "Current" label with word-level OCR,
#   crop from there to below the third row, upscale, and OCR the crop twice:
#   once with the default character set (for the row labels and "State(s)")
#   and once restricted to digits, since the default pass misreads some
#   digits in the small current-week cells (77 read as "7/"). Rows are the
#   lines matching "int int int int pct%".
#
#   Reliability check offered to callers: cumulative(week w) minus
#   cumulative(week w-1) must equal the current-week row of week w. The
#   cumulative rows use larger numbers and OCR reads them consistently.
#
# SCOPE
#   Analysis code, not part of the app. Needs pdftools, png and tesseract
#   (English data is bundled with the tesseract package on Windows).
# ------------------------------------------------------------------------------

ROW_PAT <- "(\\d[\\d,]*)\\s+(\\d[\\d,]*)\\s+(\\d[\\d,]*)\\s+(\\d[\\d,]*)\\s+([\\d.]+)\\s*%"

# Rows are labelled from the words on the same line in the default pass:
# "Cumulative" marks a cumulative row, "week" (as in "(week 43)") the current
# week. Position alone is not enough because OCR sometimes drops the
# current-week row entirely, which would shift a cumulative row into its place.
.ocr_rows_from_text <- function(txt) {
  l <- trimws(strsplit(txt, "\n")[[1]])
  m <- regmatches(l, regexec(ROW_PAT, l, perl = TRUE))
  keep <- lengths(m) == 6
  mapply(function(g, line) list(
    suspected = as.integer(gsub(",", "", g[2])), confirmed = as.integer(gsub(",", "", g[3])),
    probable  = as.integer(gsub(",", "", g[4])), deaths    = as.integer(gsub(",", "", g[5])),
    cfr       = as.numeric(g[6]),
    label     = if (grepl("Cumulative", line, ignore.case = TRUE)) "cum"
                else if (grepl("week", line, ignore.case = TRUE)) "week" else "unknown"),
    m[keep], l[keep], SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

# ocr_table1(path, dpi = 300)
#   Returns a list with `rows` (up to three lists: week, cumulative, prior
#   year, in page order), `states` (integer vector of State(s): counts in
#   page order) and `agree` (TRUE when the digits-only and default passes
#   produced identical numeric rows). NULL rows when nothing was found.
ocr_table1 <- function(path, dpi = 300) {
  png <- tempfile(fileext = ".png")
  on.exit(unlink(png), add = TRUE)
  pdftools::pdf_convert(path, pages = 1, dpi = dpi, filenames = png, verbose = FALSE)

  eng <- tesseract::tesseract("eng")
  d   <- tesseract::ocr_data(png, engine = eng)
  bb  <- do.call(rbind, lapply(strsplit(d$bbox, ","), as.numeric))
  d$y1 <- bb[, 2]
  i <- which(d$word == "Current")[1]
  if (is.na(i)) return(list(rows = NULL, states = integer(0), agree = NA))

  img <- png::readPNG(png)
  h <- dim(img)[1]
  y_top <- max(1, d$y1[i] - 40); y_bot <- min(h, d$y1[i] + 720)
  crop <- img[y_top:y_bot, , , drop = FALSE]
  big  <- crop[rep(seq_len(nrow(crop)), each = 2), rep(seq_len(ncol(crop)), each = 2), , drop = FALSE]
  cp   <- tempfile(fileext = ".png"); on.exit(unlink(cp), add = TRUE)
  png::writePNG(big, cp)

  e_txt <- tesseract::tesseract("eng", options = list(tessedit_pageseg_mode = "4"))
  e_num <- tesseract::tesseract("eng", options = list(tessedit_pageseg_mode = "4",
                                                      tessedit_char_whitelist = "0123456789.%() "))
  t_txt <- tesseract::ocr(cp, engine = e_txt)
  t_num <- tesseract::ocr(cp, engine = e_num)

  rows_txt <- .ocr_rows_from_text(t_txt)
  rows_num <- .ocr_rows_from_text(t_num)
  states <- as.integer(unlist(regmatches(t_txt, gregexpr("(?<=State\\(s\\): )\\d+", t_txt, perl = TRUE))))

  num_of <- function(r) unlist(r[c("suspected", "confirmed", "probable", "deaths", "cfr")])
  agree <- length(rows_txt) == length(rows_num) && length(rows_num) > 0 &&
    all(mapply(function(a, b) identical(num_of(a), num_of(b)), rows_txt, rows_num))

  # The digits-only pass gets the small cells right but carries no labels.
  # When both passes found the same number of rows, take the digits pass's
  # numbers with the default pass's labels; otherwise keep the labelled rows.
  rows <- if (length(rows_num) == length(rows_txt) && length(rows_txt) > 0) {
    mapply(function(n, t) { n$label <- t$label; n }, rows_num, rows_txt, SIMPLIFY = FALSE)
  } else rows_txt

  week <- Filter(function(r) r$label == "week", rows)
  cums <- Filter(function(r) r$label == "cum",  rows)
  list(rows = rows, states = states, agree = agree,
       week = if (length(week)) week[[1]] else NULL,      # current-week row, or NULL
       cum  = if (length(cums)) cums[[1]] else NULL)      # this year's cumulative row, or NULL
}

# pick_cumulative(rows, prev_cum, week)
#   Identify this year's cumulative row among OCR rows. With all three rows
#   present it is the second. When a row was dropped, only two remain and the
#   second may be the prior-year row instead; accept it only if it chains
#   from the previous week's cumulative row (cum_prev + week ~ cum) or, with
#   no previous week, is at least as large as the current week row.
pick_cumulative <- function(rows, prev_cum = NULL, week = NULL) {
  if (length(rows) >= 3) return(rows[[2]])
  if (length(rows) < 2) return(NULL)
  cand <- rows[[2]]
  if (!is.null(prev_cum) && !is.null(week)) {
    expect <- prev_cum$confirmed + week$confirmed
    if (abs(cand$confirmed - expect) <= 2 && abs(cand$suspected - (prev_cum$suspected + week$suspected)) <= 5) {
      return(cand)
    }
    return(NULL)
  }
  if (!is.null(week) && cand$suspected >= week$suspected && cand$confirmed >= week$confirmed) return(cand)
  NULL
}

# week_from_cumulative(cum, prev_cum)
#   Current-week values implied by two consecutive cumulative rows, or NULL.
week_from_cumulative <- function(cum, prev_cum) {
  if (is.null(cum) || is.null(prev_cum)) return(NULL)
  list(suspected = cum$suspected - prev_cum$suspected, confirmed = cum$confirmed - prev_cum$confirmed,
       probable = cum$probable - prev_cum$probable, deaths = cum$deaths - prev_cum$deaths)
}
