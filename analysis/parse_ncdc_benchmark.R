# analysis/parse_ncdc_benchmark.R
# ------------------------------------------------------------------------------
# PURPOSE
#   Recover the machine-readable benchmark figures printed on page 1 of NCDC
#   Lassa fever situation reports, for use as ground truth when assessing the
#   accuracy of a Map Liberator extraction.
#
# WHY THIS EXISTS
#   The per-state breakdown (Table 3) and the LGA attack-rate choropleth
#   (Figure 3) are raster images: pdftools recovers no data from them, which is
#   why they have to be digitised by hand. Page 1, however, IS text. Table 1 and
#   the Highlights bullets independently state how many states and LGAs reported
#   confirmed cases, name those states, and give national totals. Those figures
#   constrain what a correct extraction must produce, giving a per-report
#   accuracy check that costs nothing and needs no second operator.
#
# SCOPE
#   Analysis code for the manuscript. NOT part of the Shiny application, not
#   sourced by app.R, and pdftools is deliberately not an app dependency.
#
# STATUS
#   Developed against the Epi Week 52, 2025 report. NCDC layout drifts across
#   the 2018-2025 archive, so every field is extracted defensively: anything
#   that fails to match returns NA and is recorded in `parse_flags` rather than
#   silently vanishing. Audit that column before trusting a corpus-wide run.
# ------------------------------------------------------------------------------

# --- INTERNAL HELPERS ---------------------------------------------------------

# First capture group of the first match, or NA. Guarantees a length-1 result.
.first_match <- function(pattern, x, group = 1L) {
  m <- regmatches(x, regexec(pattern, x, perl = TRUE))
  hit <- Filter(function(g) length(g) > group, m)
  if (length(hit) == 0) return(NA_character_)
  hit[[1]][group + 1L]
}

# All matches of a capture group across a character vector, in document order.
# Used for the State(s):/LGA(s): cells, which sit on their own lines.
.all_matches <- function(pattern, x, group = 1L) {
  flat <- unlist(regmatches(x, gregexpr(pattern, x, perl = TRUE)), use.names = FALSE)
  if (length(flat) == 0) return(character(0))
  vapply(flat, function(s) .first_match(pattern, s, group), character(1), USE.NAMES = FALSE)
}

.as_int <- function(x) suppressWarnings(as.integer(gsub("[^0-9]", "", x)))
.as_num <- function(x) suppressWarnings(as.numeric(x))

# i-th element, or NA when the report did not supply it.
.nth <- function(x, i) if (length(x) >= i) x[[i]] else NA_integer_

# --- TABLE 1 ------------------------------------------------------------------
# Table 1 renders as three data rows, each carrying four integers followed by a
# percentage, in a fixed order:
#   1. current week   2. cumulative, report year   3. cumulative, prior year
# Matching on that numeric shape is far more robust than matching the row
# labels, which wrap across lines and are reworded between years.
.parse_table1_rows <- function(page_lines) {
  pat <- "\\s+(\\d[\\d,]*)\\s+(\\d[\\d,]*)\\s+(\\d[\\d,]*)\\s+(\\d[\\d,]*)\\s+([\\d.]+)\\s*%"
  hits <- page_lines[grepl(pat, page_lines, perl = TRUE)]

  lapply(seq_len(3), function(i) {
    if (length(hits) < i) {
      return(list(suspected = NA_integer_, confirmed = NA_integer_,
                  probable = NA_integer_, deaths = NA_integer_, cfr = NA_real_))
    }
    g <- regmatches(hits[i], regexec(pat, hits[i], perl = TRUE))[[1]]
    list(
      suspected = .as_int(g[2]), confirmed = .as_int(g[3]),
      probable  = .as_int(g[4]), deaths    = .as_int(g[5]),
      cfr       = .as_num(g[6])
    )
  })
}

# --- HIGHLIGHTS ---------------------------------------------------------------
# The narrative bullets name the states reporting confirmed cases in the week,
# e.g. "These were reported in Bauchi, Ondo, Ebonyi, Taraba and Nasarawa
# States". That named list is the strongest available check: it validates WHICH
# regions were selected, not merely how many.
.parse_named_states <- function(page_text) {
  frag <- .first_match("reported\\s+in\\s+(.+?)\\s+States?\\b", page_text)
  if (is.na(frag)) return(character(0))

  frag  <- gsub("\\s+", " ", frag, perl = TRUE)
  frag  <- gsub("\\s+and\\s+", ", ", frag, perl = TRUE)
  parts <- trimws(strsplit(frag, ",")[[1]])
  parts <- parts[nzchar(parts)]
  # Guard against the lazy quantifier running into an adjacent clause.
  parts <- parts[nchar(parts) <= 30 & !grepl("[.;:]", parts)]
  unique(parts)
}

# --- PUBLIC: SINGLE REPORT ----------------------------------------------------

# parse_ncdc_sitrep(path)
#   path: path to a sitrep PDF.
#   Returns a one-row data.frame. `parse_flags` is a semicolon-separated list
#   of fields that could not be recovered; empty means a clean parse.
parse_ncdc_sitrep <- function(path) {
  stopifnot(file.exists(path))

  pages <- pdftools::pdf_text(path)
  p1    <- if (length(pages) >= 1) pages[[1]] else ""
  lines <- strsplit(p1, "\n", fixed = TRUE)[[1]]

  epi_week <- .as_int(.first_match("Epi\\s*Week:?\\s*(\\d{1,2})", p1))
  year     <- .as_int(.first_match("Epi\\s*Week:?\\s*\\d{1,2}\\s+(\\d{4})", p1))

  rows <- .parse_table1_rows(lines)

  # State(s):N and LGA(s):N appear once per Table 1 row, in the same order.
  states <- .as_int(.all_matches("State\\(s\\)\\s*:\\s*(\\d+)", lines))
  lgas   <- .as_int(.all_matches("LGA\\(s\\)\\s*:\\s*(\\d+)",   lines))

  named <- .parse_named_states(p1)

  out <- data.frame(
    file     = basename(path),
    epi_week = epi_week,
    year     = year,

    # Current reporting week - the row a weekly extraction must reproduce.
    wk_suspected = rows[[1]]$suspected,
    wk_confirmed = rows[[1]]$confirmed,
    wk_probable  = rows[[1]]$probable,
    wk_deaths    = rows[[1]]$deaths,
    wk_cfr       = rows[[1]]$cfr,
    wk_states    = .nth(states, 1),
    wk_lgas      = .nth(lgas,   1),

    # Year-to-date cumulative, current year.
    cum_suspected = rows[[2]]$suspected,
    cum_confirmed = rows[[2]]$confirmed,
    cum_probable  = rows[[2]]$probable,
    cum_deaths    = rows[[2]]$deaths,
    cum_cfr       = rows[[2]]$cfr,
    cum_states    = .nth(states, 2),
    cum_lgas      = .nth(lgas,   2),

    # Same period, prior year - printed for comparison.
    prev_suspected = rows[[3]]$suspected,
    prev_confirmed = rows[[3]]$confirmed,
    prev_probable  = rows[[3]]$probable,
    prev_deaths    = rows[[3]]$deaths,
    prev_cfr       = rows[[3]]$cfr,
    prev_states    = .nth(states, 3),
    prev_lgas      = .nth(lgas,   3),

    wk_states_named = paste(named, collapse = "|"),
    n_pages         = length(pages),
    stringsAsFactors = FALSE
  )

  # Record what could not be recovered rather than emitting a quiet NA.
  flags <- character(0)
  if (is.na(epi_week))           flags <- c(flags, "epi_week")
  if (is.na(year))               flags <- c(flags, "year")
  if (is.na(out$wk_confirmed))   flags <- c(flags, "table1_week_row")
  if (is.na(out$cum_confirmed))  flags <- c(flags, "table1_cumulative_row")
  if (is.na(out$prev_confirmed)) flags <- c(flags, "table1_prior_year_row")
  if (is.na(out$wk_states))      flags <- c(flags, "states_affected")
  if (is.na(out$wk_lgas))        flags <- c(flags, "lgas_affected")
  if (length(named) == 0)        flags <- c(flags, "named_states")

  # Internal consistency: the narrative list should match the tabulated count.
  if (!is.na(out$wk_states) && length(named) > 0 && length(named) != out$wk_states) {
    flags <- c(flags, "named_states_count_mismatch")
  }

  out$parse_flags <- paste(flags, collapse = ";")
  out
}

# --- PUBLIC: CORPUS -----------------------------------------------------------

# rbind across frames with differing columns (error rows are narrow).
.merge_fill <- function(frames) {
  cols <- unique(unlist(lapply(frames, names)))
  filled <- lapply(frames, function(d) {
    for (cn in setdiff(cols, names(d))) d[[cn]] <- NA
    d[, cols, drop = FALSE]
  })
  do.call(rbind, filled)
}

# parse_ncdc_corpus(paths)
#   paths: a directory of sitrep PDFs, or a vector of file paths.
#   A report that fails outright is retained as a row of NAs flagged
#   `read_error` so the corpus tally stays honest about coverage.
parse_ncdc_corpus <- function(paths) {
  if (length(paths) == 1 && dir.exists(paths)) {
    paths <- list.files(paths, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
  }
  stopifnot(length(paths) > 0)

  results <- lapply(paths, function(p) {
    tryCatch(parse_ncdc_sitrep(p), error = function(e) {
      data.frame(file = basename(p),
                 parse_flags = paste0("read_error: ", conditionMessage(e)),
                 stringsAsFactors = FALSE)
    })
  })

  .merge_fill(results)
}
