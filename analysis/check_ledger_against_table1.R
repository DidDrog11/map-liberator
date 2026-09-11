# analysis/check_ledger_against_table1.R
# ------------------------------------------------------------------------------
# PURPOSE
#   Cross-check a hand-extracted ledger against the national totals printed
#   in Table 1 of each NCDC sitrep. For every source document in the ledger,
#   sum the current-week values entered across states and compare with the
#   current-week row the benchmark parser reads from page 1.
#
#   This reads page-1 text only. It never reads the state table, so it is a
#   check applied after extraction, not part of it (decision record 0011).
#
# USAGE (from the project root)
#   source("analysis/check_ledger_against_table1.R")
#   res <- check_ledger_against_table1("path/to/MapLiberator_Project.rds",
#                                      sitrep_dir = "data/sitreps")
#   res$mismatches
# ------------------------------------------------------------------------------

source("analysis/parse_ncdc_benchmark.R")

check_ledger_against_table1 <- function(project_file, sitrep_dir = "data/sitreps",
                                        variables = c("suspected", "confirmed", "probable", "deaths"),
                                        out_csv = NULL) {
  obj    <- readRDS(project_file)
  ledger <- if (is.data.frame(obj)) obj else obj$ledger
  ledger <- ledger[!is.na(ledger$Image_File) & ledger$Variable %in% variables, , drop = FALSE]

  # Ledger side: per document, sum of each variable across states, plus how
  # many states were entered and how many values were NA.
  ledger$num <- suppressWarnings(as.numeric(ledger$Value))
  docs <- split(ledger, ledger$Image_File)
  led <- do.call(rbind, lapply(names(docs), function(f) {
    d <- docs[[f]]
    sums <- vapply(variables, function(v) {
      x <- d$num[d$Variable == v]
      if (length(x) == 0 || all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
    }, numeric(1))
    # Table 1 counts states with confirmed cases, so compare like with like.
    conf <- d[d$Variable == "confirmed" & !is.na(d$num) & d$num > 0, , drop = FALSE]
    data.frame(file = f, states_entered = length(unique(conf$Region_ID)),
               na_values = sum(is.na(d$num)),
               led_suspected = sums[["suspected"]], led_confirmed = sums[["confirmed"]],
               led_probable = sums[["probable"]], led_deaths = sums[["deaths"]],
               stringsAsFactors = FALSE)
  }))

  # Page-1 side, from the text layer.
  paths <- file.path(sitrep_dir, led$file)
  bench <- parse_ncdc_corpus(paths[file.exists(paths)])
  res <- merge(led, bench[, c("file", "epi_week", "year", "wk_suspected", "wk_confirmed",
                              "wk_probable", "wk_deaths", "wk_states", "parse_flags",
                              "cum_suspected", "cum_confirmed", "cum_probable", "cum_deaths")],
               by = "file", all.x = TRUE)
  res$source <- ifelse(is.na(res$wk_confirmed), NA_character_, "text")

  # A "week" row larger than the cumulative row is the wrong row (layout
  # drift wrapped the current-week line). Drop it rather than report it.
  # Likewise when the text layer yielded only two of the three rows (the
  # prior-year flag is set): the current-week line is usually the one that
  # wrapped, and the two rows found are both cumulative. Such weeks go to OCR.
  bad_row <- !is.na(res$wk_confirmed) & (
    (!is.na(res$cum_confirmed) & res$wk_confirmed > res$cum_confirmed) |
    (grepl("table1_prior_year_row", res$parse_flags) & !grepl("early_layout", res$parse_flags)))
  for (v in c("wk_suspected", "wk_confirmed", "wk_probable", "wk_deaths",
              "cum_suspected", "cum_confirmed", "cum_probable", "cum_deaths")) res[[v]][bad_row] <- NA
  res$source[bad_row] <- NA_character_
  res$parse_flags[bad_row] <- paste0(res$parse_flags[bad_row], ";week_row_unreliable")

  # Fall back to OCR where the text layer gave nothing (2021 to mid 2022).
  # Two OCR routes: the current-week row itself, and the difference between
  # this week's and the previous week's cumulative rows. They are recorded
  # separately so a disagreement is visible rather than averaged away.
  res$year_wk  <- as.integer(sub(".*_(\\d{4})_w\\d+_.*", "\\1", res$file))
  res$file_wk  <- as.integer(sub(".*_w(\\d+)_.*", "\\1", res$file))
  res <- res[order(res$year_wk, res$file_wk), ]
  need <- which(is.na(res$wk_confirmed) & file.exists(file.path(sitrep_dir, res$file)))
  if (length(need) > 0 && requireNamespace("tesseract", quietly = TRUE)) {
    if (!exists("ocr_table1")) source("analysis/ocr_table1.R")
    for (v in variables) res[[paste0("cumdiff_", v)]] <- NA_real_
    res$ocr_agree <- NA
    prev_cum <- NULL; prev_wk <- NA
    for (i in seq_len(nrow(res))) {
      if (!(i %in% need)) {
        # A text-parsed week still supplies its cumulative row to the chain.
        prev_cum <- if (!is.na(res$cum_confirmed[i])) list(
          suspected = res$cum_suspected[i], confirmed = res$cum_confirmed[i],
          probable = res$cum_probable[i], deaths = res$cum_deaths[i]) else NULL
        prev_wk <- res$file_wk[i]
        next
      }
      o <- tryCatch(ocr_table1(file.path(sitrep_dir, res$file[i])), error = function(e) NULL)
      if (is.null(o)) { prev_cum <- NULL; prev_wk <- NA; next }
      wk  <- o$week      # labelled current-week row, NULL if OCR did not find one
      cum <- o$cum       # labelled cumulative row for this year, NULL if not found
      if (!is.null(wk)) {
        res$wk_suspected[i] <- wk$suspected; res$wk_confirmed[i] <- wk$confirmed
        res$wk_probable[i]  <- wk$probable;  res$wk_deaths[i]    <- wk$deaths
        res$wk_states[i]    <- if (length(o$states)) o$states[1] else NA_integer_
        res$ocr_agree[i]    <- o$agree
        res$source[i]       <- "ocr"
      }
      consecutive <- !is.na(prev_wk) && res$file_wk[i] == prev_wk + 1
      d <- if (consecutive) week_from_cumulative(cum, prev_cum) else NULL
      if (!is.null(d)) for (v in variables) res[[paste0("cumdiff_", v)]][i] <- d[[v]]
      if (!is.null(d) && is.null(wk)) {
        # No week row from OCR but the cumulative chain gives it: use that.
        for (v in variables) res[[paste0("wk_", v)]][i] <- d[[v]]
        res$source[i] <- "ocr_cumdiff"
      }
      prev_cum <- cum; prev_wk <- res$file_wk[i]
    }
  }

  for (v in variables) {
    res[[paste0("diff_", v)]] <- res[[paste0("led_", v)]] - res[[paste0("wk_", v)]]
  }
  res$diff_states <- res$states_entered - res$wk_states
  res <- res[order(res$year_wk, res$file_wk), ]

  diff_cols <- grep("^diff_", names(res), value = TRUE)
  res$any_mismatch <- rowSums(!is.na(res[, diff_cols]) & res[, diff_cols] != 0) > 0
  # For OCR weeks, a ledger that matches the cumulative-difference route is
  # a match even if the OCR week row disagrees (that row is the fragile one).
  if ("cumdiff_confirmed" %in% names(res)) {
    cd_ok <- rep(FALSE, nrow(res))
    has_cd <- !is.na(res$cumdiff_confirmed)
    if (any(has_cd)) {
      cd_ok[has_cd] <- rowSums(sapply(variables, function(v) {
        l <- res[[paste0("led_", v)]][has_cd]; c <- res[[paste0("cumdiff_", v)]][has_cd]
        !is.na(l) & !is.na(c) & l != c
      }) |> matrix(nrow = sum(has_cd))) == 0
    }
    res$matches_cumdiff <- ifelse(has_cd, cd_ok, NA)
    res$any_mismatch <- res$any_mismatch & !(res$matches_cumdiff %in% TRUE)
  }

  if (!is.null(out_csv)) {
    dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
    write.csv(res, out_csv, row.names = FALSE)
  }
  list(all = res, mismatches = res[res$any_mismatch | !is.na(res$parse_flags) & nzchar(res$parse_flags), ])
}
